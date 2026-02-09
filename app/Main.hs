{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Monad.Combinators.Expr as E 
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.List
import Data.Text (Text)
import Text.Megaparsec hiding (State, Pos, empty)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Data.Map.Strict (Map, (!?))
import Data.Void (Void)
import Data.Functor
import Data.Foldable
import Control.Monad.State
import Control.Monad.Except
import Lens.Micro.Platform hiding (at)
import Control.Monad.Identity
import Data.Char (isUpperCase)
import System.Process (callCommand)
import System.Exit (exitFailure)
import Debug.Trace (trace, traceShow)
import qualified Data.Text.Internal.Fusion.Size as T
import Control.Exception (assert)
import Data.List (List)

-- not sure if ISInvalid is even needed
data IndentationState = ISUnset Int | ISSet Int | ISInvalid deriving Show

type Parser = ParsecT Void Text (State IndentationState)

-- turn arithmetic into infix functions
data LimeOp =
    AssignType    -- ::
    | AssignValue -- =
    | FunArrow    -- ->
    | ForallArrow -- =>
    | Add         -- +
    | Sub         -- -
    | Mul         -- *
    | Div         -- /
    deriving Show

data LimeExpr =
    -- Parsed
    -- Basics/Literals
    Int Int
    | Bool Bool
    | Char Char
    | String Text
    | Symbol Text
    | List [LimeNode]
    -- Binary Op (Op Left Right)
    | Infix LimeOp LimeNode LimeNode
    | Prefix LimeOp LimeNode
    | FunCall LimeNode LimeNode
    | Lambda [LimeNode] LimeNode
    | Let LimeNode LimeNode LimeNode
    -- name tvars members
    | Data Text [Text] [(Text, [LimeNode])]
    -- value cases
    | Case LimeNode [(LimeNode, LimeNode)]
    | Class Text Text [LimeNode]
    | Instance Text Text [LimeNode]
    | Typedef Text [Text] LimeNode
    | Discard
    -- | DoBlock [LimeNode]
    deriving Show

type Pos = (SourcePos, SourcePos)

data LimeNode = LimeNode
    { expr :: LimeExpr
    , pos  :: Pos
    , info :: LimeType
    }
    deriving Show

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{--" "--}"

sc :: Parser ()
sc = L.space hspace1 lineComment blockComment

scng :: Parser ()
scng = L.space space1 lineComment blockComment

indentGuard :: Parser ()
indentGuard = get >>= \case
    ISUnset i -> do
        l <- unPos <$> L.indentLevel
        if l >= i
        then put $ ISSet l
        else L.incorrectIndent GT (mkPos i) (mkPos l)
    ISSet i -> do
        l <- unPos <$> L.indentLevel
        if l >= i
        then pure ()
        else L.incorrectIndent GT (mkPos i) (mkPos l)
    ISInvalid -> fancyFailure . Set.singleton $ ErrorFail "invalid indent"

scn :: Parser ()
scn = sc *> (void $ optional $ try (eol *> scng *> indentGuard))

scnno :: Parser ()
scnno = sc *> (void $ try (eol *> scng *> indentGuard))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: Text -> Parser Text
symbol = L.symbol scn

limeSymbol :: Parser LimeExpr
limeSymbol = do
    t <- T.pack <$> lexeme (some (letterChar <|> numberChar <|> char '_' <|> char '\''))
    pure $ if t == "_" then Discard else Symbol t

limeInt :: Parser LimeExpr
limeInt = Int . read @Int <$> lexeme (some digitChar)

limeString :: Parser LimeExpr
limeString = do
    _ <- char '"'
    s <- manyTill (L.charLiteral) (symbol "\"")
    pure $ String $ T.pack s

charEscapes :: Parser Char
charEscapes = char '\\' *> choice [char 'n' $> '\n']

limeChar :: Parser LimeExpr
limeChar = do
    _ <- char '\''
    l <- charEscapes <|> L.charLiteral
    _ <- lexeme (char '\'')
    pure $ Char l

limeLet :: Parser LimeExpr
limeLet = do
    _ <- symbol "let"
    l <- get
    left <- trace (show l) limeNode
    right <- indentBracket $ do
        _ <- symbol "="
        indentBracket limeNode
    _ <- symbol "in"
    expr <- limeNode
    pure $ Let left right expr

limeClass :: Parser LimeExpr
limeClass = do
    _ <- symbol "class"
    cname <- T.pack <$> lexeme (some letterChar)
    ctvar <- T.pack <$> lexeme (some letterChar)
    _ <- symbol "where"
    defs <- indentBracket $ do
        some limeDefAssign
    pure $ Class cname ctvar defs

limeInstance :: Parser LimeExpr
limeInstance = do
    _ <- symbol "instance"
    cname <- T.pack <$> lexeme (some letterChar)
    ct <- T.pack <$> lexeme (some letterChar)
    _ <- symbol "where"
    defs <- indentBracket $ do
        some limeDefAssign
    pure $ Instance cname ct defs

limeTypedef :: Parser LimeExpr
limeTypedef = do
    _ <- symbol "type"
    left <- T.pack <$> lexeme (some letterChar)
    tvs <- many (T.pack <$> lexeme (some letterChar))
    _ <- symbol "="
    r <- typeLevelNode
    pure $ Typedef left tvs r

limeData :: Parser LimeExpr
limeData = do
    _ <- symbol "data"
    left <- T.pack <$> lexeme (some letterChar)
    tvs <- many (T.pack <$> lexeme (some letterChar))
    _ <- symbol "="
    right <- ((,) <$> (T.pack <$> lexeme (some letterChar)) <*> (many (limeTermToNode limeSymbol <|> limeTypeLevelParen))) `sepBy` symbol "|"
    pure $ Data left tvs right

caseBlock :: Parser [(LimeNode, LimeNode)]
caseBlock = p `sepBy1` scnno
    where p = do
            il <- get
            l <- limeNode
            indentBracketNoScn $ do
                _ <- symbol "->"
                il <- get
                r <- limeNode
                il <- get
                pure $ (l, r)

limeCase :: Parser LimeExpr
limeCase = do
    _ <- symbol "case"
    -- v <- dbg "node" limeNode
    v <- limeNode
    indentBracket $ do
        l <- get
        _ <- symbol "of"
        b <- caseBlock
        pure $ Case v b

limeLambda :: Parser LimeExpr
limeLambda = do
    _ <- symbol "\\"
    args <- (limeTermToNode limeSymbol) `someTill` symbol "->"
    Lambda args <$> limeNode

defaultSourcePos :: (SourcePos, SourcePos)
defaultSourcePos = (initialPos "default", initialPos "default")

limeTermToNode :: Parser LimeExpr -> Parser LimeNode
limeTermToNode p = do
    start <- getSourcePos
    parserResult <- p
    end <- getSourcePos
    pure $ LimeNode parserResult (start, end) Unchecked

nodesToInfix :: LimeOp -> LimeNode -> LimeNode -> LimeNode
nodesToInfix op l@(LimeNode _ (start, _) _) r@(LimeNode _ (_, end) _) = LimeNode (Infix op l r) (start, end) Unchecked

limeParen :: Parser LimeNode
limeParen = between (symbol "(") (symbol ")") limeNode

limeExprToNode :: Parser (LimeNode -> LimeNode -> LimeExpr) -> Parser (LimeNode -> LimeNode -> LimeNode)
limeExprToNode p = do
    r <- p
    pure (\a@(LimeNode _ (start, _) _) b@(LimeNode _ (_, end) _) -> LimeNode (r a b) (start, end) Unchecked)

keyword :: Parser ()
keyword = void $ choice ["of", "in"]

limeTerm :: Parser LimeNode
limeTerm = do
    notFollowedBy keyword
    limeParen <|> (limeTermToNode $ choice [limeCase, limeInt, limeString, lexeme limeLambda, lexeme limeLet, limeChar, limeSymbol])

limeTopLevelNode :: Parser LimeNode
limeTopLevelNode = limeTermToNode $ choice [limeData, limeClass, limeInstance, limeTypedef]

indentBracketNoScn :: Parser a -> Parser a
indentBracketNoScn p = do
    o <- get
    modify (\case
        ISSet a -> ISUnset $ a + 1
        a -> a)
    res <- p
    put o
    -- case o of
    --     ISUnset _ -> put ISInvalid
    --     _ -> put o
    pure res
    -- case res of
    --     Left err -> parseError err
    --     Right v -> pure v

indentBracket :: Parser a -> Parser a
indentBracket p = indentBracketNoScn p <* scn

limeDefAssign :: Parser LimeNode
limeDefAssign = do
    l <- limeNode
    (p2, op) <- choice $ lexeme <$>
        [ (typeLevelNode, AssignType) <$ "::" 
        , (limeNode, AssignValue) <$ "=" ]
    r <- p2
    pure $ nodesToInfix op l r

limeTopLevel :: Parser LimeNode
limeTopLevel = L.nonIndented sc $ indentBracketNoScn (limeTopLevelNode <|> limeDefAssign)

limeTypeLevelParen :: Parser LimeNode
limeTypeLevelParen = between (symbol "(") (symbol ")") typeLevelNode

typeLevelNode :: Parser LimeNode
typeLevelNode = E.makeExprParser (limeTermToNode limeSymbol <|> limeTypeLevelParen) table
    where
        table = 
            [ [ binary' "" FunCall ]
            , [ binaryR "->" FunArrow ] 
            , [ binaryR "=>" ForallArrow ] 
            ]
        binary' s op = E.InfixL $ limeExprToNode (op <$ symbol s)
        binaryR s op = E.InfixR $ limeExprToNode (Infix op <$ symbol s)

limeNode :: Parser LimeNode
limeNode = E.makeExprParser limeTerm table
    where
        table = 
          [ [ binary' "" FunCall
            ]
          , [ binaryF "*" Mul
            , binaryF "/" Div
            ]
          , [ binaryF "+" Add
            , binaryF' "-" Sub
            ] 
          ]
        binary s op = E.InfixL $ limeExprToNode (Infix op <$ symbol s)
        -- parse ops like a + b as function calls
        binaryF s op = E.InfixL $ limeExprToNode (Infix op <$ symbol s)
        binaryF' s op = E.InfixL $ limeExprToNode (Infix op <$ try (symbol s <* notFollowedBy (symbol ">")))
        binary' s op = E.InfixL $ limeExprToNode (op <$ symbol s)

program :: Parser [LimeNode]
program = (many (lineComment <|> blockComment <|> void eol)) *> (limeTopLevel `sepEndBy` void (some $ lexeme eol)) <* eof

simplifyLambda :: [LimeNode] -> LimeNode -> Pos -> LimeNode
simplifyLambda [] r _ = r
simplifyLambda [a] r pos = LimeNode (Lambda [a] r) pos Unchecked
simplifyLambda (a:as) r pos =
    LimeNode (Lambda [a] $ simplifyLambda as r pos) pos Unchecked

newNode :: LimeExpr -> Pos -> LimeNode
newNode e p = LimeNode e p Unchecked

simplifyPatternMatch :: LimeNode -> LimeNode -> (LimeNode, LimeNode)
simplifyPatternMatch lhsn@(LimeNode lhs _ _) rhs@(LimeNode _ rpos _) = case lhs of
    FunCall l@(LimeNode (Symbol _) _ _) r@(LimeNode (Symbol _) _ _) -> (l, newNode (Lambda [r] rhs) rpos)
    FunCall l r@(LimeNode (Symbol _) _ _) -> simplifyPatternMatch l $ newNode (Lambda [r] rhs) rpos
    Symbol _ -> (lhsn, rhs)
    _ -> error "pattern match case not handled in simplification pass"

infixToFun :: Text -> LimeNode -> LimeNode -> Pos -> LimeNode
infixToFun name l r pos = newNode (FunCall (newNode (FunCall (newNode (Symbol name) pos) $ simplifyLime l) pos) $ simplifyLime r) pos

-- for whenever i decide to change function calls to be uncurried
-- simplifyFunCall :: LimeNode -> [LimeNode] -> (LimeNode, [LimeNode])
-- simplifyFunCall f@(LimeNode node _ _) as = case node of
--     FunCall f' [a'] -> simplifyFunCall f' ((simplifyLime a'):as)
--     _ -> (simplifyLime f, as)


simplifyLime :: LimeNode -> LimeNode
simplifyLime n@(LimeNode node pos _) = case node of
    Infix AssignValue l r -> 
        n { expr = Infix AssignValue l' r' }
        where (l', r') = simplifyPatternMatch l $ simplifyLime r
    Infix Add l r ->
        infixToFun "__add" l r pos
    Infix Sub l r ->
        infixToFun "__sub" l r pos
    Infix Div l r ->
        infixToFun "__div" l r pos
    Infix Mul l r ->
        infixToFun "__mul" l r pos
    Infix op l r ->
        n { expr = Infix op (simplifyLime l) (simplifyLime r) }
    Lambda a r ->
        simplifyLambda a r pos
    FunCall f a ->
        n { expr = FunCall (simplifyLime f) (simplifyLime a) }
    Int _ -> n
    Char _ -> n
    Symbol _ -> n
    Bool _ -> n
    String _ -> n
    List _ -> n
    Prefix op r ->
        n { expr = Prefix op $ simplifyLime r }
    Let l r e ->
        n { expr = Let l' r' (simplifyLime e) }
        where (l', r') = simplifyPatternMatch l $ simplifyLime r
    Data _ _ _ -> n
    Case v cs ->
        n { expr = Case (simplifyLime v) ((\(a,b) -> (a, simplifyLime b)) <$> cs) }
    Class name tv ds ->
        n { expr = Class name tv (map simplifyLime ds) }
    Instance name tv ds ->
        n { expr = Instance name tv (map simplifyLime ds) }
    Typedef l tvs r -> n
    Discard -> n

printNode :: LimeNode -> Text
printNode n@(LimeNode node _ _) = case node of
    Infix op l r -> printNode l <> " " <> top <> " " <> printNode r
        where top = case op of
                AssignType  -> "::"
                AssignValue -> "="
                FunArrow    -> "->"
                Add         -> "+"
                Sub         -> "-"
                Mul         -> "*"
                Div         -> "/"
    Lambda args r ->
        "\\" <> foldl' (\b a -> b <> printNode a <> " ") "" args <> "-> " <> printNode r
    FunCall f a ->
        printNode f <> " " <> printNode a
    Int v -> T.pack $ show v
    Char v -> T.pack $ show v
    Symbol s -> s
    Bool b -> T.pack $ show b
    String s -> s
    List s -> "[" <> foldl' (\b a -> b <> printNode a <> " ") "" s <> "]"
    Data n tvs ns -> "data " <> n <> (if null tvs then "" else " " <> T.intercalate " " tvs) <> " = " <> T.intercalate " | " (map (\(a, b) -> a <> " " <> (T.intercalate " " (map printNode b))) ns)
    Case v cs -> "case " <> printNode v <> " of\n    " <> (T.intercalate "\n    " $ map (\(l, r) -> printNode l <> " -> " <> printNode r) cs)
    Let l r e ->
        "let " <> printNode l <> " = " <> printNode r <> " in " <> printNode e
    Class n tv ds ->
        "class " <> n <> " " <> tv <> " where\n    "
            <> (T.intercalate "\n    " (map printNode ds))
    Instance n t ds ->
        "instance " <> n <> " " <> t <> " where\n    "
            <> (T.intercalate "\n    " (map printNode ds))
    Typedef l tvs r ->
        "type " <> l <> (if null tvs then "" else " " <> T.intercalate " " tvs) <> " = " <> printNode r
    Prefix op r ->
        ""
    Discard -> "_"

data LimePrim
    -- size, signedness, name
    = PInt Int Bool
    -- size, name
    | PFloat Int
    | PString
    | PUnit
    | PWorld
    deriving (Show, Eq, Ord)

data LimeType =
    -- arg ret
    TLambda LimeType LimeType
    -- type vars
    -- do i need a type-level tvar?
    -- var, constraints
    | TVar Int (Set.Set Text)
    | TPrim LimePrim
    -- name, fields, applied types
    | TADT Text [(Text, [LimeType])] [LimeType]
    -- recursive ADT (name, applied types)
    | TRec Text [LimeType]
    | TNamed Text [LimeType] LimeType
    -- no assigned type
    | Unchecked
    deriving (Show, Eq, Ord)

data CheckedNode = CheckedNode LimeNode LimeType
    deriving Show

data Scheme = Forall [(Int, Set.Set Text)] LimeType
    deriving Show

type TypeEnv = (Map Text Scheme, Map Text Scheme)
type Subst = Map Int LimeType

data TypecheckerState = TypecheckerState
    { _curTVar :: Int
    , _classes :: Map Text (Map Text LimeType)   
    , _instances :: Map Text (Set.Set Text)
    , _implementations :: Map (Text, LimeType) LimeNode
    }
    deriving Show

makeLenses ''TypecheckerState

data TypecheckError 
    = TEInfiniteType
    | TEMismatch LimeType LimeType
    | TEVarNotFound Text
    | TETypeNotFound Text
    | TEUnsupportedExpr LimeNode
    | TEUnsupportedPMExpr LimeNode
    | TEUnsupportedTLExpr LimeNode
    | TENonFN
    deriving Show

type Typechecker a = ExceptT (Pos, TypecheckError) (State TypecheckerState) a

printType :: LimeType -> Text
printType = \case
    TLambda a r -> 
        case a of
            TLambda _ _ -> "(" <> printType a <> ") -> " <> printType r
            _ -> printType a <> " -> " <> printType r
    TPrim p -> case p of
        PInt _ _ -> "Int" 
        PFloat _ -> "Float"
        PString -> "String"
        PUnit -> "Unit"
        PWorld -> "World"
    TVar i cs -> (if Set.null cs then "" else "(" <> (T.intercalate ", " $ Set.elems cs) <> " ") <> "p" <> T.pack (show i) <> (if Set.null cs then "" else ")")
    TADT n ms ats -> n <> T.concat (map (\at -> " " <> printType at) ats) -- <>  " (" <> T.intercalate " | " (map (\(n', ts) -> n' <> if null ts then "" else " " <> T.intercalate " " (map (\t -> "(" <> printType t <> ")") ts)) ms) <> ")"
    TRec t ats -> t <> T.concat (map (\at -> " " <> printType at) ats)
    TNamed n tvs _ -> n <> (if null tvs then "" else " " <> T.intercalate " " (map printType tvs))
    Unchecked -> "Unchecked"

instantiate :: Scheme -> Typechecker LimeType
instantiate (Forall as t) = do
    as' <- mapM (freshTVarWithConstraint . snd) as
    let s = Map.fromList $ zip (map fst as) as'
    pure $ apply s t

findVarEnv :: TypeEnv -> Text -> Typechecker (Maybe LimeType)
findVarEnv (_, venv) n = case venv !? n of
    Just a -> instantiate a >>= \t -> pure $ Just t
    Nothing -> pure Nothing

findVarError :: TypeEnv -> Text -> Pos -> Typechecker LimeType
findVarError env n pos = findVarEnv env n >>= \case
    Just a -> pure a
    Nothing -> throwError (pos, trace (T.unpack $ printTypeEnv $ snd env) $ TEVarNotFound n)

findTypeEnv :: TypeEnv -> Text -> Typechecker (Maybe LimeType)
findTypeEnv (tenv, _) n = case tenv !? n of
    Just a -> instantiate a >>= \t -> pure $ Just t
    Nothing -> pure Nothing

findTypeError :: TypeEnv -> Text -> Pos -> Typechecker LimeType
findTypeError env n pos = findTypeEnv env n >>= \case
    Just a -> pure a
    Nothing -> throwError (pos, TETypeNotFound n)

typeLevelEval :: TypeEnv -> LimeNode -> Typechecker Scheme
typeLevelEval env n = snd <$> typeLevelEvalI env n

typeLevelEvalI :: TypeEnv -> LimeNode -> Typechecker (TypeEnv, Scheme)
typeLevelEvalI env@(tenv, _) n@(LimeNode node pos _) = case node of
    Symbol s -> case tenv !? s of
            Just a -> pure (env, a)
            Nothing -> if isUpperCase $ T.head s 
                then throwError (pos, TETypeNotFound s)
                -- keep track of created tvars in env
                else freshTVar >>= \tv@(TVar v c) -> pure ((Map.insert s (Forall [] tv) tenv, snd env), Forall [(v, c)] tv)
    Infix FunArrow l r -> do
        (env', Forall lvs lt) <- typeLevelEvalI env l
        (env'', Forall rvs rt) <- typeLevelEvalI env' r
        pure (env'', Forall (lvs <> rvs) $ TLambda lt rt)
    FunCall f a -> do
        (_, f') <- typeLevelEvalI env f
        let Forall (ftv:ftvs) f'' = f'
        (env', Forall ftvs2 a') <- typeLevelEvalI env a
        b <- bind pos (fst ftv) a'
        pure (env', Forall (ftvs <> ftvs2) $ apply b f'')
    _ -> throwError (pos, TEUnsupportedTLExpr n)

freshTVarWithConstraint :: Set.Set Text -> Typechecker LimeType
freshTVarWithConstraint c = do
    n <- use curTVar
    curTVar %= (+1)
    pure $ TVar n c

freshTVar :: Typechecker LimeType
freshTVar = freshTVarWithConstraint Set.empty

freshTVarI :: Typechecker Int
freshTVarI = do
    n <- use curTVar
    curTVar %= (+1)
    pure n

schemeType :: Scheme -> LimeType
schemeType (Forall _ t) = t

getType :: LimeNode -> LimeType
getType (LimeNode _ _ t) = t

recApply :: Subst -> LimeType -> LimeType
recApply s t@(TVar v c) = case s !? v of
    Just x -> recApply s x
    Nothing -> t
recApply s (TNamed n tvs t) = TNamed n (map (recApply s) tvs) (recApply s t)
recApply s (TLambda a r) = TLambda (recApply s a) (recApply s r)
recApply _ t = t


apply :: Subst -> LimeType -> LimeType
apply s t@(TVar v c) = case s !? v of
    Just x -> recApply s x
    Nothing -> t
apply s (TLambda a r) = TLambda (apply s a) (apply s r)
apply s (TADT t ms ats) = TADT t (map (\(a, ts) -> (a, map (apply s) ts)) ms) $ map (apply s) ats
apply s (TRec t ats) = TRec t $ map (apply s) ats
apply s (TNamed n tvs t) = TNamed n (map (apply s) tvs) (apply s t)
apply _ t = t

applyS :: Subst -> Scheme -> Scheme
applyS s (Forall as t) = Forall as $ apply s' t
    where s' = foldr Map.delete s (map fst as)

applyEnv :: Subst -> TypeEnv -> TypeEnv
applyEnv s (tenv, venv) = (tenv, Map.map (applyS s) venv)

applyNodes :: Subst -> LimeNode -> LimeNode
applyNodes s n = let n' = (applyNodesInternal s n) in n' { info = apply s $ info n' } 

applyNodesInternal :: Subst -> LimeNode -> LimeNode
applyNodesInternal s n@(LimeNode node pos _) = case node of
    Infix AssignValue l r -> 
        n { expr = Infix AssignValue l (applyNodes s r) }
    Infix op l r ->
        n { expr = Infix op (applyNodes s l) (applyNodes s r) }
    Lambda a r ->
        n { expr = Lambda (applyNodes s <$> a) (applyNodes s r) }
    FunCall f a ->
        n { expr = FunCall (applyNodes s f) (applyNodes s a) }
    Int _ -> n
    Char _ -> n
    Symbol _ -> n
    Bool _ -> n
    String _ -> n
    List _ -> n
    Prefix op r ->
        n { expr = Prefix op $ applyNodes s r }
    Let l r e ->
        n { expr = Let l (applyNodes s r) (applyNodes s e) }
    Data _ _ _ -> n
    Case v cs ->
        n { expr = Case (applyNodes s v) ((\(a,b) -> (applyNodes s a, applyNodes s b)) <$> cs) }
    Class name tv ds ->
        n { expr = Class name tv (map (applyNodes s) ds) }
    Instance name tv ds ->
        n { expr = Instance name tv (map (applyNodes s) ds) }
    Typedef l tvs r -> n
    Discard -> n

unifyList :: [LimeType] -> [LimeType] -> Typechecker Subst
unifyList as bs = foldl' helper (pure Map.empty) $ zip as bs
    where helper b (ta,tb) = do
            s1 <- b
            s2 <- unifyInternal (apply s1 ta) (apply s1 tb)
            pure $ Map.union s2 s1

unify :: Pos -> LimeType -> LimeType -> Typechecker Subst
unify pos t1 t2 = withError ((pos,) . snd) (unifyInternal t1 t2)
-- unify pos t1 t2 = withError (const (pos, TEMismatch t1 t2)) (unifyInternal t1 t2)

unifyInternal :: LimeType -> LimeType -> Typechecker Subst
-- combine constraints between 2 tvars
unifyInternal (TVar a ac) (TVar b bc) = bind defaultSourcePos a (TVar b (Set.union ac bc))
-- this rule needs to be at the top, so that names are preserved
unifyInternal (TVar a c) t = bind defaultSourcePos a t
unifyInternal t (TVar a c) = bind defaultSourcePos a t
unifyInternal (TNamed _ _ a) (TNamed _ _ b) = unifyInternal a b
unifyInternal (TNamed _ _ a) b = unifyInternal a b
unifyInternal a (TNamed _ _ b) = unifyInternal a b
unifyInternal (TLambda l1 r1) (TLambda l2 r2) = do
    s1 <- unifyInternal l1 l2
    s2 <- unifyInternal (apply s1 r1) (apply s1 r2)
    pure $ Map.union s2 s1
-- TODO see whether there's limits to using nominalism beyond type application
unifyInternal (TRec a ats) (TRec b bts) | a == b = unifyList ats bts
unifyInternal (TRec a ats) (TADT b _ bts) | a == b = unifyList ats bts
unifyInternal (TADT a _ ats) (TRec b bts) | a == b = unifyList ats bts
unifyInternal (TADT a _ ats) (TADT b _ bts) | a == b = unifyList ats bts
unifyInternal (TPrim a) (TPrim b) | a == b = pure Map.empty
unifyInternal t1 t2 = throwError (defaultSourcePos, TEMismatch t1 t2)

occurs :: Int -> LimeType -> Bool
occurs v = \case
    TLambda t1 t2 -> occurs v t1 && occurs v t2
    TVar v' _ -> v == v'
    TPrim _ -> False
    -- TODO fix occurs check for ADT, TRec, TFun and TApp
    TRec _ _ -> False
    TADT _ _ _ -> False
    TNamed _ _ t -> occurs v t
    Unchecked -> False

bind :: Pos -> Int -> LimeType -> Typechecker Subst
bind pos a t
    | case t of 
            TVar ti _ -> a == ti
            _ -> False 
        = return Map.empty
    | occurs a t  = throwError (pos, TEInfiniteType)
    | otherwise   = pure $ Map.singleton a t

replaceType :: LimeNode -> LimeType -> LimeNode
replaceType n t = n { info = t }

applyN :: Subst -> LimeNode -> LimeNode
applyN s n@(LimeNode _ _ t) = n { info = apply s t }

freeTypeVars :: LimeType -> Set.Set (Int, Set.Set Text)
freeTypeVars = \case
    TLambda a r -> freeTypeVars a `Set.union` freeTypeVars r
    TVar a cs -> Set.singleton (a, cs)
    TPrim _ -> Set.empty
    -- TODO fix tvar check for ADTS once generic
    TRec _ ats -> Set.unions $ map freeTypeVars ats
    TADT _ ms ats -> Set.union (Set.unions $ concatMap (map freeTypeVars . snd) ms) (Set.unions $ map freeTypeVars ats)
    TNamed _ _ t -> freeTypeVars t
    Unchecked -> Set.empty

freeTypeVarsS :: Scheme -> Set.Set (Int, Set.Set Text)
freeTypeVarsS (Forall vs t) = freeTypeVars t `Set.difference` Set.fromList vs

-- might cause trouble later, will use this with tenv aswell in case it does
freeTypeVarsEnv :: TypeEnv -> Set.Set (Int, Set.Set Text)
freeTypeVarsEnv env = foldl' (flip $ Set.union . freeTypeVarsS) Set.empty $ Map.elems $ snd env

generalize :: TypeEnv -> LimeType -> Scheme
generalize env t = Forall as t
    where as = Set.toList $ freeTypeVars t `Set.difference` freeTypeVarsEnv env

data Constraint
    = CUnified Pos LimeType LimeType
    deriving Show

resolveConstraints :: [Constraint] -> Typechecker Subst
resolveConstraints cs = foldl' resolveConstraint (pure Map.empty) cs
    where resolveConstraint :: Typechecker Subst -> Constraint -> Typechecker Subst
          resolveConstraint s0 c = case c of
            CUnified pos t1 t2 -> s0 >>= \s1 -> Map.union s1 <$> unify pos (apply s1 t1) (apply s1 t2)

typecheckPatternMatch :: TypeEnv -> LimeNode -> Typechecker (TypeEnv, LimeNode)
typecheckPatternMatch env n@(LimeNode node npos _) = case node of
    Int _ -> pure $ (env, n { info = defInt })
    Symbol s -> 
        findVarError env s npos <&> 
            \v -> (env, n { info = v })
    FunCall f an@(LimeNode a _ _) -> do
        (env'@(tenv, venv), f') <- typecheckPatternMatch env f 
        case info f' of
            TLambda arg ret ->
                case a of
                    Symbol s ->
                        pure ((tenv, Map.insert s (Forall [] arg) venv), n { expr = FunCall f' an, info = ret })
                    Discard ->
                        pure (env', n { expr = FunCall f' an, info = ret })
                    _ -> throwError (npos, TEUnsupportedPMExpr n)
            _ -> throwError (npos, TENonFN)
            -- Nothing -> pure $ (tenv, Map.insert s t' venv)
    _ -> throwError (npos, TEUnsupportedPMExpr n)


typecheck :: TypeEnv -> LimeNode -> Typechecker (Subst, LimeNode)
typecheck env n@(LimeNode node npos _) = case node of
    Symbol s -> do
        (\v -> (Map.empty, n { info = v })) <$> findVarError env s npos
    Lambda [l@(LimeNode (Symbol a) _ _)] r@(LimeNode _ _ _) -> do
        tv <- freshTVar

        let env' = (fst env, Map.insert a (Forall [] tv) $ snd env)
        (s1, r'@(LimeNode _ _ t1)) <- typecheck env' r

        pure (s1, n { expr = Lambda [l { info = (apply s1 tv) }] r', info = apply s1 (TLambda tv t1) })
    FunCall f a -> do
        tv <- freshTVar
        (s1, f') <- typecheck env f
        (s2, a') <- typecheck (applyEnv s1 env) a
        s3 <- unify npos (apply s2 $ getType f') (TLambda (getType a') tv)
        -- applying s3 so the monomorphizer doesn't mess up
        let a'' = a' { info = apply s3 $ info a' }
        let f'' = f' { info = apply s3 $ info f' }
        -- pure (Map.unions [trace (show s3 <> " " <> show f') s3, s2, s1], n { expr = FunCall f' a', info = apply s3 tv })
        pure (Map.unions [s3, s2, s1], n { expr = FunCall f'' a'', info = apply s3 tv })
    Case v cs -> do
        (s1, v') <- typecheck env v 
        -- unify left side with v1 after applying s1
        -- unify right side with the other right sides
        -- correct that later on when i have more complex case statements
        let env' = applyEnv s1 env

        let check b (l, r) = do
                ((so, env), cs') <- b
                (env', l') <- typecheckPatternMatch env l
                (s2, r') <- typecheck env' r
                let l'' = l' { info = apply s2 $ info l' }
                s3 <- unify (pos l) (info v') (info l'')
                sn <- case cs' of
                        (c:_) -> do
                            s4 <- unify (pos r) (info $ snd c) (info r')
                            -- dunno if i even need this but i'm keeping it 4 now
                            s5 <- unify (pos l) (info $ fst c) (info l'')
                            pure $ Map.unions [s1, s2, s3, s4, s5]
                        [] -> pure $ Map.unions [s1, s2, s3]
                let env'' = applyEnv sn env'
                pure ((Map.union so sn, env''), (l'', r'):cs')
        -- TODO don't reverse the list
        ((s2, env''), cs') <- foldl' check (pure ((s1, env'), [])) cs
        -- this is crucial so that the type of v' can be determined
        let v'' = v' { info = apply s2 $ info v' }
        pure $ (s2, n { expr = Case v'' cs', info = info $ snd $ head cs'})
    Let l@(LimeNode (Symbol a) _ _) r e -> do
        -- TODO change typecheckPatternMatch so that using just a symbol works
        -- (env', l') <- typecheckPatternMatch env l
        (s1, r') <- typecheck env r
        let t'    = generalize env' $ getType r'
            (tenv, venv)  = (applyEnv s1 env)
            env' = (tenv, Map.insert a t' venv) 
        (s2, e') <- typecheck env' e

        pure $ (Map.union s1 s2, n { expr = Let (l { info = info r' }) r' e', info = info e' })
    Int _ -> pure $ (Map.empty, n { info = defInt })
    Char _ -> pure $ (Map.empty, n { info = defChar })
    String _ -> pure $ (Map.empty, n { info = defString })

    -- catch all so the darn language server doesn't complain
    _ -> throwError (npos, TEUnsupportedExpr n)

topLevelTypecheck :: TypeEnv -> LimeNode -> Typechecker (TypeEnv, LimeNode)
topLevelTypecheck env@(tenv, venv) n@(LimeNode expr pos _) = case expr of
    Infix AssignType l@(LimeNode (Symbol s) _ _) r -> do
        rt <- typeLevelEval env r
        pure $ ((tenv, Map.insert s rt venv), n)
    Infix AssignValue a@(LimeNode (Symbol s) _ _) b -> do
        tv <- freshTVar
        (s0, b') <- typecheck env b
        s1 <- Map.union s0 <$> case venv !? s of
                Just x -> unify pos (schemeType x) (info b')
                Nothing -> pure Map.empty

        let b''   = trace (show s1) $ applyNodes s1 b'
            env'  = applyEnv s1 env
            t'    = generalize env' $ getType b''
            env'' = case venv !? s of
                Just _ -> env
                Nothing -> (fst env', Map.insert s t' $ snd env')

        pure $ (env'', n { expr = Infix AssignValue a b'', info = getType b'' })
        -- pure $ (tenv, Map.insert s b' venv)
    Typedef s tvns r -> do
        tvsi <- traverse (const freshTVarI) tvns
        let tvs = map (\tvi -> TVar tvi Set.empty) tvsi
        let env' = foldl' (\m (n, tv@(TVar i _)) -> (Map.insert n (Forall [] tv) $ fst m, snd m)) env $ zip tvns tvs
        -- TODO forbid inline TV def
        (Forall tvs2 rt) <- typeLevelEval env' r
        pure $ ((Map.insert s (Forall ((, Set.empty) <$> tvsi) $ TNamed s tvs rt) tenv, venv), n)
    Data name tvs ms -> do
        tvs' <- traverse (const $ freshTVarI <&> (, Set.empty)) tvs
        let tvs'' = map (\(tvi, _) -> TVar tvi Set.empty) tvs'
            tempEnv = foldl' insertTVs (Map.insert name (Forall tvs' (TRec name tvs'')) tenv, venv) $ zip tvs tvs''
            insertTVs (tenv, venv) (n,v) = (Map.insert n (Forall [] v) tenv, venv)
        ms' <- mapM (\(a, b) -> mapM (\x -> schemeType <$> typeLevelEval tempEnv x) b >>= pure . (a,)) ms
        let t = TADT name ms' tvs''
            env' = (Map.insert name (Forall tvs' t) tenv, venv)
            env'' = foldl' (\b (a, cs) -> (fst b, Map.insert a (Forall tvs' (foldl' (\t ti -> TLambda ti t) t $ reverse cs)) $ snd b)) env' ms'
        pure $ (env'', n { info = t })
    Class name tvn ds -> do
        tvi <- freshTVarI
        let tv = TVar tvi $ Set.singleton name
        let env' = (Map.insert tvn (Forall [(tvi, Set.singleton name)] tv) $ fst env, snd env)
        let check b (dn@(LimeNode d _ _)) = do
                case d of
                    Infix AssignType l@(LimeNode (Symbol s) _ _) r -> do
                        ds' <- b
                        r' <- typeLevelEval env' r
                        pure (Map.insert s r' $ fst ds', dn { info = schemeType r' }:(snd ds'))
                    _ -> b
        -- TODO don't reverse the list
        ds' <- foldl' check (pure (Map.empty, [])) ds
        classes %= Map.insert name (Map.map schemeType (fst ds'))
        pure $ ((fst env, Map.union (fst ds') $ snd env), n { expr = Class name tvn $ snd ds' })
    Instance name t ds -> do
        let collectImplementations b (dn@(LimeNode d _ _)) = do
                case d of
                    Infix AssignValue l@(LimeNode (Symbol s) _ _) r -> do
                        (env', is, ds') <- b
                        (s1, r') <- typecheck env' r
                        -- TODO unify with original type after applying tvar 0
                        -- instead of info r' insert type of TVar 0 (somehow)
                        pure (applyEnv s1 env', Map.insert (s, trace (T.unpack $ printType $ info r') $ info r') r' is, r':ds')
                    _ -> b
        (env', is, ds') <- foldl' collectImplementations (pure (env, Map.empty, [])) ds

        implementations %= Map.union is

        -- TODO check all members of the typeclass have been implemented
        instances %= Map.alter (Just . \case
            Just s -> Set.insert t s
            Nothing -> Set.singleton t) name
        pure (env', n { expr = Instance name t ds' })
    _ -> (\(_, b) -> (env, b)) <$> typecheck env n

-- typecheck :: TypeEnv -> LimeNode -> Typechecker ([Constraint], LimeNode)
-- typecheck env n@(LimeNode node npos _) = case node of
--     Symbol s -> do
--         (\v -> ([], n { info = v })) <$> findVarError env s npos
--
--     Lambda [l@(LimeNode (Symbol a) _ _)] r@(LimeNode _ _ _) -> do
--         tv <- freshTVar
--         let env' = (fst env, Map.insert a (Forall [] tv) $ snd env)
--         (s1, r'@(LimeNode _ _ t1)) <- typecheck env' r
--         pure (s1, n { expr = Lambda [l { info = tv }] r', info = TLambda tv t1 })
--
--     FunCall f a -> do
--         tv <- freshTVar
--         (s1, f') <- typecheck env f
--         (s2, a') <- typecheck env a
--         let s3 = [CUnified npos (getType f') (TLambda (getType a') tv)]
--         pure (s1 <> s2 <> s3, n { expr = FunCall f' a', info = tv })
--
--     Case v cs -> do
--         (s1, v') <- typecheck env v 
--         -- unify left side with v1
--         -- unify right side with the other right sides
--         -- correct that later on when i have more complex case statements
--         let check (l, r) b = do
--                 (so, cs') <- b
--                 (env', l') <- typecheckPatternMatch env l
--                 (s2, r') <- typecheck env' r
--                 let s3 = s1 <> s2 <> 
--                         [CUnified (pos l) (info v') (info l')] <> 
--                         case cs' of
--                             (c:_) -> [ CUnified (pos r) (info $ snd c) (info r')
--                                      -- dunno if i even need this but i'm keeping it 4 now
--                                      , CUnified (pos l) (info $ fst c) (info l') ]
--                             [] -> []
--                 pure (so <> s3, (l', r'):cs')
--         -- TODO don't reverse the list
--         (s2, cs') <- foldr check (pure (s1, [])) cs
--         pure $ (s2, n { expr = Case v' cs', info = info $ snd $ head cs'})
--     Let l@(LimeNode (Symbol a) _ _) r e -> do
--         -- TODO change typecheckPatternMatch so that using just a symbol works
--         -- (env', l') <- typecheckPatternMatch env l
--         (s1, r') <- typecheck env r
--         let t'    = generalize env' $ getType r'
--             (tenv, venv)  = env
--             env' = (tenv, Map.insert a t' venv) 
--         (s2, e') <- typecheck env' e
--
--         pure $ (s1 <> s2, n { expr = Let (l { info = info r' }) r' e', info = info e' })
--     Int _ -> pure $ ([], n { info = defInt })
--     Char _ -> pure $ ([], n { info = defChar })
--     -- catch all so the darn language server doesn't complain
--     _ -> throwError (npos, TEUnsupportedExpr n)
--
-- topLevelTypecheck :: TypeEnv -> LimeNode -> Typechecker (TypeEnv, LimeNode)
-- topLevelTypecheck env@(tenv, venv) n@(LimeNode expr pos _) = case expr of
--     Infix AssignType l@(LimeNode (Symbol s) _ _) r -> do
--         rt <- typeLevelEval env r
--         pure $ ((tenv, Map.insert s rt venv), n)
--
--     Infix AssignValue a@(LimeNode (Symbol s) _ _) b -> do
--         tv <- freshTVar
--         (bcs, b') <- trace (show tv) $ typecheck env b
--         let bcs' = bcs <> case venv !? s of
--                 Just x -> [CUnified pos (schemeType x) (info b')]
--                 Nothing -> []
--
--         s0 <- resolveConstraints bcs
--         let b''   = applyNodes s0 b'
--             env'  = applyEnv s0 env
--             t'    = generalize env' $ getType b''
--
--         pure $ ((fst env', Map.insert s t' $ snd env'), n { expr = Infix AssignValue a b'', info = getType b'' })
--         -- pure $ (tenv, Map.insert s b' venv)
--
--     Typedef s tvns r -> do
--         tvsi <- traverse (const freshTVarI) tvns
--         let tvs = map (\tvi -> TVar tvi Set.empty) tvsi
--         let env' = foldl' (\m (n, tv@(TVar i _)) -> (Map.insert n (Forall [] tv) $ fst m, snd m)) env $ zip tvns tvs
--         -- TODO forbid inline TV def
--         (Forall tvs2 rt) <- typeLevelEval env' r
--         pure $ ((Map.insert s (Forall ((, Set.empty) <$> tvsi) $ TNamed s tvs rt) tenv, venv), n)
--
--     Data name tvs ms -> do
--         tvs' <- traverse (const $ freshTVarI <&> (, Set.empty)) tvs
--         let tvs'' = map (\(tvi, _) -> TVar tvi Set.empty) tvs'
--             tempEnv = foldl' insertTVs (Map.insert name (Forall tvs' (TRec name tvs'')) tenv, venv) $ zip tvs tvs''
--             insertTVs (tenv, venv) (n,v) = (Map.insert n (Forall [] v) tenv, venv)
--         ms' <- mapM (\(a, b) -> mapM (\x -> schemeType <$> typeLevelEval tempEnv x) b >>= pure . (a,)) ms
--         let t = TADT name ms' tvs''
--             env' = (Map.insert name (Forall tvs' t) tenv, venv)
--             env'' = foldl' (\b (a, cs) -> (fst b, Map.insert a (Forall tvs' (foldl' (\t ti -> TLambda ti t) t $ reverse cs)) $ snd b)) env' ms'
--
--         pure $ (env'', n { info = t })
--
--     Class name tvn ds -> do
--         tvi <- freshTVarI
--         let tv = TVar tvi $ Set.singleton name
--         let env' = (Map.insert tvn (Forall [(tvi, Set.singleton name)] tv) $ fst env, snd env)
--         let check (dn@(LimeNode d _ _)) b = do
--                 case d of
--                     Infix AssignType l@(LimeNode (Symbol s) _ _) r -> do
--                         ds' <- b
--                         r' <- typeLevelEval env' r
--                         pure (Map.insert s r' $ fst ds', dn { info = schemeType r' }:(snd ds'))
--                     _ -> b
--         ds' <- foldr check (pure (Map.empty, [])) ds
--         classes %= Map.insert name (Map.map schemeType (fst ds'))
--
--
--         pure $ ((fst env, Map.union (fst ds') $ snd env), n { expr = Class name tvn $ snd ds' })
--     Instance name t ds -> do
--         let collectImplementations b (dn@(LimeNode d _ _)) = do
--                 case d of
--                     Infix AssignValue l@(LimeNode (Symbol s) _ _) r -> do
--                         (env', is, ds') <- b
--                         -- TODO do i need these to be interdependent
--                         (rcs, r') <- typecheck env' r
--
--                         -- TODO unify with original type
--                         s0 <- resolveConstraints rcs
--                         let r''   = applyNodes s0 r'
--                         -- TODO unify with original type after applying tvar 0
--                         -- instead of info r' insert type of TVar 0 (somehow)
--                         pure (applyEnv s0 env', Map.insert (s, trace (T.unpack $ printType $ info r'') $ info r'') r'' is, r'':ds')
--                     _ -> b
--         (env', is, ds') <- foldl' collectImplementations (pure (env, Map.empty, [])) ds
--
--         implementations %= Map.union is
--
--         -- TODO check all members of the typeclass have been implemented
--         instances %= Map.alter (Just . \case
--             Just s -> Set.insert t s
--             Nothing -> Set.singleton t) name
--
--
--         pure (env', n { expr = Instance name t ds' })
--     -- TODO fix
--     _ -> (\(_, b) -> (env, b)) <$> typecheck env n
--     -- _ -> typecheck env n $> (env, n)

infixType :: LimeType -> LimeType
infixType t = TLambda t $ TLambda t t

defInt :: LimeType
defInt = TNamed "Int" [] $ TPrim $ PInt 0 True

defChar :: LimeType
defChar = TNamed "Char" [] $ TPrim $ PInt 1 True

defFloat :: LimeType
defFloat = TNamed "Float" [] $ TPrim $ PFloat 4

defString :: LimeType
defString = TNamed "String" [] $ TPrim $ PString

defUnit :: LimeType
defUnit = TPrim $ PUnit

defWorld :: LimeType
defWorld = TPrim $ PWorld

defIOHelper :: LimeType -> LimeType
defIOHelper t = TADT "IOHelper" [("IOHelper", [defWorld, t])] [t]

defIO :: LimeType -> LimeType
defIO t = TNamed "IO" [t] $ TLambda defWorld $ defIOHelper t

defTV :: Int -> LimeType
defTV i = TVar i Set.empty

defaultTypes :: [(Text, Scheme)]
defaultTypes = 
    [ ("Int", Forall [] defInt),
      ("Char", Forall [] defChar),
      ("Float", Forall [] defFloat),
      ("String", Forall [] defString),
      ("Unit", Forall [] defUnit),
      ("World", Forall [] defWorld)
    ]

builtinFunctions :: Map Text Scheme
builtinFunctions = Map.fromList
    [ ("__add", Forall [] $ infixType defInt),
      ("__sub", Forall [] $ infixType defInt),
      ("__mul", Forall [] $ infixType defInt),
      ("__div", Forall [] $ infixType defInt)
    ]

defaultValues :: Map Text Scheme
defaultValues = builtinFunctions

defaultTypeEnv :: TypeEnv
defaultTypeEnv = (Map.fromList defaultTypes, defaultValues)

typecheckAllI :: [LimeNode] -> TypeEnv -> Typechecker (TypeEnv, [LimeNode])
typecheckAllI [] env = pure (env, [])
typecheckAllI (n:ns) env = do
    curTVar .= 0
    (env', n') <- topLevelTypecheck env n
    (env'', n2) <- typecheckAllI ns env'
    pure $ (env'', n':n2)

typecheckAll :: [LimeNode] -> Typechecker (TypeEnv, [LimeNode])
typecheckAll ns = typecheckAllI ns defaultTypeEnv

printPos :: Pos -> Text -> TypecheckError -> IO ()
printPos (SourcePos sf sln scn, SourcePos ef eln ecn) text msg = do
    let line :: Int
        line = unPos sln - 1
        ls = lines $ T.unpack text
        l = ls !! line
        t = Prelude.take (unPos ecn - unPos scn) $ drop (unPos scn - 1) l
        lns = show $ unPos sln
        lnsl = length lns + 1
    -- print ls
    putStrLn $ sf <> " " <> show (unPos sln) <> ":" <> show (unPos scn) <> "-" <> show (unPos eln) <> ":" <> show (unPos ecn)
    putStrLn $ replicate lnsl ' ' <> "|"
    putStrLn $ lns <> " | " <> l
    putStrLn $ replicate lnsl ' ' <> "| " <> replicate (unPos scn - 1) ' ' <> replicate (unPos ecn - unPos scn) '^'
    case msg of
        TEInfiniteType -> putStrLn "infinite type"
        TEMismatch t1 t2 -> T.putStrLn $ "type mismatch: " <> printType t1 <> " and " <> printType t2
        TEVarNotFound n -> T.putStrLn $ "variable not found: " <> n
        TETypeNotFound n -> T.putStrLn $ "type not found: " <> n
        TEUnsupportedExpr n -> putStrLn $ "unsupported expr: " <> show n
        TEUnsupportedPMExpr n -> putStrLn $ "unsupported pattern matched expr: " <> show n
        TEUnsupportedTLExpr n -> putStrLn $ "unsupported type level expr: " <> show n 
        TENonFN -> putStrLn "can't apply non-function"

printTypeGo :: LimeType -> Text
printTypeGo = \case
    TLambda a r -> "func (" <> printTypeGo a <> ") " <> printTypeGo r
    TVar i cs -> "p" <> T.pack (show i)
    TPrim p -> case p of
        PFloat _ -> "float"
        PInt _ _ -> "int"
        PString -> "string"
        PUnit -> "Unit"
        PWorld -> "World"
    TADT _ _ _ -> "ADT"
    TRec _ _ -> "ADT"
    TNamed _ _ t -> printTypeGo t
    Unchecked -> "Unchecked"

printTypeEnv :: Map Text Scheme -> Text
printTypeEnv env = foldl' (\b (n, (Forall s t)) -> b <> n <> " :: " <> (if null s then "" else (T.concat $ map (\(a, cs) -> "(" <> T.intercalate ", " (Set.elems cs) <> (if Set.null cs then "" else " ") <> "p" <> T.pack (show a) <> ") ") s) <> "=> ") <>  printType t <> "\n") "" $ Map.toList env

type Counter = State Int

data Value
    = VInt Int
    | VString Text
    | VArray LimeType [Value]
    | VFunc (Text, LimeType, LimeType) [Instruction]
    | VConstructorIndex Text
    deriving Show

data Instruction
    = IApply
    | IConst Value
    | IBlock LimeType [Instruction]
    | IGetVal Text
    | IGetFunc Text LimeType
    | IGetADTType
    | IDup
    | IDrop
    | IFlip
    | ISwitch LimeType [(Value, [Instruction])]
    | ISetVal Text
    | IIndexADT Text Int
    deriving Show

data LinearizerState = LinearizerState
    { _functions :: [(Text, [Instruction], LimeType)]
    , _types :: [LimeType]
    }
    deriving Show

makeLenses ''LinearizerState

addTopLevelFunction :: Text -> [Instruction] -> LimeType -> Linearizer ()
addTopLevelFunction n f t = do
    functions %= ((n, f, t):)

type Linearizer = State LinearizerState

linearizePMCase :: LimeNode -> (Value, [Instruction], (Text, Int))
linearizePMCase (LimeNode node pos ninfo) = case node of
    Int v -> (VInt v, [], ("", 0))
    Symbol s -> (VConstructorIndex s, [], (s, 0))
    FunCall f (LimeNode a _ _) -> let (v, is, (n, i)) = linearizePMCase f in case a of
        Symbol s -> (v, is <> [IDup, IIndexADT n i, ISetVal s], (n, i+1))
        Discard -> (v, is, (n, i+1))
        _ -> error "unsupported match2"
    _ -> error "unsupported match"

linearize :: LimeNode -> Linearizer [Instruction]
linearize n@(LimeNode node npos ninfo) = case node of
    Symbol s -> pure $ case ninfo of
        TLambda _ _ -> [IGetFunc s ninfo]
        _ -> [IGetVal s]
    -- Lambda l@[LimeNode (Symbol a) _ _] r@(LimeNode _ _ _) -> do
    FunCall f a -> do
        f' <- linearize f
        a' <- linearize a
        pure $ a' <> f' <> [IApply]
    Lambda [ln@(LimeNode (Symbol a) _ ainfo)] r@(LimeNode _ _ rinfo) -> do
        r' <- linearize r 
        pure [IConst $ VFunc (a, ainfo, rinfo) r']
    Case v@(LimeNode _ _ vinfo) cs -> do
        v' <- linearize v
        cs' <- traverse (\(l,r) -> do
            r' <- linearize r
            let (l', lr, _) = linearizePMCase l
            pure (l', [IFlip] <> lr <> [IDrop] <> r')) cs
        let adtStuff = case vinfo of
                TADT _ _ _ -> [IDup, IGetADTType]
                _ -> []
        pure $ v' <> adtStuff <> [ISwitch ninfo cs']
    Let l@(LimeNode (Symbol a) _ _) r e -> do
        r' <- linearize r
        e' <- linearize e
        pure $ [IBlock (info e) (r' <> [ISetVal a] <> e')]
    Int v -> pure [IConst $ VInt v]
    Char v -> pure [IConst $ VInt $ fromEnum v]
    String v -> pure [IConst $ VString v]
    _ -> error $ T.unpack $ printNode n

    -- catch all so the darn language server doesn't complain
    -- _ -> throwError (npos, TEUnsupportedExpr n)
    

linearizeTopLevel :: LimeNode -> Linearizer ()
linearizeTopLevel n@(LimeNode node npos ninfo) = case node of
    -- Infix AssignType l@(LimeNode (Symbol s) _ _) r -> do
    Infix AssignValue (LimeNode (Symbol s) _ _) b -> do
        b' <- linearize b
        addTopLevelFunction s b' ninfo
    -- Data name ms -> do
    _ -> pure ()

data GoBackendData = GoBackendData
    { _stack :: [Int]
    , _counter :: Int
    , _recs :: Map Text Int
    }
    deriving Show

makeLenses ''GoBackendData

type GoBackend = State GoBackendData

toCVar :: Int -> Text
toCVar i = "_" <> T.pack (show i)

popStack :: GoBackend Text
popStack = do
    s <- gets $ head . _stack
    stack %= tail
    pure $ toCVar s

peekStack :: GoBackend Text
peekStack = gets $ toCVar . head . _stack

pushStack :: GoBackend Text
pushStack = do
    c <- gets _counter
    counter %= (+1)
    stack %= (c:)
    pure $ toCVar c

valueToGo :: Value -> GoBackend Text
valueToGo v = case v of
    VInt i -> pure $ T.pack (show i)
    VString i -> pure $ T.pack (show i)
    VArray t v -> do
        v' <- (T.intercalate ", " <$> (traverse valueToGo v))
        pure $ "[]" <> printTypeGo t <> "{" <> v'
    VFunc (n, t, ret) is -> do
        is' <- (T.intercalate "\n    " <$> (traverse linearizedToGo is))
        r' <- popStack
        pure $ "func (_" <> n <> " " <> printTypeGo t <> ") " <> printTypeGo ret <> " {\n    " <> is' <> "\n    return " <> r' <> "\n    }"
    VConstructorIndex t -> do
        pure $ "_cni" <> t

linearizedToGo :: Instruction -> GoBackend Text
linearizedToGo = \case
    IApply -> do
        f <- popStack
        a <- popStack
        r <- pushStack
        pure $ r <> " := " <> f <> "(" <> a <> ")"
    IConst v -> do
        r <- pushStack
        v' <- valueToGo v
        pure $ r <> " := " <> v'
    IBlock t is -> do
        ret <- gets _counter
        counter %= (+1)
        is' <- (T.intercalate "\n    " <$> (traverse linearizedToGo is))

        s <- gets _stack
        -- retrieve last value from block
        v <- popStack
        -- push return value onto stack
        stack %= (ret:)
        pure $ "var " <> toCVar ret <> " " <> printTypeGo t <> "\n    {\n    " <> is' <> "\n    " <> toCVar ret <> " = " <> v <> "\n    }"
    IGetVal v -> do
        r <- pushStack
        rs <- gets _recs
        pure $ r <> " := " <> if isUpperCase $ T.head v then "_cnv" <> v else
            case rs !? v of
                Just f -> "_fn" <> T.pack (show f) <> "()"
                Nothing -> "_" <> v
    IGetFunc v t -> do
        r <- pushStack
        rs <- gets _recs
        pure $ r <> " := " <> if isUpperCase $ T.head v then "_cnv" <> v else
            case rs !? v of
                Just f -> "_fn" <> T.pack (show f) <> "()"
                Nothing -> "_" <> v
    IGetADTType -> do
        v <- popStack
        r <- pushStack
        pure $ r <> " := " <> v <> ".t"
    IDup -> do
        v <- gets $ head . _stack
        stack %= (v:)
        pure ""
    IDrop -> do
        _ <- popStack
        pure $ ""
    IFlip -> do
        stack %= (\(a:b:cs) -> (b:a:cs))
        pure ""
    ISwitch t cs -> do
        v <- popStack
        ret <- pushStack
        let f (l, r) = do
                s <- gets _stack

                l' <- valueToGo l
                r' <- T.intercalate "\n    " <$> (traverse linearizedToGo r)
                final <- popStack

                stack .= s
                pure $ "    case " <> l' <> ":\n    " <> r' <> "\n    " <>
                    ret <> " = " <> final
        cs' <- T.intercalate "\n" <$> traverse f cs
        -- drop ADT value
        stack %= (\(a:_:cs) -> (a:cs))
        pure $ "var " <> ret <> " " <> printTypeGo t <> "\n    switch (" <> v <> ") {\n" <> cs' <> "\n    }"
    ISetVal s -> do
        v <- popStack
        pure $ "_" <> s <> " := " <> v
    IIndexADT n i -> do
        v <- popStack
        r <- pushStack
        pure $ r <> " := " <> v <> ".v.(_cns" <> n <> ")._" <> T.pack (show i)

functionToGo :: Int -> [Instruction] -> LimeType -> Text -> Text
functionToGo name is t tname = evalState f (GoBackendData [] 0 (Map.singleton tname name)) 
    where f = do
            let comment = "// " <> tname <> " :: " <> printType t <> "\n"
            let header = "func _fn" <> T.pack (show name) <> "() " <> printTypeGo t <> " {\n    "
            v <- T.intercalate "\n    " <$> (traverse linearizedToGo is)
            r <- popStack
            _ <- (assert . null) <$> gets _stack
            pure $ comment <> header <> v <> "\n    return " <> r <> ";\n}\n\n" <> nameDef
          nameDef = "var _" <> tname <> " = _fn" <> T.pack (show name) <> "()"

goGenerateConstructor :: (Text, [LimeType]) -> Text
goGenerateConstructor (name, ts) = if null ts then cnvDef <> "ADT{" <> cniName <> ",nil}"
    else "type _cns" <> name <> " struct {\n    " <> T.intercalate "\n    " (zipWith (\a b -> "_" <> (T.pack $ show b) <> " " <> printTypeGo a) ts [0..]) <> "\n}\n"
        <> cnvDef <> recvCnv ts ("ADT{" <> cniName <> "," <> cnsName <> "{" <> T.intercalate "," (zipWith (\_ b -> "_" <> T.pack (show b)) ts [0..]) <> "}" <> "}") 0
    where cnvDef = "var _cnv" <> name <> " = "
          cnsName = "_cns" <> name
          cniName = "_cni" <> name
          recvCnv [t] s i = "func (_" <> T.pack (show i) <> " " <> printTypeGo t <> ") ADT {\n    return " <> s <> "}"
          recvCnv tsf@(t:ts) s i = "func (_" <> T.pack (show i) <> " " <> printTypeGo t <> ") " <> printTypeGo (recvCnvType ts) <> " {\n    return " <> recvCnv ts s (i+1) <> "}"
          recvCnv [] s _ = s
          recvCnvType :: [LimeType] -> LimeType
          recvCnvType [t] = TLambda t $ TADT "" [] []
          recvCnvType (t:ts) = TLambda t $ recvCnvType ts
          recvCnvType [] = Unchecked

-- rename type later
typeToGo :: LimeType -> Text
typeToGo t = case t of
    TADT _ ms _ -> (fst $ foldl' f ("", 0) ms) <> "\n"
        where f (b, c) a = (b <> "var _cni" <> fst a <> " = " <> T.pack (show c) <> "\n" <> goGenerateConstructor a <> "\n", c+1)
    _ -> ""

data Literal 
    = LInt Int
    | LFloat Float
    | LString Text
    | LList [Literal]

data IR
    -- name type
    = IRVar Text LimeType
    -- literal
    | IRLit Literal
    -- fn arg
    | IRApp IR IR
    -- argname rhs
    | IRLam Text IR
    -- type lambda, arg
    | IRTApp IR LimeType
    -- tvar rhs
    | IRTLam Int IR
    -- name rhs
    | IRLet Text IR
    -- TBD
    | IRCase

data FType
    = FTFn LimeNode
    | FTCon Text
    | FTCFn Text LimeType
    deriving Show

data MonomorphizerState = MonomorphizerState
    { _monomorphizedFns :: Map (Text, LimeType) LimeNode
    , _reqTypes :: Set.Set (Text, LimeType)
    , _mFunctions :: (Map Text FType)
    , _mImplementations :: Map (Text, LimeType) LimeNode
    , _mTypes :: (Map Text Scheme)
    , _curMFn :: Text
    }
    deriving Show

makeLenses ''MonomorphizerState

type Monomorphizer = State MonomorphizerState

mUnifyList :: [LimeType] -> [LimeType] -> Map Int LimeType
mUnifyList as bs = foldl' helper Map.empty $ zip as bs
    where helper s1 (ta,tb) =
            let s2 = mUnify (apply s1 ta) (apply s1 tb)
            in Map.union s2 s1

mUnify :: LimeType -> LimeType -> Map Int LimeType
mUnify (TVar a _) b = Map.singleton a b
mUnify a (TVar b _) = Map.singleton b a
mUnify (TLambda l1 r1) (TLambda l2 r2) = let s1 = mUnify l1 l2 in Map.union s1 (mUnify (apply s1 r1) (apply s1 r2))
mUnify (TADT a _ ats) (TADT b _ bts) = mUnifyList ats bts
mUnify a (TNamed _ _ b) = mUnify a b
mUnify (TNamed _ _ a) b = mUnify a b
mUnify a b = Map.empty

monomorphizeFunCall :: LimeNode -> Map Int LimeType -> Monomorphizer (LimeNode, Maybe FType, LimeType, Map Int LimeType)
monomorphizeFunCall n@(LimeNode node _ ninfo') env = let ninfo = apply env ninfo' in case node of
    FunCall f a -> do
        (f', n', ft, fnEnv) <- monomorphizeFunCall f env
        a' <- monomorphize env a

        -- let f la lr = trace (show fnEnv') $ pure $ (n { expr = FunCall f' a', info = ninfo}, n', lr, fnEnv')
        let f la lr = pure $ (n { expr = FunCall f' a' }, n', lr, fnEnv')
                where fnEnv' = Map.union (mUnify la (info a)) fnEnv
                -- where fnEnv' = trace ((T.unpack $ printType la) <> " <-> " <> (T.unpack $ printType $ info a') <> "\na' = " <> show env) Map.union (mUnify la (info a')) fnEnv
        case ft of
            TLambda la lr -> f la lr
            _ -> f ft ft
    Symbol s -> do
        v <- gets _mFunctions
        cur <- gets _curMFn
        let n' = n { info = ninfo }
        if s == cur then
            pure (n', Nothing, ninfo, Map.empty)
        else
            case v !? s of
                -- not sure about this one
                -- should probably apply
                Just mf -> case mf of
                    FTFn f -> pure (n' { info = apply env ninfo }, Just mf, info f, Map.empty)
                    FTCon _ -> pure (n', Just mf, ninfo, Map.empty)
                    FTCFn _ _ -> do
                        impls <- gets _mImplementations
                        case impls !? (s, apply env ninfo) of
                            -- TODO monomorphize class instances
                            Just f -> do
                                monomorphize Map.empty f
                                pure (n' { info = apply env ninfo }, Just $ FTCFn s $ apply env ninfo, info f, Map.empty)
                            Nothing -> error $ T.unpack $ "typeclass instance for function " <> s <> " with signature " <> printType (apply env ninfo) <> " not found."
                Nothing -> pure (n', Nothing, ninfo, Map.empty)
                -- Nothing -> case builtinFunctions !? s of
                --     Just _ -> pure (n, Nothing, ninfo, Map.empty)
                --     Nothing -> error $ "could not find function " <> T.unpack s <> " in env"
    _ -> monomorphize env n >>= \n' -> pure (n', Nothing, ninfo, Map.empty)

monomorphizePMCase :: Map Int LimeType -> Int -> LimeNode -> Monomorphizer ()
monomorphizePMCase env i n@(LimeNode node pos ninfo) = case node of
    FunCall f (LimeNode a _ _) -> monomorphizePMCase env (i+1) f
    Symbol s -> do
        v <- gets _mFunctions
        case v !? s of
            -- Just mf -> trace (T.unpack s <> show (apply env ninfo)) $ case mf of
            Just mf -> case mf of
                FTCon c -> reqTypes %= Set.insert (c, getRet i $ apply env ninfo)
                    where getRet i t = if i > 0 then case t of
                            TLambda _ r -> getRet (i-1) r
                            _ -> error "don't do"
                            else t
                _ -> pure ()
            _ -> pure ()
    _ -> pure ()

monomorphize :: Map Int LimeType -> LimeNode -> Monomorphizer LimeNode
monomorphize env n@(LimeNode node npos ninfo') = let ninfo = apply env ninfo' in case node of
    FunCall f a -> do
        (n', r, ta, fnEnv) <- monomorphizeFunCall n env
        -- TODO figure out whether this is even necessary
        let (LimeNode (FunCall f' a') _ _) = n'
            fnEnv' = Map.union (mUnify (info a') ta) fnEnv
        case r of
            Just r' -> case r' of
                FTFn r'' -> monomorphizeFnTL r'' fnEnv'
                FTCon c -> reqTypes %= Set.insert (c, ninfo)
                -- TODO monomorphize class instances
                FTCFn name finfo -> do
                    -- impls <- trace (show name <> " " <> (T.unpack $ printType finfo)) $ gets _mImplementations
                    impls <- gets _mImplementations
                    case impls !? (name, finfo) of
                        -- TODO monomorphize class instances
                        Just f -> trace (T.unpack $ printType finfo) $ monomorphizedFns %= Map.insert (name, finfo) (n { expr = Infix AssignValue (LimeNode (Symbol name) npos finfo) f, info = finfo })
                        Nothing -> error "impossible"
            Nothing -> pure ()
        pure $ n' { info = ninfo }    
    Symbol s -> do
        v <- gets _mFunctions
        cur <- gets _curMFn
        if s == cur then
            pure ()
        else
            case v !? s of
                Just mf -> case mf of
                    FTFn r'' -> do
                        let fnEnv = mUnify ninfo (info r'')
                        monomorphizeFnTL r'' fnEnv
                    FTCon c -> reqTypes %= Set.insert (c, ninfo)
                Nothing -> pure ()
        pure $ n { info = ninfo }
    Lambda l r@(LimeNode _ _ rinfo) -> do
        r' <- monomorphize env r
        l' <- traverse (monomorphize env) l
        pure $ n { expr = Lambda l' r', info = ninfo }
    Prefix op r -> do
        r' <- monomorphize env r
        pure n { expr = Prefix op r' }
    Let l r e -> do
        -- TODO monomorphize lets
        -- doesn't work yet because needs pattern matching
        -- l' <- monomorphize env l
        r' <- monomorphize env r
        e' <- monomorphize env e
        pure n { expr = Let l r' e' }
        -- pure n
    Case v cs -> do
        v' <- monomorphize env v
        cs' <- traverse (\(a, b) -> (a,) <$> (monomorphize env b) <* (monomorphizePMCase env 0 a)) cs
        pure n { expr = Case v' cs', info = ninfo }
    Discard -> pure n
    Int _ -> pure n
    Char _ -> pure n
    Bool _ -> pure n
    String _ -> pure n
    List _ -> pure n

monomorphizeFnTL :: LimeNode -> Map Int LimeType -> Monomorphizer ()
monomorphizeFnTL n@(LimeNode node _ ninfo) env = case node of
    Infix AssignValue l@(LimeNode (Symbol s) _ _) r -> do
        exists <- gets (\t -> (s, apply env ninfo) `Map.member` _monomorphizedFns t)
        -- if trace (T.unpack s <> "\n" <> T.unpack (printTypeEnv $ Map.mapKeys (T.pack . show) $ Map.map (Forall []) env)) exists
        if exists
        then pure ()
        else do
            cur <- gets _curMFn
            curMFn .= s
            r' <- monomorphize env r
            monomorphizedFns %= Map.insert (s, apply env ninfo) (n { expr = Infix AssignValue l r', info = apply env ninfo })
            curMFn .= cur
    _ -> pure ()

collectNodeName :: LimeNode -> Map Text FType
collectNodeName n@(LimeNode node _ _) = case node of
    -- Infix AssignType (LimeNode (Symbol s) _ _) _ -> Map.singleton s $ FTFn n
    Infix AssignValue (LimeNode (Symbol s) _ _) _ -> Map.singleton s $ FTFn n
    -- data declarations aren't collected because they don't belong in the var env
    Class name tv ds -> foldl' j Map.empty ds
        where j :: Map Text FType -> LimeNode -> Map Text FType
              j b an@(LimeNode a _ _) = case a of
                Infix AssignType (LimeNode (Symbol s) _ _) _ -> Map.insert s (FTCFn s Unchecked) b
                _ -> b
    _ -> Map.empty

collectNodeNames :: [LimeNode] -> Map Text FType
collectNodeNames ns = foldl' (\b n -> Map.union b $ collectNodeName n) Map.empty ns

parseLime :: String -> Text -> IO [LimeNode]
parseLime filename contents = let (Identity (parsed, s)) = runStateT (runParserT program filename contents) $ ISSet 1
    in case parsed of
        Right p -> pure p
        Left e -> do
            putStrLn $ errorBundlePretty e 
            print s
            exitFailure

typecheckLime :: [LimeNode] -> Text -> IO (TypeEnv, [LimeNode], Map (Text, LimeType) LimeNode)
typecheckLime ns fileContents = let Identity (r, s) = runStateT (runExceptT $ typecheckAll ns) (TypecheckerState 0 Map.empty Map.empty Map.empty)
    in case r of
        Left (pos, msg) -> do
            printPos pos fileContents msg
            putStr "\n"
            exitFailure
        Right (env, ns) -> pure (env, ns, _implementations s)

printValue :: Int -> Value -> Text
printValue i v = case v of
    VInt i -> "VInt " <> T.pack (show i)
    VString i -> "VString " <> T.pack (show i)
    VArray _ a -> "Varray [" <> T.intercalate ", " (map (printValue 0) a) <> "]"
    VFunc (n,_,_) is -> "VFunc " <> n <> "\n" <> (T.intercalate "\n" $ map (printInstruction (i+4)) is)
    VConstructorIndex t -> "VConstructorIndex " <> t

printInstruction :: Int -> Instruction -> Text
printInstruction i inst = T.replicate i " " <> case inst of
    IApply -> "IApply"
    IConst v -> "IConst " <> printValue i v
    IBlock _ is -> "IBlock\n" <> (T.intercalate "\n" $ map (printInstruction (i+4)) is)
    IGetVal v -> "IGetVal " <> v
    IGetFunc v t -> "IGetFunc " <> v <> " " <> printType t
    IGetADTType -> "IGetADTType"
    IDup -> "IDup"
    IDrop -> "IDrop"
    IFlip -> "IFlip"
    ISwitch _ cs -> "ISwitch\n" <> T.intercalate "\n" (map (\(l, r) -> T.replicate (i+4) " " <> printValue 0 l <> "\n" <> printInstructions (i+8) r) cs)
    ISetVal t -> "ISetVal " <> t
    IIndexADT n idx -> "IIndexADT " <> n <> " " <> T.pack (show idx)
    
printInstructions :: Int -> [Instruction] -> Text
printInstructions i v = T.intercalate "\n" $ map (printInstruction i) v

generateTypeName :: LimeType -> Text
generateTypeName = \case
    TLambda a r -> "L" <> generateTypeName a <> generateTypeName r
    TVar i cs -> "p" <> T.pack (show i)
    TPrim p -> case p of
        PFloat _ -> "_Float"
        PInt _ _ -> "_Int"
        PUnit -> "_Unit"
        PWorld -> "_World"
    -- add type applications to the end
    TADT n _ _ -> "_" <> n
    TNamed n _ _ -> "_" <> n
    Unchecked -> "Unchecked"

collectConstructors :: [LimeType] -> [(Text, FType)]
collectConstructors ts = concatMap getConstructors $ filter isADT ts
    where isADT t = case t of
            TADT _ _ _ -> True
            _ -> False
          getConstructors t = case t of
            TADT n cs _ -> zip (map fst cs) (repeat $ FTCon n)

main :: IO ()
main = do
    test <- T.readFile "test.lm"
    -- let test = "add a b = a + b\nmain = add 1 2\njoe = 1 + 3"
    parsed <- parseLime "test.lm" test
    let simplified = map simplifyLime parsed
    traverse_ (T.putStrLn . printNode) simplified
    putStrLn ""
    (env, ns, impls) <- typecheckLime simplified test

    let mappedNodes = collectNodeNames ns

    -- T.putStrLn $ T.intercalate "\n" $ fmap (\(a,b) -> let FTFn b' = b in a <> ": " <> printNode b') $ Map.toList $ mappedNodes
    T.putStrLn $ T.intercalate "\n" $ fmap (\(a,b) -> case b of
        FTFn b' -> a <> ": " <> printNode b'
        FTCFn s t -> a <> ": FTCFn " <> s) $ Map.toList $ mappedNodes

    T.putStrLn ""

    T.putStrLn $ printTypeEnv $ fst env
    T.putStrLn $ printTypeEnv $ snd env
    T.putStrLn "joue"

    case mappedNodes !? "main" of
        Nothing -> putStrLn "ERROR: no main function found"
        Just m -> let (_, s)    = runState (monomorphizeFnTL m' Map.empty) (MonomorphizerState Map.empty Set.empty envFns impls (fst env) "")
                      envFns    = (Map.fromList $ collectConstructors $ map schemeType $ Map.elems $ fst env) <> mappedNodes
                      (FTFn m') = m
                      mfns      = (\((a, b), c) -> (a, b, c)) <$> (Map.toList $ _monomorphizedFns s)
                      rtypes    = _reqTypes s
                in do
                let (r, s) = runState (traverse (\(_,_,a) -> linearizeTopLevel a) mfns) (LinearizerState [] [])
                let newFunctions = map (\(n,b,c) -> if n == "main" then ("_lmmain",b,c) else (n,b,c)) $ _functions s
                T.putStrLn $ T.intercalate "\n" (map (\(a,b,_) -> a <> " :: " <> printType b) mfns)
                T.putStrLn ""
                -- print $ envFns
                -- print $ mfns
                print $ rtypes
                T.putStrLn ""
                -- putStrLn $ Data.List.intercalate "\n" (map (\(a,b,_) -> T.unpack a <> " :: " <> show b) mfns)
                T.putStrLn $ T.intercalate "\n\n" $ map (\(_,v,_) -> printInstructions 0 v) $ newFunctions
                -- print m

                header <- T.readFile "header.go"
                T.writeFile "out.go" $ header <> "\n" <> 
                    -- (T.concat $ map typeToGo $ map schemeType $ filter (\(Forall tvs _) -> null tvs) (Map.elems $ fst env)) <>
                    (T.concat $ map (typeToGo . snd) $ Set.elems rtypes) <>
                    (T.intercalate "\n\n" $ map (\(a, (d, b, c)) -> functionToGo a b c d) (zip [0..] newFunctions))
                putStrLn ""

                callCommand "go run out.go"
