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
import Debug.Trace (trace)
import qualified Data.Text.Internal.Fusion.Size as T
import Control.Exception (assert)

-- not sure if ISInvalid is even needed
data IndentationState = ISUnset Int | ISSet Int | ISInvalid deriving Show

type Parser = ParsecT Void Text (State IndentationState)

-- turn arithmetic into infix functions
data LimeOp =
    AssignType    -- ::
    | AssignValue -- =
    | FunArrow    -- ->
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

indentGuard :: Parser ()
indentGuard = get >>= \case
    ISUnset i -> do
        sc
        l <- unPos <$> L.indentLevel
        if l >= i
        then put $ ISSet l
        else L.incorrectIndent GT (mkPos i) (mkPos l)
    ISSet i -> do
        sc
        l <- unPos <$> L.indentLevel
        if l >= i
        then pure ()
        else L.incorrectIndent GT (mkPos i) (mkPos l)
    ISInvalid -> fancyFailure . Set.singleton $ ErrorFail "invalid indent"

scn :: Parser ()
scn = sc *> (void (optional (try (void eol *> void (indentGuard)))))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: Text -> Parser Text
symbol = L.symbol scn

limeSymbol :: Parser LimeExpr
limeSymbol = Symbol . T.pack <$> lexeme (some (letterChar <|> numberChar <|> char '_'))

limeInt :: Parser LimeExpr
limeInt = Int . read @Int <$> lexeme (some digitChar)

limeLet :: Parser LimeExpr
limeLet = do
    _ <- symbol "let"
    left <- limeNode
    _ <- symbol "="
    right <- limeNode
    _ <- symbol "in"
    expr <- limeNode
    pure $ Let left right expr

limeData :: Parser LimeExpr
limeData = do
    _ <- symbol "data"
    left <- T.pack <$> lexeme (some letterChar)
    tvs <- many (T.pack <$> lexeme (some letterChar))
    _ <- symbol "="
    right <- ((,) <$> (T.pack <$> lexeme (some letterChar)) <*> (many (limeTermToNode limeSymbol <|> limeTypeLevelParen))) `sepBy` symbol "|"
    pure $ Data left tvs right

caseBlock :: Parser [(LimeNode, LimeNode)]
caseBlock = (some (p <* lexeme eol))
    where p = do
            -- l <- dbg "node" limeNode
            l <- limeNode
            indentBracket $ do
                _ <- symbol "->"
                r <- limeNode
                pure (l, r)

limeCase :: Parser LimeExpr
limeCase = do
    _ <- symbol "case"
    -- v <- dbg "node" limeNode
    v <- limeNode
    indentBracket $ do
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

limeDiscard :: Parser LimeExpr
limeDiscard = Discard <$ symbol "_"

limeTerm :: Parser LimeNode
limeTerm = do
    notFollowedBy keyword
    limeParen <|> (limeTermToNode $ choice [limeCase, limeDiscard, limeInt, lexeme limeLambda, lexeme limeLet, limeSymbol])

limeTopLevelNode :: Parser LimeNode
limeTopLevelNode = limeTermToNode $ choice [limeData] 

indentBracket :: Parser a -> Parser a
indentBracket p = do
    o <- get
    modify (\case
        ISSet a -> ISUnset $ a + 1
        a -> a)
    res <- p
    -- case o of
    --     ISUnset _ -> put ISInvalid
    --     _ -> put o
    pure res

limeTopLevel :: Parser LimeNode
limeTopLevel = L.nonIndented scn $ (put $ ISSet 1) *> indentBracket (limeTopLevelNode <|> p)
    where p = do
            l <- limeNode
            (p2, op) <- choice $ lexeme <$>
                [ (typeLevelNode, AssignType) <$ "::" 
                , (limeNode, AssignValue) <$ "=" ]
            r <- p2
            pure $ nodesToInfix op l r

limeTypeLevelParen :: Parser LimeNode
limeTypeLevelParen = between (symbol "(") (symbol ")") typeLevelNode

typeLevelNode :: Parser LimeNode
typeLevelNode = E.makeExprParser (limeTermToNode limeSymbol <|> limeTypeLevelParen) table
    where
        table = 
            [ [ binary' "" FunCall ]
            , [ binaryR "->" FunArrow ] 
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
    Symbol s -> s
    Bool b -> T.pack $ show b
    String s -> s
    List s -> "[" <> foldl' (\b a -> b <> printNode a <> " ") "" s <> "]"
    Data n tvs ns -> "data " <> n <> (if null tvs then "" else " " <> T.intercalate " " tvs) <> " = " <> T.intercalate " | " (map (\(a, b) -> a <> " " <> (T.intercalate " " (map printNode b))) ns)
    Case v cs -> "case " <> printNode v <> " of\n    " <> (T.intercalate "\n    " $ map (\(l, r) -> printNode l <> " -> " <> printNode r) cs)
    Let l r e ->
        "let " <> printNode l <> " = " <> printNode r <> " in " <> printNode e
    Prefix op r ->
        ""
    Discard -> "_"

data LimePrim
    -- size, signedness, name
    = PInt Int Bool
    -- size, name
    | PFloat Int
    deriving (Show, Eq, Ord)

data LimeType =
    -- arg ret
    TLambda LimeType LimeType
    -- type vars
    -- do i need a type-level tvar?
    | TVar Int
    | TPrim LimePrim
    -- name, fields, applied types
    | TADT Text [(Text, [LimeType])] [LimeType]
    -- recursive ADT (name, applied types)
    | TRec Text [LimeType]
    | TNamed Text LimeType
    -- no assigned type
    | Unchecked
    deriving (Show, Eq, Ord)

printType :: LimeType -> Text
printType = \case
    TLambda a r -> 
        case a of
            TLambda _ _ -> "(" <> printType a <> " -> " <> printType r <> ")"
            _ -> printType a <> " -> " <> printType r
    TPrim p -> case p of
        PInt _ _ -> "Int" 
        PFloat _ -> "Float"
    TVar i -> "p" <> T.pack (show i)
    TADT n ms ats -> n <> T.concat (map (\at -> " " <> printType at) ats) <>  " (" <> T.intercalate " | " (map (\(n', ts) -> n' <> if null ts then "" else " " <> T.intercalate " " (map (\t -> "(" <> printType t <> ")") ts)) ms) <> ")"
    TRec t ats -> t <> T.concat (map (\at -> " " <> printType at) ats)
    TNamed t _ -> t
    Unchecked -> "Unchecked"

data CheckedNode = CheckedNode LimeNode LimeType
    deriving Show

data Scheme = Forall [Int] LimeType
    deriving Show

type TypeEnv = (Map Text Scheme, Map Text Scheme)
type Subst = Map Int LimeType

data TypecheckerState = TypecheckerState
    { _curTVar       :: Int
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

instantiate :: Scheme -> Typechecker LimeType
instantiate (Forall as t) = do
    as' <- mapM (const freshTVar) as
    let s = Map.fromList $ zip as as'
    pure $ apply s t

findVarEnv :: TypeEnv -> Text -> Typechecker (Maybe LimeType)
findVarEnv (_, venv) n = case venv !? n of
    Just a -> instantiate a >>= \t -> pure $ Just t
    Nothing -> pure Nothing

findVarError :: TypeEnv -> Text -> Pos -> Typechecker LimeType
findVarError env n pos = findVarEnv env n >>= \case
    Just a -> pure a
    Nothing -> throwError (pos, TEVarNotFound n)

findTypeEnv :: TypeEnv -> Text -> Typechecker (Maybe LimeType)
findTypeEnv (tenv, _) n = case tenv !? n of
    Just a -> instantiate a >>= \t -> pure $ Just t
    Nothing -> pure Nothing

findTypeError :: TypeEnv -> Text -> Pos -> Typechecker LimeType
findTypeError env n pos = findTypeEnv env n >>= \case
    Just a -> pure a
    Nothing -> throwError (pos, TETypeNotFound n)

typeLevelEval :: TypeEnv -> LimeNode -> Typechecker Scheme
typeLevelEval env@(tenv, _) n@(LimeNode node pos _) = case node of
    Symbol s -> case tenv !? s of
            Just a -> pure a
            Nothing -> if isUpperCase $ T.head s 
                then throwError (pos, TETypeNotFound s)
                -- keep track of created tvars in env
                else freshTVar >>= \(TVar v) -> pure $ Forall [v] $ TVar v
    Infix FunArrow l r -> do
        (Forall lvs lt) <- typeLevelEval env l
        (Forall rvs rt) <- typeLevelEval env r
        pure $ Forall (lvs <> rvs) $ TLambda lt rt
    FunCall f a -> do
        f' <- typeLevelEval env f
        let (Forall (ftv:ftvs) f'') = f'
        a' <- typeLevelEval env a
        b <- bind pos ftv $ schemeType a'
        pure (Forall ftvs $ apply b f'')
    _ -> throwError (pos, TEUnsupportedTLExpr n)

freshTVar :: Typechecker LimeType
freshTVar = do
    n <- use curTVar
    curTVar %= (+1)
    pure $ TVar n

freshTVarI :: Typechecker Int
freshTVarI = do
    n <- use curTVar
    curTVar %= (+1)
    pure n

schemeType :: Scheme -> LimeType
schemeType (Forall _ t) = t

getType :: LimeNode -> LimeType
getType (LimeNode _ _ t) = t

apply :: Subst -> LimeType -> LimeType
apply s t@(TVar v) = Map.findWithDefault t v s
apply s (TLambda a r) = TLambda (apply s a) (apply s r)
apply s (TADT t ms ats) = TADT t (map (\(a, ts) -> (a, map (apply s) ts)) ms) $ map (apply s) ats
apply s (TRec t ats) = TRec t $ map (apply s) ats
apply _ t = t

applyS :: Subst -> Scheme -> Scheme
applyS s (Forall as t) = Forall as $ apply s' t
    where s' = foldr Map.delete s as

applyEnv :: Subst -> TypeEnv -> TypeEnv
applyEnv s (tenv, venv) = (tenv, Map.map (applyS s) venv)

unifyList :: Pos -> [LimeType] -> [LimeType] -> Typechecker Subst
unifyList pos as bs = foldl' helper (pure Map.empty) $ zip as bs
    where helper b (ta,tb) = do
            s1 <- b
            s2 <- unify pos (apply s1 ta) (apply s1 tb)
            pure $ Map.union s2 s1

unify :: Pos -> LimeType -> LimeType -> Typechecker Subst
-- this rule needs to be at the top, so that names are preserved
unify pos (TVar a) t = bind pos a t
unify pos t (TVar a) = bind pos a t
unify pos (TNamed _ a) (TNamed _ b) = unify pos a b
unify pos (TNamed _ a) b = unify pos a b
unify pos a (TNamed _ b) = unify pos a b
unify pos (TLambda l1 r1) (TLambda l2 r2) = do
    s1 <- unify pos l1 l2
    s2 <- unify pos (apply s1 r1) (apply s1 r2)
    pure $ Map.union s2 s1
-- TODO see whether there's limits to using nominalism beyond type application
unify pos (TRec a ats) (TRec b bts) | a == b = unifyList pos ats bts
unify pos (TRec a ats) (TADT b _ bts) | a == b = unifyList pos ats bts
unify pos (TADT a _ ats) (TRec b bts) | a == b = unifyList pos ats bts
unify pos (TADT a _ ats) (TADT b _ bts) | a == b = unifyList pos ats bts
unify _ (TPrim a) (TPrim b) | a == b = pure Map.empty
unify pos t1 t2 = throwError $ (pos, TEMismatch t1 t2)

occurs :: Int -> LimeType -> Bool
occurs v = \case
    TLambda t1 t2 -> occurs v t1 && occurs v t2
    TVar v' -> v == v'
    TPrim _ -> False
    -- TODO fix occurs check for ADT, TRec, TFun and TApp
    TRec _ _ -> False
    TADT _ _ _ -> False
    TNamed _ t -> occurs v t
    Unchecked -> False

bind :: Pos -> Int -> LimeType -> Typechecker Subst
bind pos a t
    | t == TVar a = return Map.empty
    | occurs a t  = throwError (pos, TEInfiniteType)
    | otherwise   = pure $ Map.singleton a t

replaceType :: LimeNode -> LimeType -> LimeNode
replaceType n t = n { info = t }

applyN :: Subst -> LimeNode -> LimeNode
applyN s n@(LimeNode _ _ t) = n { info = apply s t }

freeTypeVars :: LimeType -> Set.Set Int
freeTypeVars = \case
    TLambda a r -> freeTypeVars a `Set.union` freeTypeVars r
    TVar a -> Set.singleton a
    TPrim _ -> Set.empty
    -- TODO fix tvar check for ADTS once generic
    TRec _ ats -> Set.unions $ map freeTypeVars ats
    TADT _ ms ats -> Set.union (Set.unions $ concatMap (map freeTypeVars . snd) ms) (Set.unions $ map freeTypeVars ats)
    TNamed _ t -> freeTypeVars t
    Unchecked -> Set.empty

freeTypeVarsS :: Scheme -> Set.Set Int
freeTypeVarsS (Forall vs t) = freeTypeVars t `Set.difference` Set.fromList vs

-- might cause trouble later, will use this with tenv aswell in case it does
freeTypeVarsEnv :: TypeEnv -> Set.Set Int
freeTypeVarsEnv env = foldl' (flip $ Set.union . freeTypeVarsS) Set.empty $ Map.elems $ snd env

generalize :: TypeEnv -> LimeType -> Scheme
generalize env t = Forall as t
    where as = Set.toList $ freeTypeVars t `Set.difference` freeTypeVarsEnv env

typecheckPatternMatch :: TypeEnv -> LimeNode -> Typechecker (TypeEnv, LimeNode)
typecheckPatternMatch env n@(LimeNode node npos _) = case node of
    Int _ -> pure $ (env, n { info = defInt })
    Symbol s -> 
        findVarError env s npos <&> (\v -> (env, n { info = v }))
    FunCall f (LimeNode a _ _) -> do
        (env'@(tenv, venv), f') <- typecheckPatternMatch env f 
        case info f' of
            TLambda arg ret ->
                case a of
                    Symbol s ->
                        pure ((tenv, Map.insert s (Forall [] arg) venv), n { info = ret })
                    Discard ->
                        pure (env', n { info = ret })
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
        -- pure (Map.unions [trace (show s3 <> " " <> show f') s3, s2, s1], n { expr = FunCall f' a', info = apply s3 tv })
        pure (Map.unions [s3, s2, s1], n { expr = FunCall f' a'', info = apply s3 tv })
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

    -- catch all so the darn language server doesn't complain
    _ -> throwError (npos, TEUnsupportedExpr n)

topLevelTypecheck :: TypeEnv -> LimeNode -> Typechecker (TypeEnv, LimeNode)
topLevelTypecheck env@(tenv, venv) n@(LimeNode expr pos _) = case expr of
    Infix AssignType l@(LimeNode (Symbol s) _ _) r -> do
        rt <- typeLevelEval env r
        curTVar .= 0
        pure $ ((tenv, Map.insert s rt venv), n)
    Infix AssignValue a@(LimeNode (Symbol s) _ _) b -> do
        -- tv <- freshTVar
        -- -- TODO IMPORTANT add proper recursiveness checking
        -- (s1, b') <- typecheck (tenv, Map.insert s (Forall [] tv) venv) b
        (s1, b') <- typecheck env b
        let env'  = applyEnv s1 env
            t'    = generalize env' $ getType b'
        env'' <- case tenv !? s of
            Just x -> unify pos (schemeType x) (schemeType t') *> pure env
            Nothing -> pure $ (tenv, Map.insert s t' venv)
        curTVar .= 0
        pure $ (env'', n { expr = Infix AssignValue a b', info = getType b' })
    Data name tvs ms -> do
        tvs' <- traverse (const freshTVarI) tvs
        let tvs'' = map TVar tvs'
            tempEnv = foldl' insertTVs (Map.insert name (Forall tvs' (TRec name tvs'')) tenv, venv) $ zip tvs tvs''
            insertTVs (tenv, venv) (n,v) = (Map.insert n (Forall [] v) tenv, venv)
        ms' <- mapM (\(a, b) -> mapM (\x -> schemeType <$> typeLevelEval tempEnv x) b >>= pure . (a,)) ms
        let t = TADT name ms' tvs''
            env' = (Map.insert name (Forall tvs' t) tenv, venv)
            env'' = foldl' (\b (a, cs) -> (fst b, Map.insert a (Forall tvs' (foldl' (\t ti -> TLambda ti t) t $ reverse cs)) $ snd b)) env' ms'
        pure $ (env'', n { info = t })
    _ -> (\(_, b) -> (env, b)) <$> typecheck env n

infixType :: LimeType -> LimeType
infixType t = TLambda t $ TLambda t t

defInt :: LimeType
defInt = TNamed "Int" $ TPrim $ PInt 4 True

defaultTypes :: [(Text, Scheme)]
defaultTypes = 
    [ ("Int", Forall [] $ TNamed "Int" $ TPrim $ PInt 4 True),
      ("Float", Forall [] $ TNamed "Float" $ TPrim $ PFloat 4)
    ]

builtinFunctions :: Map Text Scheme
builtinFunctions = Map.fromList
    [ ("__add", Forall [] $ infixType defInt),
      ("__sub", Forall [] $ infixType defInt),
      ("__mul", Forall [] $ infixType defInt),
      ("__div", Forall [] $ infixType defInt)
    ]

defaultValues :: Map Text Scheme
defaultValues =
    builtinFunctions

defaultTypeEnv :: TypeEnv
defaultTypeEnv = (Map.fromList defaultTypes, defaultValues)

typecheckAllI :: [LimeNode] -> TypeEnv -> Typechecker (TypeEnv, [LimeNode])
typecheckAllI [] env = pure (env, [])
typecheckAllI (n:ns) env = do
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
    TVar i -> "p" <> T.pack (show i)
    TPrim p -> case p of
        PFloat _ -> "float"
        PInt _ _ -> "int"
    TADT _ _ _ -> "ADT"
    TRec _ _ -> "ADT"
    TNamed _ t -> printTypeGo t
    Unchecked -> "Unchecked"

printTypeEnv :: Map Text Scheme -> Text
printTypeEnv env = foldl' (\b (n, (Forall s t)) -> b <> n <> " :: " <> (if null s then "" else (T.concat $ map (\a -> "p" <> T.pack (show a) <> " ") s) <> "=> ") <>  printType t <> "\n") "" $ Map.toList env

type Counter = State Int

data Value
    = VInt Int
    | VArray LimeType [Value]
    | VFunc (Text, LimeType, LimeType) [Instruction]
    | VConstructorIndex Text
    deriving Show

data Instruction
    = IApply
    | IConst Value
    | IBlock LimeType [Instruction]
    | IGetVal Text
    | IGetFunc Int
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
    Symbol s -> pure [IGetVal s]
    -- Lambda l@[LimeNode (Symbol a) _ _] r@(LimeNode _ _ _) -> do
    FunCall f a -> do
        a' <- linearize a
        f' <- linearize f
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
                TADT _ _ _ -> [IDup, IGetVal "_adtType", IApply]
                _ -> []
        pure $ v' <> adtStuff <> [ISwitch ninfo cs']
    Let l@(LimeNode (Symbol a) _ _) r e -> do
        r' <- linearize r
        e' <- linearize e
        pure $ [IBlock (info e) (r' <> [ISetVal a] <> e')]
    Int v -> pure [IConst $ VInt v]
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
    IGetFunc i -> do
        r <- pushStack
        pure $ r <> " := _fn" <> T.pack (show i)
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
            let header = "func _fn" <> T.pack (show name) <> "() " <> printTypeGo t <> " {\n    "
            v <- T.intercalate "\n    " <$> (traverse linearizedToGo is)
            r <- popStack
            _ <- (assert . null) <$> gets _stack
            pure $ header <> v <> "\n    return " <> r <> ";\n}\n\n" <> nameDef
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
    deriving Show

data MonomorphizerState = MonomorphizerState
    { _monomorphizedFns :: Map (Text, LimeType) LimeNode
    , _reqTypes :: Set.Set (Text, LimeType)
    , _mFunctions :: (Map Text FType)
    , _mTypes :: (Map Text Scheme)
    , _curMFn :: Text
    }
    deriving Show

makeLenses ''MonomorphizerState

type Monomorphizer = State MonomorphizerState

monomorphizeFunCall :: LimeNode -> Map Int LimeType -> Monomorphizer (LimeNode, Maybe FType, LimeType, Map Int LimeType)
monomorphizeFunCall n@(LimeNode node _ ninfo) env = case node of
    FunCall f a -> do
        (f', n', ft, fnEnv) <- monomorphizeFunCall f env
        a' <- monomorphize env a

        let f la lr = pure $ (n { expr = FunCall f' a' }, n', lr, fnEnv')
                where fnEnv' = case la of
                        TVar v -> Map.insert v (apply env $ info a) fnEnv
                        _ -> fnEnv

        case ft of
            TLambda la lr -> f la lr
            _ -> f ft ft
    Symbol s -> do
        v <- gets _mFunctions
        cur <- gets _curMFn
        if s == cur then
            pure (n, Nothing, ninfo, Map.empty)
        else
            case v !? s of
                -- not sure about this one
                -- should probably apply
                Just mf -> case mf of
                    FTFn f -> pure (n { info = apply env ninfo }, Just mf, info f, Map.empty)
                    FTCon _ -> pure (n, Just mf, ninfo, Map.empty)
                Nothing -> case builtinFunctions !? s of
                    Just _ -> pure (n, Nothing, ninfo, Map.empty)
                    Nothing -> error $ "could not find function " <> T.unpack s <> " in env"
    _ -> monomorphize env n >>= \n' -> pure (n', Nothing, ninfo, Map.empty)

monomorphize :: Map Int LimeType -> LimeNode -> Monomorphizer LimeNode
monomorphize env n@(LimeNode node _ ninfo) = case node of
    FunCall f a -> do
        (n', r, _, fnEnv) <- monomorphizeFunCall n env
        case r of
            Just r' -> case r' of
                FTFn r'' -> monomorphizeFnTL r'' fnEnv
                FTCon c -> reqTypes %= Set.insert (c, ninfo)
            Nothing -> pure ()
        pure $ n' { info = apply env ninfo }    
    Symbol s -> do
        v <- gets _mFunctions
        cur <- gets _curMFn
        if s == cur then
            pure ()
        else
            case v !? s of
                Just mf -> case mf of
                    FTFn r'' -> monomorphizeFnTL r'' Map.empty
                    FTCon c -> reqTypes %= Set.insert (c, ninfo)
                Nothing -> pure ()
        pure $ n { info = apply env ninfo }
    Lambda l r@(LimeNode _ _ rinfo) -> do
        r' <- monomorphize env r
        l' <- traverse (monomorphize env) l
        pure $ n { expr = Lambda l' r', info = apply env ninfo }
    Prefix op r -> do
        r' <- monomorphize env r
        pure n { expr = Prefix op r' }
    Let l r e -> do
        -- TODO monomorphize lets
        -- doesn't work yet because needs pattern matching
        -- l' <- monomorphize env l
        -- r' <- monomorphize env r
        -- e' <- monomorphize env e
        -- pure n { expr = Let l' r' e' }
        pure n
    Case v cs -> do
        v' <- monomorphize env v
        cs' <- traverse (\(a, b) -> monomorphize env b >>= pure . (a,)) cs
        pure n { expr = Case v' cs', info = apply env ninfo }
    Discard -> pure n
    Int _ -> pure n
    Bool _ -> pure n
    String _ -> pure n
    List _ -> pure n

monomorphizeFnTL :: LimeNode -> Map Int LimeType -> Monomorphizer ()
monomorphizeFnTL n@(LimeNode node _ ninfo) env = case node of
    Infix AssignValue l@(LimeNode (Symbol s) _ _) r -> do
        exists <- gets (\t -> (s, apply env ninfo) `Map.member` _monomorphizedFns t)
        if exists
        then pure ()
        else do
            cur <- gets _curMFn
            curMFn .= s
            r' <- monomorphize env r
            monomorphizedFns %= Map.insert (s, apply env ninfo) (n { expr = Infix AssignValue l r', info = apply env ninfo })
            curMFn .= cur
    _ -> pure ()

collectNodeName :: LimeNode -> Maybe (Text, LimeNode)
collectNodeName n@(LimeNode node _ _) = case node of
    Infix AssignType (LimeNode (Symbol s) _ _) _ -> Just (s, n)
    Infix AssignValue (LimeNode (Symbol s) _ _) _ -> Just (s, n)
    -- data declarations aren't collected because they don't belong in the var env
    _ -> Nothing

collectNodeNames :: [LimeNode] -> Map Text FType
collectNodeNames ns = foldl' f Map.empty ns
    where f b n = case collectNodeName n of
            Just (l, r) -> Map.insert l (FTFn r) b
            Nothing -> b

parseLime :: String -> Text -> IO [LimeNode]
parseLime filename contents = let (Identity (parsed, s)) = runStateT (runParserT program filename contents) $ ISUnset 1
    in case parsed of
        Right p -> pure p
        Left e -> do
            putStrLn $ errorBundlePretty e 
            print s
            exitFailure

typecheckLime :: [LimeNode] -> Text -> IO (TypeEnv, [LimeNode])
typecheckLime ns fileContents = let Identity (r, _) = runStateT (runExceptT $ typecheckAll ns) (TypecheckerState 0)
    in case r of
        Left (pos, msg) -> do
            printPos pos fileContents msg
            putStr "\n"
            exitFailure
        Right res -> pure res

printValue :: Int -> Value -> Text
printValue i v = case v of
    VInt i -> "VInt " <> T.pack (show i)
    VArray _ a -> "Varray [" <> T.intercalate ", " (map (printValue 0) a) <> "]"
    VFunc (n,_,_) is -> "VFunc " <> n <> "\n" <> (T.intercalate "\n" $ map (printInstruction (i+4)) is)
    VConstructorIndex t -> "VConstructorIndex " <> t

printInstruction :: Int -> Instruction -> Text
printInstruction i inst = T.replicate i " " <> case inst of
    IApply -> "IApply"
    IConst v -> "IConst " <> printValue i v
    IBlock _ is -> "IBlock\n" <> (T.intercalate "\n" $ map (printInstruction (i+4)) is)
    IGetVal v -> "IGetVal " <> v
    IDup -> "IDup"
    IDrop -> "IDrop"
    IFlip -> "IFlip"
    IGetFunc f -> "IGetFunc" <> T.pack (show f)
    ISwitch _ cs -> "ISwitch\n" <> T.intercalate "\n" (map (\(l, r) -> T.replicate (i+4) " " <> printValue 0 l <> "\n" <> printInstructions (i+8) r) cs)
    ISetVal t -> "ISetVal " <> t
    IIndexADT n idx -> "IIndexADT " <> n <> " " <> T.pack (show idx)
    
printInstructions :: Int -> [Instruction] -> Text
printInstructions i v = T.intercalate "\n" $ map (printInstruction i) v

generateTypeName :: LimeType -> Text
generateTypeName = \case
    TLambda a r -> "L" <> generateTypeName a <> generateTypeName r
    TVar i -> "p" <> T.pack (show i)
    TPrim p -> case p of
        PFloat _ -> "_Float"
        PInt _ _ -> "_Int"
    -- add type applications to the end
    TADT n _ _ -> "_" <> n
    TNamed n _ -> "_" <> n
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
    (env, ns) <- typecheckLime simplified test

    let mappedNodes = collectNodeNames ns

    T.putStrLn $ T.intercalate "\n" $ fmap (\(a,b) -> let FTFn b' = b in a <> ": " <> printNode b') $ Map.toList $ mappedNodes


    T.putStrLn $ printTypeEnv $ fst env
    T.putStrLn $ printTypeEnv $ snd env

    case mappedNodes !? "main" of
        Nothing -> putStrLn "ERROR: no main function found"
        Just m -> let (_, s)    = runState (monomorphizeFnTL m' Map.empty) (MonomorphizerState Map.empty Set.empty envFns (fst env) "")
                      envFns    = (Map.fromList $ collectConstructors $ map schemeType $ Map.elems $ fst env) <> mappedNodes
                      (FTFn m') = m
                      mfns      = (\((a, b), c) -> (a, b, c)) <$> (Map.toList $ _monomorphizedFns s)
                      rtypes    = _reqTypes s
                in do
                let (r, s) = runState (traverse (\(_,_,a) -> linearizeTopLevel a) mfns) (LinearizerState [] [])
                let newFunctions = map (\(n,b,c) -> if n == "main" then ("_lmmain",b,c) else (n,b,c)) $ _functions s
                T.putStrLn $ T.intercalate "\n" (map (\(a,b,_) -> a <> generateTypeName b <> " :: " <> printType b) mfns)
                T.putStrLn ""
                -- print $ mfns
                print $ rtypes
                T.putStrLn ""
                -- putStrLn $ Data.List.intercalate "\n" (map (\(a,b,_) -> T.unpack a <> " :: " <> show b) mfns)
                T.putStrLn $ T.intercalate "\n\n" $ map (\(_,v,_) -> printInstructions 0 v) $ newFunctions
                -- print m

                header <- T.readFile "header.go"
                T.writeFile "out.go" $ header <> "\n" <> 
                    (T.concat $ map typeToGo $ map schemeType $ filter (\(Forall tvs _) -> null tvs) (Map.elems $ fst env)) <>
                    (T.concat $ map (typeToGo . snd) $ Set.elems rtypes) <>
                    (T.intercalate "\n\n" $ map (\(a, (d, b, c)) -> functionToGo a b c d) (zip [0..] newFunctions))
                putStrLn ""

                callCommand "go run out.go"
