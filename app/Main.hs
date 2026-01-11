{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Monad.Combinators.Expr as E 
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as L
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
    | Let LimeNode LimeNode
    -- name members
    | Data Text [(Text, [LimeNode])]
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
limeSymbol = Symbol . T.pack <$> lexeme (some (letterChar <|> char '_'))

limeInt :: Parser LimeExpr
limeInt = Int . read @Int <$> lexeme (some digitChar)

limeLet :: Parser LimeExpr
limeLet = do
    _ <- symbol "let"
    left <- limeNode
    _ <- symbol "="
    right <- limeNode
    pure $ Let left right

limeData :: Parser LimeExpr
limeData = do
    _ <- symbol "data"
    left <- T.pack <$> lexeme (some letterChar)
    _ <- symbol "="
    right <- ((,) <$> (T.pack <$> lexeme (some letterChar)) <*> (many typeLevelNode)) `sepBy` symbol "|"
    pure $ Data left right

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
    v <- dbg "node" limeNode
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
keyword = void $ choice ["of"]

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
    case o of
        ISUnset _ -> put ISInvalid
        _ -> put o
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

typeLevelNode :: Parser LimeNode
typeLevelNode = E.makeExprParser (limeTermToNode limeSymbol) table
    where
        table =
            [ [ binaryR "->" FunArrow ] ]
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
    Let l r ->
        n { expr = Let (simplifyLime l) (simplifyLime r) }
    Data _ _ -> n
    Case _ _ -> n
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
    Data n ns -> "data " <> n <> " = " <> T.intercalate " | " (map (\(a, b) -> a <> (T.intercalate " " (map printNode b))) ns)
    Case v cs -> "case " <> printNode v <> " of\n    " <> (T.intercalate "\n    " $ map (\(l, r) -> printNode l <> " -> " <> printNode r) cs)
    Prefix op r ->
        ""
    Let l r ->
        ""
    Discard -> "_"

data LimePrim
    -- size, signedness, name
    = PInt Int Bool
    -- size, name
    | PFloat Int
    deriving (Show, Eq)

data LimeType =
    -- arg ret
    TLambda LimeType LimeType
    -- type vars
    | TVar Int
    | TPrim LimePrim
    | TADT Text [(Text, [LimeType])]
    | TNamed Text LimeType
    -- no assigned type
    | Unchecked
    deriving (Show, Eq)

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
    TADT n ms -> n <> " (" <> T.intercalate " | " (map (\(n', ts) -> n' <> if null ts then "" else " " <> T.intercalate " " (map printType ts)) ms) <> ")"
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
    -- this seems dumb
    -- probably just need number of tvars instead of array of names
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
    Symbol s -> if isUpperCase $ T.head s 
        then case tenv !? s of
            Just a -> pure a
            Nothing -> throwError (pos, TETypeNotFound s)
        else freshTVar >>= \(TVar v) -> pure $ Forall [v] $ TVar v
    Infix FunArrow l r -> do
        (Forall lvs lt) <- typeLevelEval env l
        (Forall rvs rt) <- typeLevelEval env r
        pure $ Forall [] $ TLambda lt rt
    _ -> throwError (pos, TEUnsupportedTLExpr n)

freshTVar :: Typechecker LimeType
freshTVar = do
    n <- use curTVar
    curTVar %= (+1)
    pure $ TVar n

schemeType :: Scheme -> LimeType
schemeType (Forall _ t) = t

getType :: LimeNode -> LimeType
getType (LimeNode _ _ t) = t

apply :: Subst -> LimeType -> LimeType
apply s t@(TVar v) = Map.findWithDefault t v s
apply s (TLambda a r) = TLambda (apply s a) (apply s r)
apply _ t = t

applyS :: Subst -> Scheme -> Scheme
applyS s (Forall as t) = Forall as $ apply s t

applyEnv :: Subst -> TypeEnv -> TypeEnv
applyEnv s (tenv, venv) = (tenv, Map.map (applyS s) venv)

unify :: Pos -> LimeType -> LimeType -> Typechecker Subst
unify pos (TNamed _ a) (TNamed _ b) = unify pos a b
unify pos (TNamed _ a) b = unify pos a b
unify pos a (TNamed _ b) = unify pos a b
unify pos (TLambda l1 r1) (TLambda l2 r2) = do
    s1 <- unify pos l1 l2
    s2 <- unify pos (apply s1 r1) (apply s1 r2)
    pure $ Map.union s2 s1
unify pos (TVar a) t = bind pos a t
unify pos t (TVar a) = bind pos a t
unify _ (TPrim a) (TPrim b) | a == b = pure Map.empty
unify _ (TADT na _) (TADT nb _) | na == nb = pure Map.empty
unify pos t1 t2 = throwError $ (pos, TEMismatch t1 t2)

occurs :: Int -> LimeType -> Bool
occurs v = \case
    TLambda t1 t2 -> occurs v t1 && occurs v t2
    TVar v' -> v == v'
    TPrim _ -> False
    -- TODO fix occurs check for ADT
    TADT _ _ -> False
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
    TADT _ _ -> Set.empty
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
        (\v -> (env, n { info = v })) <$> findVarError env s npos
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
        pure (Map.unions [s3, s2, s1], n { expr = FunCall f' a', info = apply s3 tv })
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
                s3 <- unify (pos l) (info v') (info l')
                sn <- case cs' of
                        (c:_) -> do
                            s4 <- unify (pos r) (info $ snd c) (info r')
                            -- dunno if i even need this but i'm keeping it 4 now
                            s5 <- unify (pos l) (info $ fst c) (info l')
                            pure $ Map.unions [s1, s2, s3, s4, s5]
                        [] -> pure $ Map.union s1 s2
                let env'' = applyEnv sn env'
                pure ((Map.union so sn, env''), (l', r'):cs')
        ((s2, env''), cs') <- foldl' check (pure ((s1, env'), [])) cs
        -- this is crucial so that the type of v' can be determined
        let v'' = v' { info = apply s2 $ info v' }
        pure $ (s2, n { expr = Case v'' cs', info = info $ snd $ head cs'})
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
        (s1, b') <- typecheck env b
        let env'  = applyEnv s1 env
            t'    = generalize env' $ getType b'
        env'' <- case tenv !? s of
            Just x -> unify pos (schemeType x) (schemeType t') *> pure env
            Nothing -> pure $ (tenv, Map.insert s t' venv)
        curTVar .= 0
        pure $ (env'', n { expr = Infix AssignValue a b', info = getType b' })
    Data name ms -> do
        ms' <- mapM (\(a, b) -> mapM (\x -> schemeType <$> typeLevelEval env x) b >>= pure . (a,)) ms
        let t = TADT name ms'
            env' = (Map.insert name (Forall [] t) tenv, venv)
            env'' = foldl' (\b (a, cs) -> (fst b, Map.insert a (Forall [] (foldl' (\t ti -> TLambda ti t) t $ reverse cs)) $ snd b)) env' ms'
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

defaultValues :: [(Text, Scheme)]
defaultValues =
    [ ("__add", Forall [] $ infixType defInt),
      ("__sub", Forall [] $ infixType defInt),
      ("__mul", Forall [] $ infixType defInt),
      ("__div", Forall [] $ infixType defInt)
    ]

defaultTypeEnv :: TypeEnv
defaultTypeEnv = (Map.fromList defaultTypes, Map.fromList defaultValues)

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
    TADT _ _ -> "ADT"
    TNamed _ t -> printTypeGo t
    Unchecked -> "Unchecked"

data GBackendState = GBackendState
    { ctr :: Int
    , bubble :: Text
    }

type GBackend = State GBackendState

getTempVar :: GBackend Int
getTempVar = do
    v <- get
    put $ v { ctr = ctr v + 1 }
    pure $ ctr v

addBubble :: Text -> GBackend ()
addBubble t = modify (\s -> s { bubble = bubble s <> "\n" <> t })

popBubble :: GBackend Text
popBubble = gets bubble <* modify (\s -> s { bubble = "" })

-- goBackend :: LimeNode -> GBackend Text
-- goBackend n@(LimeNode node pos ninfo) = case node of
--     Data _ _ -> pure $ fst $ foldl' f ("", 0) ms
--         where f (b, c) a = (b <> "var _cni" <> fst a <> " = " <> T.pack (show c) <> "\n" <> goGenerateConstructor a <> "\n", c+1)
--               (TADT _ ms) = ninfo
--     Infix AssignType _ _ -> pure ""
--     Infix AssignValue (LimeNode (Symbol "main") _ _) r -> do
--         r' <- goBackend r
--         pure $ "func main () {\n    fmt.Println(" <> r' <> ")\n}"
--     Infix AssignValue l r -> do
--         l' <- goBackend l
--         r' <- goBackend r
--         pure $ "var " <> l' <> " = " <> r'
--     Lambda [ln@(LimeNode (Symbol a) _ arg)] r@(LimeNode _ _ ret)  -> do
--         s <- get
--         r' <- goBackend r
--         -- b <- bubble <$> get
--         b <- popBubble
--         -- restore bubble
--         modify (\s' -> s' { bubble = bubble s })
--         pure $ "func (" <> a <> " " <> printTypeGo arg <> ") " <> printTypeGo ret <> " {\n" <> b <> "\n    return " <> r' <> "\n}"
--     FunCall f a -> do
--         f' <- goBackend f 
--         a' <- goBackend a
--         pure $ f' <> "(" <> a' <> ")"
--     Int v -> pure $ T.pack $ show v
--     Symbol s -> if isUpperCase $ T.head s then pure $ "_cnv" <> s else pure s
--     Case v cs -> do
--         tv <- (("_tv" <>) . T.pack . show) <$> getTempVar
--         v' <- goBackend v
--         let v'' = case info v of
--                 TADT _ _ -> v' <> ".t"
--                 _ -> v'
--         r' <- foldl' (\b (al, ar) -> do
--             b' <- b
--             al' <- goPMBackend al
--             ar' <- goBackend ar
--             pure $ b' <> "\ncase " <> al' <> ":\n\t" <> tv <> " = " <> ar') (pure "") cs 
--         -- let (TLambda _ ret) = info
--         addBubble $ "var " <> tv <> " " <> printTypeGo ninfo <> "\n" <> "switch " <> v'' <> "{" <> r' <> "\n}"
--         pure tv
--     -- catch all so the darn language server doesn't complain
--     _ -> pure "" --throwError (pos, "unsupported expression " <> show n)

printTypeEnv :: Map Text Scheme -> Text
printTypeEnv env = foldl' (\b (n, t) -> b <> n <> " :: " <> printType (schemeType t) <> "\n") "" $ Map.toList env

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
    { _functions :: [([Instruction], LimeType)]
    , _curFn :: Int
    , _names :: [(Text, Int)]
    , _types :: [LimeType]
    }
    deriving Show

makeLenses ''LinearizerState

addFunction :: [Instruction] -> LimeType -> Linearizer Int
addFunction f t = do
    i <- gets _curFn
    curFn %= (+1)
    functions %= ((f, t):)
    pure i

addTopLevelFunction :: Text -> [Instruction] -> LimeType -> Linearizer ()
addTopLevelFunction n f t = do
    i <- addFunction f t
    names %= ((n, i):)

type Linearizer = State LinearizerState

goPMBackend :: LimeNode -> GBackend Text
goPMBackend n@(LimeNode node pos ninfo) = case node of
    Int v -> pure $ T.pack $ show v
    Symbol s -> if isUpperCase $ T.head s then pure $ "_cni" <> s else pure s
    FunCall f a -> goPMBackend f
    _ -> pure ""

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
    Int v -> pure [IConst $ VInt v]
    Case v@(LimeNode _ _ vinfo) cs -> do
        v' <- linearize v
        cs' <- traverse (\(l,r) -> do
            r' <- linearize r
            let (l', lr, _) = linearizePMCase l
            pure (l', [IFlip] <> lr <> [IDrop] <> r')) cs
        let adtStuff = case vinfo of
                TADT _ _ -> [IDup, IGetVal "_adtType", IApply]
                _ -> []
        pure $ v' <> adtStuff <> [ISwitch ninfo cs']
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

data CBackendData = CBackendData
    { _stack :: [Int]
    , _counter :: Int
    }
    deriving Show

makeLenses ''CBackendData

type CBackend = State CBackendData

toCVar :: Int -> Text
toCVar i = "_" <> T.pack (show i)

popStack :: CBackend Text
popStack = do
    s <- gets $ head . _stack
    stack %= tail
    pure $ toCVar s

peekStack :: CBackend Text
peekStack = gets $ toCVar . head . _stack

pushStack :: CBackend Text
pushStack = do
    c <- gets _counter
    counter %= (+1)
    stack %= (c:)
    pure $ toCVar c

valueToGo :: Value -> CBackend Text
valueToGo v = case v of
    VInt i -> pure $ T.pack (show i)
    VArray t v -> do
        v' <- (T.intercalate ", " <$> (traverse valueToGo v))
        pure $ "[]" <> printTypeGo t <> "{" <> v'
    VFunc (n, t, ret) is -> do
        is' <- (T.intercalate "\n    " <$> (traverse linearizedToC is))
        r' <- popStack
        pure $ "func (_" <> n <> " " <> printTypeGo t <> ") " <> printTypeGo ret <> " {\n    " <> is' <> "\n    return " <> r' <> "\n    }"
    VConstructorIndex t -> do
        pure $ "_cni" <> t

linearizedToC :: Instruction -> CBackend Text
linearizedToC = \case
    IApply -> do
        f <- popStack
        a <- popStack
        r <- pushStack
        pure $ r <> " := " <> f <> "(" <> a <> ")"
    IConst v -> do
        r <- pushStack
        v' <- valueToGo v
        pure $ r <> " := " <> v'
    IGetVal v -> do
        r <- pushStack
        pure $ r <> " := " <> if isUpperCase $ T.head v then "_cnv" <> v else "_" <> v
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
                r' <- T.intercalate "\n    " <$> (traverse linearizedToC r)
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

functionToC :: Int -> [Instruction] -> LimeType -> Text
functionToC name is t = evalState f (CBackendData [] 0) 
    where f = do
            let header = "func _fn" <> T.pack (show name) <> "() " <> printTypeGo t <> " {\n    "
            v <- T.intercalate "\n    " <$> (traverse linearizedToC is)
            r <- popStack
            pure $ header <> v <> "\n    return " <> r <> ";\n}"

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
          recvCnvType [t] = TLambda t $ TADT "" []
          recvCnvType (t:ts) = TLambda t $ recvCnvType ts
          recvCnvType [] = Unchecked

typeToGo :: Scheme -> Text
typeToGo (Forall _ t) = case t of
    TADT _ ms -> (fst $ foldl' f ("", 0) ms) <> "\n"
        where f (b, c) a = (b <> "var _cni" <> fst a <> " = " <> T.pack (show c) <> "\n" <> goGenerateConstructor a <> "\n", c+1)
    _ -> ""

data MonomorphizerState = MonomorphizerState
    { _monomorphizedFns :: [(Maybe Text, LimeType, LimeNode)]
    , _reqTypes :: [(Text, [LimeType])]
    , _mFunctions :: (Map Text LimeNode)
    }
    deriving Show

makeLenses ''MonomorphizerState

type Monomorphizer = State MonomorphizerState

monomorphizeFunCall :: LimeNode -> Map Int LimeType -> Monomorphizer (LimeNode, Maybe LimeNode, LimeType, Map Int LimeType)
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
        case v !? s of
            -- not sure about this one
            -- should probably apply
            Just f -> monomorphize env n >>= \n' -> pure (n', Just f, info f, Map.empty)
            Nothing -> error "could not find function in env"
    _ -> monomorphize env n >>= \n' -> pure (n', Nothing, ninfo, Map.empty)

monomorphize :: Map Int LimeType -> LimeNode -> Monomorphizer LimeNode
monomorphize env n@(LimeNode node _ ninfo) = case node of
    FunCall f a -> do
        (n', r, _, fnEnv) <- monomorphizeFunCall n env
        case r of
            Just r' -> monomorphizeFnTL r' fnEnv
            Nothing -> pure ()
        pure $ n' { info = apply env ninfo }    
    Symbol s -> pure $ n { info = apply env ninfo }
    Lambda l r@(LimeNode _ _ rinfo) -> do
        r' <- monomorphize env r
        l' <- traverse (monomorphize env) l
        pure $ n { expr = Lambda l' r', info = apply env ninfo }
    Int v -> pure n

monomorphizeFnTL :: LimeNode -> Map Int LimeType -> Monomorphizer ()
monomorphizeFnTL n@(LimeNode node _ ninfo) env = case node of
    Infix AssignValue l@(LimeNode (Symbol s) _ _) r -> do
        r' <- monomorphize env r
        monomorphizedFns %= ((Just s, apply env ninfo, n { expr = Infix AssignValue l r', info = apply env ninfo }):)

collectNodeName :: LimeNode -> Maybe (Text, LimeNode)
collectNodeName n@(LimeNode node _ _) = case node of
    Infix AssignType (LimeNode (Symbol s) _ _) _ -> Just (s, n)
    Infix AssignValue (LimeNode (Symbol s) _ _) _ -> Just (s, n)
    Data name _ -> Just (name, n)
    _ -> Nothing

collectNodeNames :: [LimeNode] -> Map Text LimeNode
collectNodeNames ns = foldl' f Map.empty ns
    where f b n = case collectNodeName n of
            Just (l, r) -> Map.insert l r b
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

    T.putStrLn $ T.intercalate "\n" $ fmap (\(a,b) -> a <> ": " <> printNode b) $ Map.toList $ mappedNodes


    T.putStrLn $ printTypeEnv $ fst env
    T.putStrLn $ printTypeEnv $ snd env


    case mappedNodes !? "main" of
        Nothing -> putStrLn "ERROR: no main function found"
        Just m -> let (_, s) = runState (monomorphizeFnTL m Map.empty) (MonomorphizerState [] [] mappedNodes)
            in let mfns = _monomorphizedFns s
            in do
                let (r, s) = runState (traverse (\(_,_,a) -> linearizeTopLevel a) mfns) (LinearizerState [] 0 [] [])
                let newNames = map (\(a,b) -> if a == "main" then ("_lmmain",b) else (a,b)) $ _names s
                T.putStrLn $ T.intercalate "\n\n" $ map (\(v,_) -> printInstructions 0 v) $ _functions s

                header <- T.readFile "header.go"
                T.writeFile "out.go" $ header <> "\n" <> 
                    (T.intercalate "" $ map typeToGo (Map.elems $ fst env)) <>
                    (T.intercalate "\n\n" $ map (\(a, (b, c)) -> functionToC a b c) (zip [0..] (_functions s))) <> "\n\n" <>
                    (T.intercalate "\n\n" $ map (\(a, b) -> "var _" <> a <> " = _fn" <> T.pack (show (_curFn s - b - 1)) <> "()") $ newNames)

                putStrLn ""

                callCommand "go run out.go"


    
    -- case _functions s !? "main" of
    --     Nothing -> putStrLn "ERROR: no main function found"
    --     Just m -> 
    --         case snd env !? "main" of
    --             Nothing -> putStrLn "ERROR: no main function found"
    --             Just t -> do
    --                 header <- T.readFile "header.d"
    --                 T.writeFile "out.d" $ header <> "\n" <> functionToC "_hsmain" m t
    -- case snd env !? "main" of
    --     Nothing -> putStrLn "ERROR: no main function found"
    --     Just m -> do
    --         pure ()
    --         header <- T.readFile "header.go"
    --         T.writeFile "out.go" $ evalState (foldl' (\b a -> do
    --             b' <- b
    --             a' <- goBackend a
    --             pure $ b' <> a' <> "\n") (pure header) ns) (GBackendState 0 "")
    --         callCommand "go run out.go"
