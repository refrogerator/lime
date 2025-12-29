{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec
import qualified Control.Monad.Combinators.Expr as E 
import qualified Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as L
import System.Process hiding (env)
import Data.Text (Text)
import Text.Megaparsec hiding (State, Pos, empty)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Data.Maybe
import Data.Map.Strict (Map, (!?))
import Data.Void (Void)
import Data.Functor
import Data.Foldable
import Data.Traversable
import Control.Monad.State
import Control.Monad.Except
import Lens.Micro.Platform hiding (at)
import Control.Monad.Identity

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
    | Data Text [Text]
    -- value cases
    | Case LimeNode [(LimeNode, LimeNode)]
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
    right <- (T.pack <$> lexeme (some letterChar)) `sepBy` symbol "|"
    pure $ Data left right

caseBlock :: Parser [(LimeNode, LimeNode)]
caseBlock = (some (p <* lexeme eol))
    where p = do
            -- l <- dbg "node" limeNode
            il <- unPos <$> L.indentLevel
            l <- limeNode
            indentBracket $ do
                _ <- symbol "->"
                r <- limeNode
                pure (l, r)

limeCase :: Parser LimeExpr
limeCase = do
    l <- unPos <$> L.indentLevel
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

limeExprToNode :: Parser (LimeNode -> LimeNode -> LimeExpr) -> Parser (LimeNode -> LimeNode -> LimeNode)
limeExprToNode p = do
    r <- p
    pure (\a@(LimeNode _ (start, _) _) b@(LimeNode _ (_, end) _) -> LimeNode (r a b) (start, end) Unchecked)

keyword :: Parser ()
keyword = void $ choice ["of"]

limeTerm :: Parser LimeNode
limeTerm = limeTermToNode $ do
    notFollowedBy keyword
    choice [limeCase, limeInt, lexeme limeLambda, lexeme limeLet, limeSymbol] 

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
            op <- choice $ lexeme <$>
                [ AssignType <$ "::" 
                , AssignValue <$ "=" ]
            r <- limeNode
            pure $ nodesToInfix op l r

typeLevelNode :: Parser LimeNode
typeLevelNode = E.makeExprParser limeTerm table
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
          , [ binaryF "+" Add
            , binaryF' "-" Sub
            ] 
          , [ binaryF "*" Mul
            , binaryF "/" Div
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

infixToFun :: Text -> LimeNode -> LimeNode -> Pos -> LimeNode
infixToFun name l r pos = newNode (FunCall (newNode (FunCall (newNode (Symbol name) pos) $ simplifyLime l) pos) $ simplifyLime r) pos

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
    Data n ns -> "data " <> n <> " = " <> T.intercalate " | " ns
    Case v cs -> "case " <> printNode v <> " of\n    " <> (T.intercalate "\n    " $ map (\(l, r) -> printNode l <> " -> " <> printNode r) cs)
    Prefix op r ->
        ""
    Let l r ->
        ""

data Value =
    VFloat Float
    | VBool Bool
    | VString Text
    | VSymbol Text
    | VList [LimeNode]
    | VFunc LimeNode
    | VNone
    deriving (Show)

data LimePrim
    -- size, signedness, name
    = PInt Int Bool Text
    -- size, name
    | PFloat Int Text
    deriving (Show, Eq)

data LimeType =
    -- arg ret
    TLambda LimeType LimeType
    -- type vars
    | TVar Int
    | TPrim LimePrim
    | TADT Text [Text]
    -- no assigned type
    | Unchecked
    deriving (Show, Eq)

printType :: LimeType -> Text
printType = \case
    TLambda a r -> "(" <> printType a <> " -> " <> printType r <> ")"
    TPrim p -> case p of
        PInt _ _ n -> n
        PFloat _ n -> n
    TVar i -> "p" <> T.pack (show i)
    TADT n _ -> n
    Unchecked -> "Unchecked"

data CheckedNode = CheckedNode LimeNode LimeType
    deriving Show

data Scheme = Forall [Int] LimeType
    deriving Show

type TypeEnv = Map Text Scheme
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
    | TEUnsupportedExpr LimeNode
    | TEUnsupportedPMExpr LimeNode
    | TEUnsupportedTLExpr LimeNode
    deriving Show

type Typechecker a = ExceptT (Pos, TypecheckError) (State TypecheckerState) a

instantiate :: Scheme -> Typechecker LimeType
instantiate (Forall as t) = do
    -- this seems dumb
    as' <- mapM (const freshTVar) as
    let s = Map.fromList $ zip as as'
    pure $ apply s t

findVarEnv :: TypeEnv -> Text -> Typechecker (Maybe LimeType)
findVarEnv env n = case env !? n of
    Just a -> instantiate a >>= \t -> pure $ Just t
    Nothing -> pure Nothing

findVarError :: TypeEnv -> Text -> Pos -> Typechecker LimeType
findVarError env n pos = findVarEnv env n >>= \case
    Just a -> pure a
    Nothing -> throwError (pos, TEVarNotFound n)

typeLevelEval :: TypeEnv -> LimeNode -> Typechecker Scheme
typeLevelEval env n@(LimeNode node pos _) = case node of
    Symbol s -> case env !? s of
        Just a -> pure a
        Nothing -> throwError (pos, TEVarNotFound s)
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
applyEnv s env = Map.map (applyS s) env

unify :: Pos -> LimeType -> LimeType -> Typechecker Subst
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
    TADT n ns -> False
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
    TADT n ns -> Set.empty
    Unchecked -> Set.empty

freeTypeVarsS :: Scheme -> Set.Set Int
freeTypeVarsS (Forall vs t) = freeTypeVars t `Set.difference` Set.fromList vs

freeTypeVarsEnv :: TypeEnv -> Set.Set Int
freeTypeVarsEnv env = foldl' (flip $ Set.union . freeTypeVarsS) Set.empty $ Map.elems env

generalize :: TypeEnv -> LimeType -> Scheme
generalize env t = Forall as t
    where as = Set.toList $ freeTypeVars t `Set.difference` freeTypeVarsEnv env

typecheckPatternMatch :: TypeEnv -> LimeNode -> Typechecker (Subst, LimeNode)
typecheckPatternMatch env n@(LimeNode node npos _) = case node of
    Int _ -> pure $ (Map.empty, n { info = defInt })
    Symbol s -> 
        (\v -> (Map.empty, n { info = v })) <$> findVarError env s npos
    _ -> throwError (npos, TEUnsupportedPMExpr n)


typecheck :: TypeEnv -> LimeNode -> Typechecker (Subst, LimeNode)
typecheck env n@(LimeNode node npos _) = case node of
    Symbol s -> do
        (\v -> (Map.empty, n { info = v })) <$> findVarError env s npos
    Lambda l@[LimeNode (Symbol a) _ _] r@(LimeNode _ _ _) -> do
        tv <- freshTVar

        let env' = Map.insert a (Forall [] tv) env
        (s1, r'@(LimeNode _ _ t1)) <- typecheck env' r
        
        pure (s1, n { expr = Lambda l r', info = apply s1 (TLambda tv t1) })
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
                (s1, r') <- typecheck env r
                (s2, l') <- typecheckPatternMatch env l
                s3 <- unify (pos l) (info v') (info l')
                sn <- case cs' of
                        (c:_) -> do
                            s4 <- unify (pos r) (info $ snd c) (info r')
                            -- dunno if i even need this but i'm keeping it 4 now
                            s5 <- unify (pos l) (info $ fst c) (info l')
                            pure $ Map.unions [s1, s2, s3, s4, s5]
                        [] -> pure $ Map.union s1 s2
                let env' = applyEnv sn env
                pure ((Map.union so sn, env'), (l', r'):cs')
        ((s2, env''), cs') <- foldl' check (pure ((s1, env'), [])) cs
        pure $ (s2, n { expr = Case v' cs', info = info $ snd $ head cs'})
    Int _ -> pure $ (Map.empty, n { info = defInt })

    -- catch all so the darn language server doesn't complain
    _ -> throwError (npos, TEUnsupportedExpr n)

topLevelTypecheck :: TypeEnv -> LimeNode -> Typechecker (TypeEnv, LimeNode)
topLevelTypecheck env n@(LimeNode expr pos _) = case expr of
    Infix AssignType l@(LimeNode (Symbol s) _ _) r -> do
        rt <- typeLevelEval env r
        curTVar .= 0
        pure $ (Map.insert s rt env, n)
    Infix AssignValue a@(LimeNode (Symbol s) _ _) b -> do
        (s1, b') <- typecheck env b
        let env'  = applyEnv s1 env
            t'    = generalize env' $ getType b'
        env'' <- case env !? s of
            Just x -> unify pos (schemeType x) (schemeType t') *> pure env
            Nothing -> pure $ Map.insert s t' env
        curTVar .= 0
        pure $ (env'', n { expr = Infix AssignValue a b', info = getType b' })
    Data name ms ->
        let t = TADT name ms
            env' = Map.insert name (Forall [] t) env
            env'' = foldl' (\b a -> Map.insert a (Forall [] t) b) env' ms
        in pure $ (env'', n { info = t })
    _ -> (\(_, b) -> (env, b)) <$> typecheck env n

infixType :: LimeType -> LimeType
infixType t = TLambda t $ TLambda t t

defInt :: LimeType
defInt = TPrim $ PInt 4 True "Int"

defaultTypes :: TypeEnv
defaultTypes = Map.fromList 
    [ ("Int", Forall [] $ TPrim $ PInt 4 True "Int"),
      ("Float", Forall [] $ TPrim $ PFloat 4 "Float"),
      ("__add", Forall [] $ infixType defInt),
      ("__sub", Forall [] $ infixType defInt),
      ("__mul", Forall [] $ infixType defInt),
      ("__div", Forall [] $ infixType defInt)
    ]

typecheckAllI :: [LimeNode] -> TypeEnv -> Typechecker (TypeEnv, [LimeNode])
typecheckAllI [] env = pure (env, [])
typecheckAllI (n:ns) env = do
    (env', n') <- topLevelTypecheck env n
    (env'', n2) <- typecheckAllI ns env'
    pure $ (env'', n':n2)

typecheckAll :: [LimeNode] -> Typechecker (TypeEnv, [LimeNode])
typecheckAll ns = typecheckAllI ns defaultTypes

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
        TEUnsupportedExpr n -> putStrLn $ "unsupported expr: " <> show n
        TEUnsupportedPMExpr n -> putStrLn $ "unsupported pattern matched expr: " <> show n
        TEUnsupportedTLExpr n -> putStrLn $ "unsupported type level expr: " <> show n 

printTypeGo :: LimeType -> Text
printTypeGo = \case
    TLambda a r -> "func (" <> printTypeGo a <> ") " <> printTypeGo r
    TVar i -> "p" <> T.pack (show i)
    TPrim p -> case p of
        PFloat _ _ -> "float"
        PInt _ _ _ -> "int"
    TADT _ _ -> "int"
    Unchecked -> "Unchecked"

goBackend :: LimeNode -> Text
goBackend n@(LimeNode node pos info) = case node of
    Data _ ms -> fst $ foldl' f ("", 0) ms
        where f (b, c) a = (b <> "var " <> a <> " = " <> T.pack (show c) <> "\n", c+1)
    Infix AssignType _ _ -> ""
    Infix AssignValue (LimeNode (Symbol "main") _ _) r -> "func main () {\n    fmt.Println(" <> goBackend r <> ")\n}"
    Infix AssignValue l r -> "var " <> goBackend l <> " = " <> goBackend r
    Lambda [ln@(LimeNode (Symbol a) _ _)] r -> "func (" <> a <> " " <> printTypeGo arg <> ") " <> printTypeGo ret <> " {\n" <> "    return " <> goBackend r <> "\n}"
        where (TLambda arg ret) = info
    FunCall f a -> goBackend f <> "(" <> goBackend a <> ")"
    Int v -> T.pack $ show v
    Symbol s -> s
    Case v cs -> "switch " <> goBackend v <> "{" <> foldl' (\b (al, ar) -> b <> "\ncase " <> goBackend al <> ":\n\t" <> goBackend ar) "" cs <> "\n}"
    -- catch all so the darn language server doesn't complain
    _ -> "" --throwError (pos, "unsupported expression " <> show n)

-- printScheme :: Scheme -> Text
-- printScheme (Forall vs t) = foldl' (\b a -> b <> (T.pack $ show a) <> " ") "" vs <> "=> " <> printType t

printTypeEnv :: TypeEnv -> Text
printTypeEnv env = foldl' (\b (n, t) -> b <> n <> " :: " <> printType (schemeType t) <> "\n") "" $ Map.toList env

main :: IO ()
main = do
    test <- T.readFile "test.lm"
    let (Identity (parsed, s)) = runStateT (runParserT program "test.lm" test) $ ISUnset 1
    -- let parsed = runParser program "test.lm" test
    -- print parsed
    case parsed of
        Right p -> do
            -- runStateT (runExceptT $ for_ p eval) (InterpreterState Data.Map.Strict.empty []) >>= print
            -- print $ T.intercalate "\n" $ map toLisp p
            let simplified = map simplifyLime p
            traverse_ (T.putStrLn . printNode) p
            putStrLn ""
            let Identity (r, s) = runStateT (runExceptT $ typecheckAll simplified) (TypecheckerState 0)
            case r of
                Left (pos, msg) -> do
                    printPos pos test msg
                    putStr "\n"
                Right (env, ns) -> do
                    T.putStrLn $ printTypeEnv env
                    case env !? "main" of
                        Nothing -> putStrLn "ERROR: no main function found"
                        Just m -> do
                            pure ()
                            -- header <- T.readFile "header.go"
                            -- T.writeFile "out.go" $ foldl' (\b a -> b <> goBackend a <> "\n") header ns
                            -- callCommand "go run out.go"
        Left e -> do
            putStrLn $ errorBundlePretty e
            print s
