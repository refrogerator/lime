{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Process
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec hiding (State, Pos)
import Text.Megaparsec.Char
import qualified Control.Monad.Combinators.Expr as E 
import Data.Maybe
import qualified Data.List
import Data.Map.Strict hiding (foldr', map, drop, foldl')
import Data.Void (Void)
import Data.Functor
import Data.Foldable
import Control.Monad.State
import Control.Monad.Except
import Lens.Micro.Platform hiding (at)
import Control.Monad.Identity
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

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
    deriving Show

type Pos = (SourcePos, SourcePos)

data LimeNode = LimeNode
    { _expr :: LimeExpr
    , _pos  :: Pos
    , _info :: TypeInfo
    }
    deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

symbol :: Text -> Parser Text
symbol = L.symbol hspace

limeSymbol :: Parser LimeExpr
limeSymbol = Symbol . T.pack <$> lexeme (some letterChar)

limeInt :: Parser LimeExpr
limeInt = Int . read @Int <$> lexeme (some digitChar)

limeLet :: Parser LimeExpr
limeLet = do
    _ <- symbol "let"
    left <- limeNode
    _ <- symbol "="
    right <- limeNode
    pure $ Let left right

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

limeExprToNode :: Parser (LimeNode -> LimeNode -> LimeExpr) -> Parser (LimeNode -> LimeNode -> LimeNode)
limeExprToNode p = do
    r <- p
    pure (\a@(LimeNode _ (start, _) _) b@(LimeNode _ (_, end) _) -> LimeNode (r a b) (start, end) Unchecked)

limeTerm :: Parser LimeNode
limeTerm = limeTermToNode $ choice [limeInt, limeSymbol, lexeme limeLambda, lexeme limeLet] 

limeNode :: Parser LimeNode
limeNode = E.makeExprParser limeTerm table
    where
        table = 
          [ [ binary' "" FunCall
            ]
          , [ binary "+" Add
            , binary'' "-" Sub
            ] 
          , [ binary "*" Mul
            , binary "/" Div
            ]
          , [ binaryR "->" FunArrow
            ]
          , [ binary "="  AssignValue
            , binary "::" AssignType
            ]
          ]
        binary s op = E.InfixL $ limeExprToNode (Infix op <$ symbol s)
        binary'' s op = E.InfixL $ limeExprToNode (Infix op <$ try (symbol s <* notFollowedBy (symbol ">")))
        binary' s op = E.InfixL $ limeExprToNode (op <$ symbol s)
        binaryR s op = E.InfixR $ limeExprToNode (Infix op <$ symbol s)

program :: Parser [LimeNode]
program = limeNode `sepEndBy` void (some $ lexeme eol)

simplifyLambda :: [LimeNode] -> LimeNode -> Pos -> LimeNode
simplifyLambda [] r _ = r
simplifyLambda [a] r pos = LimeNode (Lambda [a] r) pos Unchecked
simplifyLambda (a:as) r pos =
    LimeNode (Lambda [a] $ simplifyLambda as r pos) pos Unchecked

simplifyLime :: LimeNode -> LimeNode
simplifyLime n@(LimeNode node pos _) = case node of
    Infix op l r ->
        n { _expr = Infix op (simplifyLime l) (simplifyLime r) }
    Lambda a r ->
        simplifyLambda a r pos
    FunCall f a ->
        n { _expr = FunCall (simplifyLime f) (simplifyLime a) }
    Int _ -> n
    Symbol _ -> n
    Bool _ -> n
    String _ -> n
    List _ -> n
    Prefix op r ->
        n { _expr = Prefix op $ simplifyLime r }
    Let l r ->
        n { _expr = Let (simplifyLime l) (simplifyLime r) }

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

-- toLisp :: LimeNode -> Text
-- toLisp node = case node of
--     Infix AssignType l r -> ""
--     Infix AssignValue (Symbol name) r -> "(define (" <> name <> ") " <> toLisp r <> ")"
--     Infix arithOp l r -> do
--         let a = toLisp l
--             b = toLisp r
--         case arithOp of
--             Add -> "(+ " <> a <> " " <> b <> ")"
--             Sub -> "(- " <> a <> " " <> b <> ")"
--             Mul -> "(* " <> a <> " " <> b <> ")"
--             Div -> "(/ " <> a <> " " <> b <> ")"
--     Float a -> T.pack $ show a
--     Symbol a -> a
--     FunCall l r -> "(" <> toLisp l <> " " <> toLisp r <> ")"
--     _ -> ""

data TypeInfo =
    -- arg ret
    FuncInfo TypeInfo TypeInfo
    -- byte size
    | FloatInfo Int Text
    -- byte size, signed
    | IntInfo Int Bool Text
    -- literals
    | StringLit
    | IntLit
    | FloatLit
    -- undetermined, but handled, only for variables
    | Undet
    -- no assigned type
    | Unchecked
    deriving (Show, Eq)

printType :: TypeInfo -> Text
printType = \case
    FuncInfo a r -> "(" <> printType a <> " -> " <> printType r <> ")"
    FloatInfo _ t -> t
    IntInfo _ _ t -> t
    StringLit -> "StringLit"
    IntLit -> "IntLit"
    FloatLit -> "FloatLit"
    Undet -> "Undet"
    Unchecked -> "Unchecked"

data CheckedNode = CheckedNode LimeNode TypeInfo
    deriving Show

type Scope = Map Text TypeInfo

data TypecheckerState = TypecheckerState
    { _locals  :: [Scope]
    , _globals :: Scope
    , _types   :: Scope
    }
    deriving Show

makeLenses ''TypecheckerState

type Typechecker a = ExceptT (Pos, String) (State TypecheckerState) a

addScope :: [(Text, TypeInfo)] -> Typechecker ()
addScope a = locals %= (fromList a:)

removeScope :: Typechecker ()
removeScope = locals %= drop 1

findVar :: Text -> Typechecker (Maybe TypeInfo)
findVar n = do
    s <- use locals
    g <- use globals
    pure $ case find (member n) s of
        Just a -> Just $ a ! n
        Nothing -> g !? n

findVarError :: Text -> Pos -> Typechecker TypeInfo
findVarError n pos = findVar n >>= \case
    Just a -> pure a
    Nothing -> throwError (pos, "variable \"" <> T.unpack n <> "\" does not exist")

findType :: Text -> Typechecker (Maybe TypeInfo)
findType n = do
    ts <- use types
    pure $ ts !? n

findTypeError :: Text -> Pos -> Typechecker TypeInfo
findTypeError n pos = findType n >>= \case
    Just a -> pure a
    Nothing -> throwError (pos, "type \"" <> T.unpack n <> "\" does not exist")

-- typeEq :: LimeNode -> LimeNode

-- typeError :: Pos -> Text -> Typechecker ()
-- typeError pos msg = do

-- checkRequirementsInternal :: [Text] -> TypeInfo -> Typechecker Bool
-- checkRequirementsInternal reqs t = 


data Side = L | R

checkTypeEqInternal :: TypeInfo -> TypeInfo -> Typechecker (Maybe TypeInfo)
checkTypeEqInternal a b
    | a == b = pure $ Just a 
    | a == Undet && b /= Undet = pure $ Just b
    | b == Undet && a /= Undet = pure $ Just a
    | otherwise = pure Nothing

-- requires manual var concretization
checkTypeEq :: TypeInfo -> TypeInfo -> Pos -> String -> Typechecker TypeInfo
checkTypeEq a b pos msg = checkTypeEqInternal a b >>= \case
    Just v -> pure v
    Nothing -> throwError (pos, msg)

updateVar :: Text -> TypeInfo -> Typechecker ()
updateVar n t = do
    s <- use locals
    case Data.List.findIndex (member n) s of
        Just a -> locals . ix a %= insert n t
        Nothing -> globals %= insert n t

concretizeUndetVar :: LimeNode -> TypeInfo -> Typechecker ()
concretizeUndetVar (LimeNode ne _ ni) t =
    if ni == Undet then case ne of
        Symbol s -> updateVar s t
        _ -> pure ()
    else pure ()


checkNodeTypeEq :: LimeNode -> LimeNode -> Pos -> Typechecker (LimeNode, LimeNode)
checkNodeTypeEq a@(LimeNode _ _ ai) b@(LimeNode _ _ bi) pos = checkTypeEqInternal ai bi >>= \case
    Just ti -> do
        concretizeUndetVar a ti
        concretizeUndetVar b ti
        pure (a { _info = ti }, b { _info = ti })
    Nothing -> throwError (pos, "types do not match")

infixTypeEq :: LimeNode -> Typechecker LimeNode
infixTypeEq n@(LimeNode (Infix op l r) pos _) = do
    lp <- typecheck l
    rp <- typecheck r
    (lpm, rpm) <- checkNodeTypeEq lp rp pos
    pure $ n { 
        _expr = Infix op lpm rpm,
        _info = _info lpm
        }

typeLevelEval :: LimeNode -> Typechecker LimeNode
typeLevelEval n@(LimeNode node pos _) = case node of
    Symbol s -> (\v -> n { _info = v }) <$> findTypeError s pos 
    Infix FunArrow l r -> do
        lt@(LimeNode _ _ li) <- typeLevelEval l
        rt@(LimeNode _ _ ri) <- typeLevelEval r
        pure $ n { _expr = Infix FunArrow lt rt, _info = FuncInfo li ri }
    _ -> throwError (pos, "unsupported type-level expression")

-- patternMatch :: LimeNode -> Typechecker (Maybe (TypeInfo, [TypeInfo]))
-- patternMatch (LimeNode lhs lpos _) = case lhs of
--         Symbol l -> findVar l >>= \case
--             Just a -> pure $ Just (a, [])
--             Nothing -> pure Nothing
--         -- add holes as a possibility
--         FunCall (LimeNode (Symbol ls) _ _) (LimeNode (Symbol rs) _ _) -> pure ()

typecheck :: LimeNode -> Typechecker LimeNode
typecheck n@(LimeNode node pos _) = case node of
    Infix AssignType ln@(LimeNode (Symbol l) lpos _) r -> do
        findVar l >>= \case
            Just _ -> throwError (lpos, "can't redefine variable \"" <> T.unpack l <> "\"")
            Nothing -> pure ()
        rt@(LimeNode _ _ t) <- typeLevelEval r
        globals %= insert l t
        pure $ n { _expr = Infix AssignType (ln { _info = t }) rt, _info = t }
    -- treated like symbol now but will be pattern match in the future
    Infix AssignValue nl@(LimeNode (Symbol l) _ _) r -> do
        rtn@(LimeNode _ _ t) <- typecheck r
        ti <- findVar l >>= \case
            Just a -> checkTypeEq a t pos "types do not match"
            Nothing -> do
                globals %= insert l t 
                pure t 
        -- need to update inner node
        pure $ n { _expr = Infix AssignValue nl rtn, _info = ti }
    Lambda [ln@(LimeNode (Symbol a) _ _)] r@(LimeNode _ rpos _) -> do
        addScope [(a, Undet)]
        rtn@(LimeNode _ _ t) <- typecheck r
        at <- findVarError a pos
        removeScope
        if at == Undet then throwError (rpos, "type of " <> T.unpack a <> " could not be determined") else pure ()
        pure $ n { _expr = Lambda [(ln { _info = at })] rtn, _info = FuncInfo at t }
    Infix Add _ _ -> infixTypeEq n
    Infix Sub _ _ -> infixTypeEq n
    Infix Mul _ _ -> infixTypeEq n
    Infix Div _ _ -> infixTypeEq n
    FunCall f a -> do
        ft@(LimeNode _ fpos fi) <- typecheck f
        at@(LimeNode _ apos ai) <- typecheck a
        case fi of
            FuncInfo arg ret -> do
                ti <- checkTypeEq arg ai apos $ "incorrect function argument, expected " <> show arg
                -- concretizeUndetVar at ti
                pure $ n { _expr = FunCall ft at, _info = ret }
            _ -> throwError (fpos, "cannot call non-function")
    -- return IntLit later
    Int _ -> pure $ n { _info = IntInfo 4 True "Int"}
    Symbol s -> (\v -> n { _info = v }) <$> findVarError s pos
        
    -- catch all so the darn language server doesn't complain
    _ -> throwError (pos, "unsupported expression " <> show n)

defaultTypes :: Scope
defaultTypes = fromList 
    [ ("Int", IntInfo 4 True "Int"),
      ("Float", FloatInfo 4 "Float")
    ]

typecheckAll :: [LimeNode] -> Typechecker [LimeNode]
typecheckAll = traverse typecheck

printPos :: Pos -> Text -> String -> IO ()
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
    putStrLn msg

printTypeGo :: TypeInfo -> Text
printTypeGo = \case
    FuncInfo a r -> "func (" <> printTypeGo a <> ") " <> printTypeGo r
    FloatInfo _ t -> "float"
    IntInfo _ _ t -> "int"
    StringLit -> "StringLit"
    IntLit -> "IntLit"
    FloatLit -> "FloatLit"
    Undet -> "Undet"
    Unchecked -> "Unchecked"

goBackend :: LimeNode -> Text
goBackend n@(LimeNode node pos info) = case node of
    Infix AssignType _ _ -> ""
    Infix AssignValue (LimeNode (Symbol "main") _ _) r -> "func main () {\n    fmt.Println(" <> goBackend r <> ")\n}"
    Infix AssignValue l r -> "var " <> goBackend l <> " = " <> goBackend r
    Lambda [ln@(LimeNode (Symbol a) _ _)] r -> "func (" <> a <> " " <> printTypeGo arg <> ") " <> printTypeGo ret <> " {\n" <> "    return " <> goBackend r <> "\n}"
        where (FuncInfo arg ret) = info
    Infix Add a b -> goBackend a <> " + " <> goBackend b
    Infix Sub a b -> goBackend a <> " - " <> goBackend b
    Infix Mul a b -> goBackend a <> " * " <> goBackend b
    Infix Div a b -> goBackend a <> " / " <> goBackend b
    FunCall f a -> goBackend f <> "(" <> goBackend a <> ")"
    Int v -> T.pack $ show v
    Symbol s -> s
    -- catch all so the darn language server doesn't complain
    _ -> "" --throwError (pos, "unsupported expression " <> show n)

main :: IO ()
main = do
    test <- T.readFile "test.lm"
    let parsed = runParser program "test.lm" test 
    print parsed
    putStr "\n"
    case parsed of
        Right p -> do
            -- runStateT (runExceptT $ for_ p eval) (InterpreterState Data.Map.Strict.empty []) >>= print
            -- print $ T.intercalate "\n" $ map toLisp p
            -- let simplified = map simplifyLime p
            -- traverse_ (T.putStrLn . printNode) simplified
            let Identity (r, s) = runStateT (runExceptT $ typecheckAll p) (TypecheckerState [] Data.Map.Strict.empty defaultTypes)
            case r of
                Left (pos, msg) -> do
                    printPos pos test msg
                    putStr "\n"
                    print s
                Right a -> do
                    T.writeFile "out.go" $ Data.Foldable.foldl' (\b a -> b <> "\n" <> goBackend a) "package main\n\nimport fmt \"fmt\"" a
                    callCommand "go run out.go"
        Left e -> putStrLn $ errorBundlePretty e
