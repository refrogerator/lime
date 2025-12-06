module AST where

import Data.Text (Text)

type Name = Text

type Block = [Expr]

data Op
    = EqOp
    | OrOp
    deriving (Eq, Show)

data Expr
    = ListLit [Expr]
    | FloatLit Float
    | IntLit Int
    | StringLit Text
    | BoolLit Bool
    | Ident Text

    | Infix Op Expr Expr
    | Prefix Op Expr

    | SetType Expr Expr

    | Function Expr Expr
    | Call Expr Expr

    | Paren Expr

    | DoBlock Block
    | Let Expr Expr
    | Module Text
    deriving (Eq, Show)
