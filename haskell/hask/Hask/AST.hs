module Hask.AST where

import Control.Monad.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

data HaskError = NameError String
               | TypeError String
               | ArithmeticError String
               | CaseError String
               | SyntaxError String
               deriving (Show, Eq)
               
type FuncEnv = [(String, Value)]
type ProgEnv = [(String, Decl)]
    
type Eval a = ExceptT HaskError (StateT (FuncEnv, ProgEnv) IO) a
type Prog a = StateT ProgEnv Identity a

newtype Name = Name String deriving (Show, Eq)
newtype ArgName = ArgName String deriving (Show, Eq)
    
data Pattern = NilPattern
             | ConsPattern Pattern Pattern
             | IdentPattern Name
             | ConstantPattern Value
             deriving (Show, Eq)
    
data Expr = Arg ArgName
          | NumLiteral Int
          | Call Expr Expr
          | LetExpr Expr Name Expr
          | LambdaExpr ArgName Expr
          | QuoteExpr Value
          | Brackets Expr
          | CaseExpr Expr [(Pattern, Expr)]
          deriving (Show, Eq)
    
data Value = Number Int 
           | Function (Value -> Expr -> Eval Value)
           | List [Value]

instance Eq Value where
    (Number a) == (Number b) = a == b
    (List a) == (List b) = a == b
    _ == _ = False

instance Show Value where
    show (Number n) = show n
    show (List a) = show a
    show (Function _) = "<closure>"
    
data Decl = ConstantDecl Name Expr
          deriving (Show, Eq)

newtype Program = Program [Decl] deriving (Show, Eq)
