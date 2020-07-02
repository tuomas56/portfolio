{-# LANGUAGE FlexibleInstances #-}

module AME.Compute.Expr (
    Expr(..),
    Equation(..),
    (=:=),
    X,
    x
) where

import AME.Compute.Error
 
--  This module contains a generalized expression class, 
--  that represents elementary expressions of two parameters: 
--      * A type of variables
--      * A field of constants
--  This is a portable representation used for all operations in ame-compute.

data Expr var const = Var var
                    | Const const
                    | Sum [Expr var const]
                    | Mul [Expr var const]
                    | Div (Expr var const) (Expr var const)
                    | Pow (Expr var const) (Expr var const)
                    | Exp (Expr var const)
                    | Log (Expr var const)
                    | Sin (Expr var const)
                    | Cos (Expr var const)
                    | Tan (Expr var const)
                    | ASin (Expr var const)
                    | ACos (Expr var const)
                    | ATan (Expr var const)
                    | Abs (Expr var const)
                    deriving (Show, Read, Eq, Ord)

data Equation var const = Equation (Expr var const) (Expr var const) deriving (Eq, Show, Read, Ord)
--Equation is an equation with       a lhs        and     a rhs

-- (to make constructing equations a little prettier)
(=:=) :: Expr var const -> Expr var const -> Equation var const
(=:=) = Equation
infixr 2 =:=

-- The below classes are from the Haskell prelude and are implemented for 
-- the sake of convenience for the rest of this library, as they will probably not be used by
-- the next stages of the application.

-- If the constant field supports addition, subtraction, multiplication, then Expr does too.
instance Num const => Num (Expr var const) where
    fromInteger = Const . fromInteger 
    --Integers become constant integers.
    a + b = Sum [a, b]
    negate a = Mul [(Const $ negate 1), a]
    a * b = Mul [a, b]
    abs = Abs
    signum = error "Expr does not support signum!" 
    -- If we use signum somethings gone horribly wrong, so crash the program with an error.

-- If the constant field supports division, then Expr does too
instance Fractional const => Fractional (Expr var const) where
    fromRational = Const . fromRational
    --Rationals become constant rationals
    (/) = Div

-- If the constant field supports floating point operations, Expr does too
instance Floating const => Floating (Expr var const) where
    pi = Const pi
    exp = Exp
    log = Log
    (**) = Pow
    sin = Sin
    cos = Cos
    tan = Tan
    asin = ASin
    acos = ACos
    atan = ATan
    sinh = error "Hyperbolic functions and their inverses are not supported!"
    cosh = error "Hyperbolic functions and their inverses are not supported!"
    tanh = error "Hyperbolic functions and their inverses are not supported!"
    asinh = error "Hyperbolic functions and their inverses are not supported!"
    acosh = error "Hyperbolic functions and their inverses are not supported!"
    atanh = error "Hyperbolic functions and their inverses are not supported!"
    -- If we use this, something has gone horribly wrong.

data X = X deriving (Show, Eq, Ord)

x :: Expr X const
x = Var X
