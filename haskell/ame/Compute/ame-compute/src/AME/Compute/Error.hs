{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module AME.Compute.Error (
    NumericalError(..),
    Except.throwError,
    Except.catchError,
    runCompute,
    Compute,
    IsIntegral,
    isIntegral,
    asInt
) where

import qualified Control.Monad.Except as Except
import Control.Monad

-- A module providing a type for general numerical errors, 
-- used for all functions where they could occur.

-- A type that can tell if it is an integral value, used for the const type in Expr var const
class IsIntegral a where
    isIntegral :: a -> Bool
    asInt :: a -> Maybe Int

instance IsIntegral Double where
    isIntegral a = abs (a - (fromIntegral $ truncate a)) < 0.000001
    asInt a = Just $ round a

instance IsIntegral Float where
    isIntegral a = abs (a - (fromIntegral $ truncate a)) < 0.000001
    asInt a = Just $ round a

--obvious implementations of IsIntegral for our two basic floating types

data NumericalError = DivisionByZero -- obvious
                    | UnexpectedFreeVariable -- when trying to evaluate, we had variables
                    | MatrixHasWrongDimensions Int Int Int Int -- when trying to multiply two matrices
                    | MatrixShouldBeSquare Int Int -- when trying to compute determinants, eigenvalues etc.
                    | MatrixNotInvertible -- obvious
                    | MatrixOperationNotSupported String -- when a trig or log function is called on a matrix
                    | IntersectsEverywhere -- when trying to find the intersection of two shapes which are the same
                    | ShouldBeLinear -- when trying to solve simultaneous equations
                    | WrongNumberOfEquations Int Int -- when trying to solve simultaneous equations
                    | NoPartialIntegrals -- when trying to integrate and there is more than one free variable.
                    deriving (Eq, Show, Ord)

-- A transformer that adds exception handling to a type.
type Compute t = Except.Except NumericalError t

-- Evaluate a Compute expression.
runCompute :: Compute t -> Either NumericalError t
runCompute = Except.runExcept
