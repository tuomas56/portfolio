module AME.Interpret.AST (
    Number,
    Value(..),
    Expression(..),
    Statement(..)
) where

-- This module contains a representation of all the possible values that a calculation could take
-- as well as all possible operations that can be performed.

import AME.Compute.Expr
import AME.Compute.Error
import AME.Compute.Matrix
import AME.Compute.Geometry
import AME.Compute.Statistics

-- The main number type. This consists of Double extended
-- with matrices of Doubles (MatrixExt Double) and then wrapped
-- in Compute to allow possible failure of arithmetic operations.
-- This lets us use the Num and Fractional implementations for
-- Num a => Num (Compute (MatrixExt a)) etc. that were defined in ame-compute
type Number = Compute (MatrixExt Double)

-- All the type of values that a computation could take.
data Value = Number Number
           | Shape (Shape Number)
           | Expr (Expr String Number)
           | Eqn (Equation String Number)
           | List [Value]
           | Tbl (Table String Number)
           | Point Number Number
           deriving (Show, Eq)

-- All the possible types of computation that can be represented
-- Most of these are direct wrappers around computations from ame-compute
-- And a few others such as Variable which represents a variable,
-- Value represents a constant value, and MakeVariable a computation to 
-- produce a fresh unbound variable.
data Expression = Variable String
                | MakeVariable String
                | Value Value
                | EquationOf Expression                             -- Begin AME.Compute.Geometry
                | IntersectionOf Expression Expression
                | LinePointGradient Expression Expression
                | LineInterceptGradient Expression Expression
                | LinePointPoint Expression Expression
                | MakePoint Expression Expression
                | CircleCenterRadius Expression Expression
                | CircleCenterPoint Expression Expression           -- End AME.Compute.Geometry
                | MakeEquation Expression Expression                -- Begin AME.Compute.Expr
                | MulE Expression Expression
                | SumE Expression Expression
                | DivE Expression Expression
                | PowE Expression Expression
                | SubE Expression Expression
                | SinE Expression
                | CosE Expression
                | TanE Expression
                | AbsE Expression
                | ExpE Expression
                | LogE Expression
                | ASinE Expression
                | ACosE Expression
                | ATanE Expression
                | SqrtE Expression                                  -- End AME.Compute.Expr
                | Substitute String Expression Expression           -- Begin AME.Compute.Simplify
                | Evaluate Expression                               -- End AME.Compute.Simplify
                | Solve Expression (Maybe String)                   -- Begin AME.Compute.Solver
                | SolveSimultaneous [Expression]                    -- End AME.Compute.Solver
                | Integral Expression                               -- Begin AME.Compute.Calculus.Integrals
                | DefiniteIntegral Expression Expression Expression -- End AME.Compute.Calculus.Integrals
                | Derivative Expression (Maybe String)              -- AME.Compute.Calculus.Derivatives
                | Mean String Expression                            -- Begin AME.Compute.Statistics
                | SampleMean String Expression
                | Mode String Expression
                | Median String Expression
                | Variance String Expression
                | SampleVariance String Expression
                | StdDev String Expression
                | SampleStdDev String Expression
                | ChiSquared String String Expression
                | PearsonCorrelation String String Expression
                | SpearmanCorrelation String String Expression      -- End AME.Compute.Statistics
                | DotProduct Expression Expression                  -- Begin AME.Compute.Matrix
                | Magnitude Expression                     
                | Determinant Expression
                | InverseMatrix Expression
                | AngleBetween Expression Expression
                | CharacteristicEquation Expression
                | Eigenvalues Expression
                | Eigenvectors Expression                           -- End AME.Compute.Matrix
                deriving (Show, Eq)

-- The main datatype which the interpreter works with:
-- Assign takes an expression and tries to assign it
-- to either a single variable or several variables. If it is the expression
-- is a list it unpacks the list so each variable has a different list element.
-- If it is not a list it expects there to be only one variable.
-- AssignArgs defines a new function with the given string as a name
-- and defines a new variable for each argument name string.
data Statement = Assign [String] Expression
               | AssignArgs String [String] Expression
               | Expression Expression
               deriving (Show, Eq)

