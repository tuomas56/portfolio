{-# LANGUAGE ConstraintKinds #-}

module AME.Compute.Solver (
  solve,
  solveSimultaneous,
  solveExactPoly,
  isPolynomial,
  findRoots,
  polyCoeffs,
  Soluble
) where

import AME.Compute.Expr
import AME.Compute.Error
import AME.Compute.Simplify
import AME.Compute.Matrix.Base
import AME.Compute.Solver.Polynomial
import AME.Compute.Solver.Newton
import Data.List
import Control.Monad
import Data.Function

type Soluble var const = (Eq var, Ord var, Ord const, Floating const, IsIntegral const)

--Generalized solver, this will figure out if it is a polynomial or not and if not,
--solve via newton's method.
solve :: Soluble var const => var -> Equation var const -> Compute [const]
solve var (Equation lhs rhs) = do
    expr <- simplify $ lhs - rhs
    findRoots var expr

--Solve _linear_ simulatneous equations via matrix inverses.
solveSimultaneous :: Soluble var const => [Equation var const] -> Compute [(var, const)]
solveSimultaneous eqs = do
    exprs' <- exprs
    let vars = maximumBy (compare `on` length) $ map varSet exprs'
    --the total variables are the maximum...........set of free variables in the expressions
    when (length vars /= length eqs) $ throwError $ WrongNumberOfEquations (length vars) (length eqs)
    --if we have either too many or too few equations, throw an error
    when (not $ all isLinear exprs') $ throwError $ ShouldBeLinear
    --similarly if not all of the equations are linear, throw an error
    let mat = matrix $ [[coeffOf var expr | var <- vars] | expr <- exprs' ]
    --make the matrix of coefficients
    let res = vector $ [negate $ constTerm expr | expr <- exprs']
    --the result vector is the vector of the constant terms
    imat <- inverse mat
    --invert the coefficients
    let vals = concat $ values $ imat `multiply` res
    --and multiply by the constants
    return $ zip vars vals
    --attach these back to the variables they apply to
    where exprs = mapM simplify $ map (\(Equation lhs rhs) -> lhs - rhs) eqs
          --turn all the equations into simple expressions of lhs - rhs
          multiply a' b' = matrix [[(l `row` a') `dot` (k `column` b') | k <- [1..columns b']] | l <- [1..rows a']]
          --multiply two matrices.. this skips all the error checking as 
          --we already know they will have the right dimensions
          
varSet :: Eq var => Expr var const -> [var]
varSet (Var v) = [v] --the set of variables of x is [x]
varSet (Const _) = [] --constants contain no variables
varSet (Sum xs) = foldl1 union $ map varSet xs 
--the variables of a sum of terms is the union of the variables of the terms
varSet (Mul xs) = foldl1 union $ map varSet xs
--same with products
varSet (Exp x) = varSet x
varSet (Log x) = varSet x
varSet (Abs x) = varSet x
varSet (Sin x) = varSet x
varSet (Cos x) = varSet x
varSet (Tan x) = varSet x
varSet (ASin x) = varSet x
varSet (ACos x) = varSet x
varSet (ATan x) = varSet x

--exactly the following are linear:
--x, c*x, c with c constant
--and any sum given that all the terms are linear.
isLinear :: Expr var const -> Bool
isLinear (Var _) = True
isLinear (Mul [Const _, Var _]) = True
isLinear (Const _) = True
isLinear (Sum xs) = all isLinear xs
isLinear _ = False

--extract the coefficient of a variables
coeffOf :: (Num const, Eq var) => var -> Expr var const -> const
coeffOf var (Var v) = if v == var then 1 else 0
--coefficient of x = 1
coeffOf var (Const _) = 0
--coefficient of c = 0
coeffOf var (Mul [Const c, Var v]) = if v == var then c else 0
--coefficient of cx = c
coeffOf var (Sum xs) = sum $ map (coeffOf var) xs
--coefficient of a sum is the sum of the coefficients of that variable in the terms
coeffOf _ _ = 0

--get the constant term of an expression
constTerm :: Num const => Expr var const -> const
constTerm (Var _) = 0
--constant part of x = 0
constTerm (Const c) = c
--constant part of c = c
constTerm (Mul [Const _, Var _]) = 0
--constant part of ax = 0
constTerm (Sum xs) = sum $ map constTerm xs
--constant part of a sum is the sum of the constant parts of the terms
constTerm _ = 0

findRoots :: (Eq var, Ord const, Floating const, IsIntegral const) => var -> Expr var const -> Compute [const]
findRoots var expr = if isPolynomial var expr
    then solveExactPoly $ polyCoeffs var expr
    --if this is a polynomial handle it specially,
    else newtonSolve var expr (isTrig expr)
    --otherwise use the general solver.

--Detect if a function is a trig function
--to trigger special behaviour in solvePoly that
--prevents it from freezing
isTrig :: Expr var const -> Bool
isTrig (Sum xs) = any isTrig xs
isTrig (Mul xs) = any isTrig xs
isTrig (Sin _) = True
isTrig (Cos _) = True
isTrig (Tan _) = True
isTrig (ATan _) = True
isTrig (ACos _) = True
isTrig (ASin _) = True
isTrig (Div a b) = isTrig a || isTrig b
isTrig (Pow a b) = isTrig a || isTrig b
isTrig _ = False

solveExactPoly :: (Ord const, Floating const) => [(const, const)] -> Compute [const]
solveExactPoly = return . solvePoly

--Something is a polynomial if all its terms are monomial
isPolynomial :: (Num const, Ord const, Eq var, IsIntegral const) => var -> Expr var const -> Bool
isPolynomial var (Sum xs) = all (isMonomial var) xs
isPolynomial var expr = isMonomial var expr

--Exactly the following are monomial:
--  x, c*x, c*x^n, x^n, c where c si 
isMonomial :: (Ord const, Num const, Eq var, IsIntegral const) => var -> Expr var const -> Bool
isMonomial var (Mul [Const _, Var v]) = v == var
isMonomial var (Mul [Const _, Pow (Var v) (Const c)]) = isIntegral c && (v == var) && (c >= 0)
isMonomial var (Var v) = v == var
isMonomial var (Pow (Var v) (Const c)) = isIntegral c && (v == var) && (c >= 0)
isMonomial var (Const _) = True
isMonomial _ _ = False

fromJust :: Maybe a -> a
fromJust (Just x) = x

--Find the polynomial coefficeints of a given expression
polyCoeffs :: (Eq var, Num const, IsIntegral const) => var -> Expr var const -> [(const, const)]
polyCoeffs var (Sum xs) = concatMap (polyCoeffs var) xs
polyCoeffs var (Var v) | v == var = [(1, 1)]
polyCoeffs var (Pow (Var v) (Const c)) | (v == var) && isIntegral c = [(fromIntegral $ fromJust $ asInt c, 1)]
polyCoeffs var (Mul [Const a, Var v]) | v == var = [(1, a)]
polyCoeffs var (Mul [Const a, Pow (Var v) (Const c)]) | (v == var) && isIntegral c = [(fromIntegral $ fromJust $ asInt c, a)]
polyCoeffs var (Const c) = [(0, c)]
polyCoeffs _ _ = []

--find the maximum degree of the polynomial
degree :: (Num const, Ord const) => [(const, const)] -> const
degree coeffs = maximum $ map fst $ filter ((/= 0) . snd) coeffs
--                ^           ^                ^
--by finding the maximum...of the powers...of the terms that aren't zero
