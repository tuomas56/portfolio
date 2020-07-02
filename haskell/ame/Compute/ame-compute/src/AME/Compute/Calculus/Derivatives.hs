module AME.Compute.Calculus.Derivatives (differentiate) where

import AME.Compute.Expr
import AME.Compute.Error
import AME.Compute.Simplify


-- Find the partial derivative of an expression with respect to a variable.

differentiate :: (Eq var, Eq const, Floating const) => var -> Expr var const -> Compute (Expr var const)
differentiate var (Const _) = return 0
differentiate var (Var v) | v == var = return 1
differentiate var (Mul []) = return 0 
-- the empty product is constant.. this _should_ only appear as an edge case for the generalized product rule
differentiate var (Mul (x:xs)) = do
    x' <- differentiate var x
    xs' <- differentiate var (Mul xs)
    return $ x' * Mul xs + xs' * x
-- d/dx (uv) = vu' + uv' (only recursively cos we have more than two thing in our product)
differentiate var (Sum xs) = Sum <$> mapM (differentiate var) xs
-- d/dx (u + v) = u' + v'
differentiate var (Div u v) = do
    u' <- differentiate var u
    v' <- differentiate var v
    return $ (u' * v - v' * u) / (v^2)
-- d/dx (u/v) = (vu' - uv')/(v^2)
differentiate var (Pow (Const c) v) = do
    v' <- differentiate var v
    return $ v' / (Const $ log c) * Pow (Const c) v
differentiate var (Pow v (Const c)) | c /= 0 = do
    v' <- differentiate var v
    return $ v' * (Const c) * (Pow v (Const $ c - 1))
differentiate var (Pow v (Const c)) | c == 0 = return 0
differentiate var (Pow u v) = do
    u' <- differentiate var u
    v' <- differentiate var v
    return $ (v' * log u + u' * v / u) * (Pow u v)
-- d/dx (u^v) = (v'log(u) + u'v/u)u^v
differentiate var (Log u) = differentiate var u >>= \u' -> return $ u' * recip u
-- d/dx (log u) = u' * 1/u
differentiate var (Exp u) = differentiate var u >>= \u' -> return $ u' * exp u
-- d/dx (e^u) = u'e^u
differentiate var (Sin u) = differentiate var u >>= \u' -> return $ u' * cos u
-- d/dx (sin(u)) = u'cos(u)
differentiate var (Cos u) = differentiate var u >>= \u' -> return $ negate $ u' * sin u
-- d/dx (cos(u)) = -u'sin(u)
differentiate var (Tan u) = differentiate var u >>= \u' -> return $ u' * (1/cos u)^2
-- d/dx (tan(u)) = (sec(u))^2
differentiate var (ASin u) = differentiate var u >>= \u' -> return $ u' / sqrt (1 - u^2)
-- d/dx (asin(u)) = u'/sqrt(1 - u^2)
differentiate var (ACos u) = differentiate var u >>= \u' -> return $ negate $ u' / sqrt (1 - u^2)
-- d/dx (acos(u)) = -u'/sqrt(1 - u^2)
differentiate var (ATan u) = differentiate var u >>= \u' -> return $ u' / (1 + u^2)
-- d/dx (atan(u)) = u'/(1 + u^2)
differentiate var (Abs u) = differentiate var u >>= \u' -> return $ u * u' / abs u
-- d/dx |u| = uu'/|u|
