module AME.Compute.Solver.Newton (
  newtonSolve
) where

import AME.Compute.Expr
import AME.Compute.Error
import AME.Compute.Simplify
import AME.Compute.Calculus.Derivatives
import Data.List

--Solve equations approximately with newton's method
newtonSolve :: (Eq var, Ord const, Floating const) => var -> Expr var const -> Bool -> Compute [const]
newtonSolve v e isTrig = do
      --to solve something with newton's method
      as <- newtonSolve' 1 e []
      --start looking from both 1
      bs <- newtonSolve' (-1) e []
      -- ...and -1, in order to skip out biases inthe middle bit
      return $ as `union` bs
    where newtonSolve' x00 e' acc = do
            let x0 = case acc of
                        [] -> x00 --If we've only just started, start looking from x00 (any number will do here)
                        (r:_) -> r + 0.1 --If we've already got a root, start looking a small distance from that root,
                                         --because if that root is 1, then setting x0 = x00 will get a DivisionByZero error.
            root <- newtonRoot v x0 isTrig e' -- Find a root of e'
            if isTrig && (length acc == 4) -- If we have a trig function, we dont want to find too many roots (there should be infinite)
                then return acc
                else case root of
                    Just r -> newtonSolve' x00 (e' / (0.01 * (Var v - Const r))) (r : acc)
                    -- if we find a root, add it to the list of roots, then find more roots, 
                    -- avoiding the one we just found by dividing the function by (x - r) (we multiply by 0.01
                    -- to make it avoid r even more) so that when it is close to r it is a large value.
                    Nothing -> return acc

newtonRoot :: (Eq var, Ord const, Floating const) => var -> const -> Bool -> Expr var const -> Compute (Maybe const)
newtonRoot v x0 isTrig e = do
    fx0 <- f x0
    --taking one root in newton's method, we iterate the newton steps on it
    iterateNewton fx0 x0 0
    where e' = differentiate v e -- the derivative of e with respect to v
          f c = do -- evaluate the function at c
            se <- substitute v (Const c) e 
            -- substitute v for c and evaluate
            evaluate se
          f' c =  do -- the same thing but with the derivative
            e'' <- e' -- except differentiate returns Compute (Expr _ _) so we need to evaluate it
            se' <- substitute v (Const c) e''
            evaluate se'
          newtonStep x0 = do -- perform one step of newton's method
            fx <- f x0
            fx' <- f' x0
            return $ x0 - fx/fx' -- x1 = x0 - f(x0)/f'(x0)
          iterateNewton fx0 x0 n = do
            x1 <- newtonStep x0
            fx1 <- f x1
            if n > 100
                -- if we have done more than one hundred steps, assume we can't find a root
                then return Nothing
                else if (abs fx1 < 0.00001)
                    then return $ Just x1 -- if we find a root, return it
                    else iterateNewton fx1 x1 (n + 1) -- otherwise keep looking
