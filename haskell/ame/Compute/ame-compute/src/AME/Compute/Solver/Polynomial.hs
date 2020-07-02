{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module AME.Compute.Solver.Polynomial (
  solveLinear,
  solveQuadratic,
  solveHigherOrder,
  solvePoly
) where

import Data.List
import Data.Function
import Data.Ord

--Solve a polynomial represented as a list of coefficients
solvePoly :: (Floating t, Ord t) => [(t, t)] -> [t]
solvePoly coeffs' = case n of
    2 -> solveQuadratic (get 2) (get 1) (get 0)
    1 -> solveLinear (get 1) (get 0)
    0 -> solveLinear 0 (get 0)
    --Specialized solvers for quadratics and linear equations.
    _ -> solveHigherOrder coeffs
    where n = degree coeffs
          get k = case lookup k coeffs of
            Just x -> x
            Nothing -> 0
          coeffs = sortOn (Down . fst) coeffs'
                   --Sort by power, in reverse
                   --This is necessary for solveHigherOrder to work

degree :: (Num const, Ord const) => [(const, const)] -> const
degree coeffs = maximum $ map fst $ filter ((/= 0) . snd) coeffs

solveLinear :: (Eq t, Fractional t) => t -> t -> [t]
solveLinear 0 0 = [0]
--0 = 0x + 0 has one solution 0
solveLinear 0 _ = []
--0 = 0x + a has no solutions for a /= 0
solveLinear a b = [-b/a]
--0 = ax + b has one solution -b/a

--solve a quadratic with the quadratic formula.
solveQuadratic :: (Ord t, Floating t) => t -> t -> t -> [t]
solveQuadratic a b c = case disc `compare` 0 of
    LT -> []
    EQ -> [cp]
    --If the discriminant is zero, we have one repeated root
    GT -> [cp + sqrt disc, cp - sqrt disc]
    --Otherwise we can have two roots.
    where disc = (b^2 - 4*a*c)/(4*a^2)
          cp = -b/(2*a)

-- approximately solving higher order polynomials is also _robustly_ possible
-- (i.e not using the erratic joke we call Newton's method)
-- using the following algorithm: 
-- let f(x) be a polynomial of degree > 1, then the roots of f(x) must fall into two categories
-- 1) it must fall between two stationary points that are not also points of inflection, 
--    (we call these the interior roots of f(x))
-- 2) or it must fall somewhere outside (or on) one of the outermost stationary or inflection points
--    (we call these the exterior roots) (this also covers the case of curves with no stationary points)
-- to find the interior roots, we find all the stationary points and iterate over them pairwise
-- for each pair, we check whether they are on different sides of the x-axis, and if so, there must be a
-- root between them by the intermediate value theorem and hence we find it using the bisection method.
-- to find the exterior roots, we first check which of two categories the curve is in:
-- 1) if the curve has stationary points, we find the outermost stationary points. we check if they
--    whether they are on the wrong side of the x-axis and it they are not, we then perform a bisection 
--    search between the outermost point and the Lagrange bound for the root interval [1].
-- 2) if the curve has no stationary points it must have odd degree and hence at least one root, 
--    thus we find the inflection point (there must be one) and check whether its above or below zero,
--    and perform the same search as before.
-- [1]: Lagrange derived the bound of  max {1, sum from i = 0 to n - 1 of |ai/an|}
--      on the magnitude of the roots of a polynomial with real coefficients.
--      See: Lagrange J–L (1798) Traite de la r'esolution des equations numeriques. Paris.

solveHigherOrder :: (Ord t, Floating t) => [(t, t)] -> [t]
solveHigherOrder coeffs = unionBy (closeEnough 0.0000001) interior exterior
                          --the final root set is the union of interior and exterior roots
    where closeEnough e a b = abs (a - b) <= e
          coeffs' = polyDiff coeffs
          --The first derivative of the function
          coeffs'' = polyDiff coeffs'
          --The second derivative
          sps = map ((,) <*> (polyEval coeffs)) $ solvePoly coeffs'
          --The stationary points are the roots of the first derivative
          ips = map ((,) <*> (polyEval coeffs)) $ solvePoly coeffs''
          --The inflection points are the roots of the second derivative
          cisps = sortBy (compare `on` fst) $ unionBy (closeEnough 0.0000001 `on` fst) sps ips
          --Combine these and sort them, left to right
          interior = case map findInterior (zip cisps $ tail cisps) of
          --we find the interior roots my looking between each pair of roots
          --(zip x $ tail x, generates the list [(a, b), (b, c), (c, d)] if x = [a, b, c, d])
                [] -> []
                --If there are none, thats okay
                xs -> foldl1 (unionBy $ closeEnough 0.0000001) xs
                --Otherwise remove duplicates.
          findInterior ((x1, fx1), (x2, fx2)) = if signum fx1 /= signum fx2 
            --To find interior roots we first check that they are on different sides of the x axis
            then [bisection x1 x2 0]
            --and if they are use a bisection search between them
            else []
            --and if no, give up.
          exterior = (findExterior True $ minimumBy (compare `on` fst) cisps) 
                     ++  (findExterior False $ maximumBy (compare `on` fst) cisps)
          --the exterior roots are left of the minimum defined point
          --and right of the maximum defind point
          findExterior leftmost (x1, fx1) = case (dir, fx1 <= 0) of
            (True, False) -> []
            --If we are looking upwards, but x1 is > 0, there are no solutions
            (False, True) -> []
            --If we are looking downwards, but x1 is <= 0 there are no solutions
            (True, True) -> [bisection x1 lgbound 0]
            --If we are looking upwards and x1 is <= 0 bisect between here
            --and the lagrange bound.
            (False, False) -> [bisection x1 lgbound 0]
            --If we are looking downwards and x1 > 0, do the same.
            where dir = if leftmost
                            then polyEval coeffs (x1 - 0.1) > fx1
                            else polyEval coeffs (x1 + 0.1) > fx1
                  --Check which direction we are looking by evaluating a point on the exterior side
                  --of the last point.
                  lgbound = if leftmost then negate lgbound' else lgbound'
                  --The lagrange bound for the maximum magnitude of the roots.
                  lgbound' = max 1 (sum $ map (\i -> abs $ (a i)/an) $ range (n - 1))
                  n = degree coeffs
                  an = getCoeff coeffs n
                  a i = getCoeff coeffs i
                  range 0 = return 0
                  range n = n : range (n - 1)
          bisection x1 x2 n = if (closeEnough 0.0000001 fx1 0)
            --To perform a bisection search, we first check if the endpoints are zero.
            then x1 
            else if (closeEnough 0.0000001 fx2 0) 
                then x2
                --if so, we are done
                else case (fx1 > 0, fm > 0) of
                --otherwise, check whether the midpoint and one end point is > 0
                        (True, True) -> bisection m x2 (n + 1)
                        --If both are > 0 we need to bisect between the midpoint and the other endpoint
                        (False, True) -> bisection x1 m (n + 1)
                        --If the midpoint is >0 but the first end point is not, we bisect between the midpoint
                        --and that end point
                        (True, False) -> bisection x1 m (n + 1)
                        --Similarly, but with the positions swapped
                        (False, False) -> bisection m x2 (n + 1)
                        --If both are > 0 we need to bisect between the midpoint and the other endpoint
            where fx1 = polyEval coeffs x1
                  fx2 = polyEval coeffs x2
                  m = (x1 + x2)/2
                  fm = polyEval coeffs m

getCoeff :: (Eq const, Num const) => [(const, const)] -> const -> const
getCoeff coeffs k = case lookup k coeffs of
    Just x -> x
    Nothing -> 0

--Evaluate a polynomial at a given input.
polyEval :: Floating const => [(const, const)] -> const -> const
polyEval coeffs val = sum $ map (\(deg, coeff) -> coeff * val ** deg) coeffs 

polyDiff :: (Eq const, Num const) => [(const, const)] -> [(const, const)]
polyDiff = map (\(deg, coeff) -> if deg == 0 then (0, 0) else (deg - 1, coeff * deg))
