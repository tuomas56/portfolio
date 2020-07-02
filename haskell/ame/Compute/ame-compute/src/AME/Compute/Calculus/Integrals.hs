{-# LANGUAGE RankNTypes, LambdaCase #-}

module AME.Compute.Calculus.Integrals (
    integrate,
    definiteIntegral
) where

import AME.Compute.Error
import AME.Compute.Expr
import AME.Compute.Simplify
import AME.Compute.Solver.Polynomial
import AME.Compute.Calculus.Derivatives
import Control.Monad
import Data.Maybe
import Data.List
import Data.Function
import Debug.Trace


--Integrate some functions.
--This is capable of integrating:
--  * Most rational functions (including all with denominator of degree < 4)
--  * Square roots of polynomials of degree < 3,
--  * Logs of most polynomials (all with degree < 4 + most others)
--  * Simple trig expressions (i.e sin, cos, tan, sec etc. of a*x)
--  * Simple applications of integration by parts (i.e e^x*sin(x) or x*e^x)
--The reason why this is limited in its polynomial parts is that I have no
--method of factorizing products of irreducible polynomials, i.e any polynomial
--that contains at most one irreducible quadratic factor should be doable.
integrate :: Simplifiable var const => var -> Expr var const -> Compute (Maybe (Expr var const))
integrate var expr = do
    --To simplify things, we only integrate expressions with
    --zero or one free variables. It would be easy to adapt the existing
    --algorithm to handle others, but it would take a long time.
    case freeVariables expr of
        [(v, _)] | v == var -> dointegral
        [] -> dointegral
        fv -> throwError NoPartialIntegrals
    where dointegral = do 
            --Simplify it before _and_ afterwards.
            expr' <- simplify expr
            let expr'' = subSingleVar var expr'
            val <- (integrate' `thenTry` byparts) var expr''
            --We try integration by parts seperately
            --so that the integrate' function doesn't end up looping on
            --integration by parts indefinately..
            case val of
                Just val' -> Just <$> simplify val'
                Nothing -> return Nothing

integrate' :: IFunc var const
integrate' var expr = do
    expr' <- simplify expr
    let expr'' = subSingleVar var expr'
    --Try all of these things in this specific order
    --Because we need to integrate basic forms first,
    --or else polydiv and partialfraction will loop forever.
    val <- (basicforms `thenTry` 
            rationalforms `thenTry`
            polydiv `thenTry`
            partialfraction `thenTry`
            rootforms `thenTry`
            logforms `thenTry`
            expforms `thenTry`
            trigforms) var expr''
    case val of
        Just val' -> Just <$> simplify val'
        Nothing -> return Nothing

--We could do a numerical definite integrator, but for now
--we just use the fundamental theorem of calculus to evaluate the
--indefinite integral at the two end points.
definiteIntegral :: Simplifiable var const => var -> Expr var const -> const -> const -> Compute (Maybe const)
definiteIntegral var expr a b = do
    i <- integrate var expr
    case i of
        Nothing -> return Nothing
        Just i' -> do
            s1 <- substitute var (Const a) i'
            s2 <- substitute var (Const b) i'
            x1 <- evaluate s1
            x2 <- evaluate s2
            return $ Just $ x2 - x1 

--Substitute a single variable x with a*x for some constant a
--to make the integrate' function more general.
subSingleVar :: (Num const, Eq var) => var -> Expr var const -> Expr var const
subSingleVar var (Var v) | v == var = Mul [Const 1, Var v]
--x = 1*x
subSingleVar var m@(Mul [Const _, Var v]) | v == var = m
--a*x = a*x
subSingleVar _ c@(Const _) = c
subSingleVar var (Sum xs) = Sum $ map (subSingleVar var) xs
subSingleVar var (Mul xs) = Mul $ map (subSingleVar var) xs
subSingleVar var (Div a b) = Div (subSingleVar var a) (subSingleVar var b)
subSingleVar var (Pow a b) = Pow (subSingleVar var a) (subSingleVar var b)
subSingleVar var (Exp a) = Exp (subSingleVar var a)
subSingleVar var (Log a) = Log (subSingleVar var a)
subSingleVar var (Abs a) = Abs (subSingleVar var a)
subSingleVar var (Sin a) = Sin (subSingleVar var a)
subSingleVar var (Cos a) = Cos (subSingleVar var a)
subSingleVar var (Tan a) = Tan (subSingleVar var a)
subSingleVar var (ASin a) = ASin (subSingleVar var a)
subSingleVar var (ACos a) = ACos (subSingleVar var a)
subSingleVar var (ATan a) = ATan (subSingleVar var a)

--An integration function, it takes a variable to integrate with respect to
--an expression to integrate, and then might return another expression that can
--include error messages.
type IFunc var const = Simplifiable var const => var -> Expr var const -> Compute (Maybe (Expr var const))

--Try one function and if it fails try another.
thenTry :: IFunc var const -> IFunc var const -> IFunc var const
thenTry a b var expr = do
    a' <- a var expr
    case a' of 
        Just r -> return $ Just r
        Nothing -> b var expr

basicforms :: IFunc var const
basicforms var (Div _ (Const 0)) = throwError DivisionByZero
--int(ax) = 1/2ax^2
basicforms var (Mul [Const c, Var v]) | v == var = return $ Just $ Mul [Const (c / 2), Pow (Var v) 2]
basicforms var (Mul (Const c:xs)) = (fmap ((Const c) *)) <$> (integrate' var $ Mul xs)
--int(k*f(x)) = k*int(f(x))
basicforms var (Mul [x]) = integrate' var x
basicforms var (Sum xs) = (fmap Sum) <$> mapM id <$> mapM (integrate' var) xs
--int(a + b) = int(a) + int(b)
basicforms var (Const c) = return $ Just $ (Const c) * (Var var)
--int(k) = kx
basicforms var (Var v) | v == var = return $ Just $ (1/2) * (Pow (Var v) 2)
--int(x) = 0.5x^2
basicforms var (Pow (Mul [Const a, Var v]) (Const (-1))) | v == var = return $ Just $ 1/(Const a) * log (Var v)
--int((ax)^-1) = 1/a * ln(x)
basicforms var (Pow (Mul [Const a, Var v]) (Const b)) | (var == v) = 
    return $ Just $ (Const $ a ** b / (b + 1)) * Pow (Var v) (Const $ b + 1)
--int((ax)^b) = (a^b/(b+1))*x^(b+1) for integral b /= -1
basicforms var (Div (Const a) (Mul [Const b, Var v])) | v == var = return $ Just $ (Const $ a/b) * log (Var v)
--int(a/bx) = (a/b) * ln(x)
basicforms var (Div (Const a) (Pow (Mul [Const b, Var v]) (Const c))) | v == var = 
   fmap ((Const a) *) <$> (basicforms var $ Pow (Mul [Const b, Var v]) (Const $ negate c))
--int(a/(bx)^c) = a * int((bx)^(-c))
basicforms _ _ = return Nothing

--integrate basic rational functions to make partialfraction work
rationalforms :: IFunc var const
rationalforms var (Div (Const a) b) | b `isPolynomialIn` var = case degree coeffs of
        0 -> integrate var $ Const (a / c0) -- this _shouldn't_ happen, but just in case it does..
        1 -> return $ Just $ Const (a * recip c1) * Log (Const c1 * Var var + Const c0)
        -- int(1/(ax + b)) = 1/a * ln (ax + b)
        2 -> let ndisc = 4*c2*c0 - c1*c1
                 srnd = sqrt ndisc in if ndisc > 0
                    then return $ Just $ Const (2*a/srnd) * ATan (Const (2*c2 / srnd) * (Var var) + Const (c1 / srnd))
                    else return Nothing -- if ndisc is < 0 this should have
                    --been handled by partialfraction
        -- int(1/(ax^2 + bx + c)) = 2/sqrt(4ac - b^2) * atan((2ax + b)/(sqrt(4ac - b^2)))
        _ -> return Nothing
    where coeffs = polyCoeffs var b
          get = getCoeff coeffs
          c0 = get 0
          c1 = get 1
          c2 = get 2
rationalforms var (Div (Mul [Const a, Var v]) b) | (v == var) && (b `isPolynomialIn` v) = case degree coeffs of
        2 -> let ndisc = 4*c2*c0 - c1*c1
                 srnd = sqrt ndisc in if ndisc > 0 
                    then return $ Just $ Const (a / (2*c2)) * Log b 
                        - Const (a * c1 /(c2 * srnd)) * ATan (Const (2*c2 / srnd) * (Var var) + Const (c1 / srnd))
                    else return Nothing
        _ -> return Nothing
        -- this will be handled by the polynomial divider in polydiv if degree < 2
        -- if degree > 2 then it will be split into partial fractions.
    where coeffs = polyCoeffs var b
          get = getCoeff coeffs
          c0 = get 0
          c1 = get 1
          c2 = get 2
rationalforms _ _ = return Nothing

--Perform polynomial long division.
polydiv :: IFunc var const
polydiv var (Div (Sum xs) b) | ((Sum xs) `isPolynomialIn` var) && 
                               (b `isPolynomialIn` var) = 
    (fmap Sum) <$> mapM id <$> mapM (integrate var) (map (/b) xs)
polydiv var (Div a b) | (a `isPolynomialIn` var) && (b `isPolynomialIn` var) = 
    case (q, r) of
        ([], []) -> return $ Just $ Const 0
        ([], _) -> return Nothing
        (_, []) -> integrate var $ repoly var q
        (_, _) -> integrate var $ repoly var q + (repoly var r)/b
    where (q, r) = coeffDiv [] acoeffs bcoeffs
          acoeffs = polyCoeffs var a
          bcoeffs = polyCoeffs var b
polydiv _ _ = return Nothing

--Evaluate a polynomial at a given value
polyEval :: Floating const => [(const, const)] -> const -> const
polyEval coeffs val = sum $ map (\(deg, coeff) -> coeff * val ** deg) coeffs 

--Differentiate a polynomial by multiplying the coefficient
--by power and subtracting one from the power.
polyDiff :: (Eq const, Num const) => [(const, const)] -> [(const, const)]
polyDiff = map (\(deg, coeff) -> if deg == 0 then (0, 0) else (deg - 1, coeff * deg))

--Divide one polynomial by another, using the following algorithm:
--  * Divide the leading term by the other leading term
--  * Add that to the result.
--  * Multiply it by the divisor.
--  * Subtract that from the dividend and then repeat these steps.
coeffDiv :: (Floating t, Ord t) => [(t, t)] -> [(t, t)] -> [(t, t)] -> ([(t, t)], [(t, t)])
coeffDiv q r d = if nonzero r && (degree r >= degree d)
    then let t = [lead r `tdiv` lead d] in coeffDiv (q `polyAdd` t) (r `polySub` (t `polyMul` d)) d
    else (q, r)
    where nonzero [] = False
          nonzero xs = any (/= 0) $ map snd xs
          lead xs = maximumBy (compare `on` fst) $ filter ((/= 0) . snd) xs
          tdiv (d1, c1) (d2, c2) = (d1 - d2, c1 / c2)

--Multiply out two polynomials by multiplying one polynomial
--by each term of the other, and then multiplying through by that factor
polyMul :: (Num t, Ord t) => [(t, t)] -> [(t, t)] -> [(t, t)]
polyMul [] _ = []
polyMul _ [] = []
--                sum up.........multiply through one term....for each term
polyMul xs ys = foldl1 polyAdd $ map (\x -> map (`tmul` x) ys) xs
    where tmul (d1, c1) (d2, c2) = (d1 + d2, c1 * c2)

--Subtract two polynmials, by subtracting the coefficients of same powers
polySub :: (Num t, Ord t) => [(t, t)] -> [(t, t)] -> [(t, t)]
polySub xs ys = map (\case
          [(d, c)] -> if (d, c) `elem` ys
              then (d, negate c)
              else (d, c)
          [(d1, c1), (d2, c2)] -> (d1, c1 - c2)) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $ xs ++ ys
          -- subtract coefficients for each power ...group...similar powers....sorting by powers...on the union of both polynomials
--The same but for adding
polyAdd :: (Num t, Ord t) => [(t, t)] -> [(t, t)] -> [(t, t)]
polyAdd xs ys = map (\case
          [x] -> x
          [(d1, c1), (d2, c2)] -> (d1, c1 + c2)) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $ xs ++ ys

data T = T deriving (Eq, Ord)

coeffGCD :: (Floating t, Num t, Ord t, IsIntegral t) => [(t, t)] -> [(t, t)] -> [(t, t)]
coeffGCD a [(0, 0)] = a
coeffGCD a b = let (_, r) = coeffDiv [] a b in coeffGCD b (simplifyPoly T r)

--Perform the partial fraction algorithm on rational functions
--whose denominators are factorizable into linear factors and
--at most one quadratic.
--(FYI: The multiplicity of an nth root is n.. i.e a double root has multiplicity 2 etc.)
partialfraction :: IFunc var const
partialfraction var (Div a b) | (a `isPolynomialIn` var) &&
                                (b `isPolynomialIn` var) &&
                                (degree $ polyCoeffs var a) < (degree $ polyCoeffs var b) = do
                                --degree of denomiator should be < numerator otherwise its handled by polydiv
    let rootcoeffs = map (foldl1 polyMul) $ map (\(x, m) -> replicate m [(1, 1), (0, negate x)]) roots
            ---       multiply out........................(x - a)^n for an nth root a............for each root
    let rcoeffs = foldl1 polyMul rootcoeffs
    --            multiply all these factors together... this is the denominator divided by all non-linear factors.
    let denoms = map (\(x, m) -> (Var var + Const (negate x), m)) roots
    --           For each root, record its linear factor (x - a), and its multiplicity
    let finaldenom = simplifyPoly var $ fst $ coeffDiv [] bcoeffs rcoeffs
    --  The denominator of the non-linear component is the denominator divided by all the roots.
    let finalpoly = repoly var finaldenom
    let fracPower r m d p = do
    -- processing each root, with a given mulitplicity, denominator, and the current power we are looking at.
    -- p is not always m, because for m > 1 we consider fracPower for all values of p < m.
    -- Here we use the "cover up method" for partial fractions:
    --  * For a given factor, remove that factor from the denominator of the function
    --  * Then find the root of that factor, and evaluate the new expression with at that root
    -- i.e for 1/(x - 1)(x - 2), we remove (x - 1) and thus evaluate 1/(x - 2) at the root of (x - 1),
    -- giving the coefficient of 1/(x - 1) in the partial fraction expansion as 1/(1 - 2) = -1.
            let basedenom = Mul $ finalpoly : (map (\(d, m) -> Pow d (Const $ fromIntegral m)) $ filter ((/= d) . fst) denoms)
            -- First we compute the denominator of the new expression by multiplying all the factors together
            -- including the non linear factor, except for the one we are looking at. (Here, d is the factor we computed earlier,
            -- and m is the multiplicity)
            let basef' = Div a basedenom
            -- The to compute the new expression we just divide the numerator by the new denominator
            basef <- simplify basef'
            -- Then simplify that
            expr <- nthderivative (m - p) basef
            -- To compute the coefficient of a factor with power > 1,
            -- we must slightly modify the algorithm above, as follows:
            -- If a root r has multiplicty m, then the mth derivative of 
            -- the function will have a root r with multiplicty 1,
            -- hence by taking the mth derivative we can just evaluate it as normal
            -- (I have a proof of this if you want to see it.)
            sub <- substitute var (Const r) expr
            val <- evaluate sub
            let denom = Pow d (Const $ fromIntegral p)
            let coeff = val/(fromIntegral $ factorial $ m - p)
            -- And we have to divide by (m - p)! to correct for the error incurred when taking the derivatives
            basedenom' <- simplify $ Const coeff * basedenom
            -- Finally we compute the modified denominator as we first computed, mulitplied by our term coefficient
            -- this is used later to compute the non-linear component
            return $ (basedenom', denom, coeff)
    let frac (r, m) = mapM (fracPower r m (Var var + Const (negate r))) [1..m]
    --All the partial fraction terms for a given root are given by
    --evaluating fracPower on powers between 1 and m
    fracnumers <- concat <$> mapM frac roots
    --Concatenate all the terms for each root
    let fntb = simplifyPoly var $ acoeffs `polySub` (foldl1 polyAdd $ map (\(x, _, _) -> polyCoeffs var x) fracnumers)
    -- Subtract all the multiplied denominators we got earlier from the numerator.
    let finalnumer = simplifyPoly var $ fst $ coeffDiv [] fntb rcoeffs
    -- And divide this by all the linear components to get the numerator of the non-linear component
    -- Maths says this _should_ always divide evenly, but it still freaks me out a bit.
    let final = Div (repoly var finalnumer') (repoly var finaldenom')
        --So the non-linear component is just the _simplified_ numerator divided by the denominator
            where finalnumer' = fst $ coeffDiv [] finalnumer pgcd
                  --To simplify these we take the gcd of them, and divide both by it
                  --effectively cancelling down the fraction.
                  finaldenom' = fst $ coeffDiv [] finaldenom pgcd
                  pgcd = coeffGCD finalnumer finaldenom
    --Then to integrate each term, we follow the usual rules.
    let intterm (Div (Const c) (Pow (Sum [Var var, Const nr]) (Const p))) = case p of
            --int(c/(x + nr)) = c*log(x + nr) 
            1 -> Const c * Log (Sum [Var var, Const nr])
            --int(c/(x + nr)^p) = c/(1 - p) * (x - nr)^(p - 1)
            _ -> Div (Const (c / (1 - p))) (Pow (Sum [Var var, Const nr]) (Const $ p - 1))
    is <- do
            final' <- integrate var final
            --Integrate the non-linear term
            let fracnumers' = map (Just . intterm) $ map (\(_, d, c) -> Div (Const c) d) fracnumers
            --Integrate all the linear terms
            return $ final' : fracnumers'
    let mis = sequence is
    case mis of
        Nothing -> return Nothing
        Just mis' -> return $ Just $ Sum mis'
        --If none of that failed, add them all together
    where closeEnough e a b = abs (a - b) <= e
          --sometimes my solving algorithm is a little unstable
          --so we check if two numbers are _veeery_ close instead of
          --_exactly_ equal
          factorial 0 = 1
          factorial p = p * factorial (p - 1)
          --simple recursive factorial definition
          nthderivative 0 expr = return expr
          --the zeroth derivative is just the function
          nthderivative n expr = do
            expr' <- nthderivative (n - 1) expr
            differentiate var expr'
          --the nth derivative is the derivative of the (n - 1)th derivative
          acoeffs = polyCoeffs var a
          --the coefficients of the numerator
          bcoeffs = polyCoeffs var b
          -- ... and of the denominator
          broots = solvePoly bcoeffs
          -- solve the denominator using the specialized polynomial solver.
          multiplicity coeffs n r = if closeEnough (0.000001) (polyEval coeffs r) 0
            then multiplicity (polyDiff coeffs) (n + 1) r
            else n
          --we can use the above theorem about multiplicity to find the multiplicity of a root
          --by counting how many derivatives we can take before it is no longer a root.
          roots = map ((,) <*> multiplicity bcoeffs 0) broots
partialfraction _ _ = return Nothing

--something is polynomial in a variable if all its terms are monomial in that variable
isPolynomialIn :: (Num const, Ord const, Eq var, IsIntegral const) => Expr var const -> var -> Bool
isPolynomialIn (Sum xs) var = all (isMonomial var) xs
isPolynomialIn expr var = isMonomial var expr

-- The following expressions are monomials:
-- x, c*x, x^n, c*x^n, c*(k*x)^n, c for constant c, k and integer n
isMonomial :: (Num const, Ord const, Eq var, IsIntegral const) => var -> Expr var const -> Bool
isMonomial var (Mul [Const _, Var v]) = v == var
isMonomial var (Mul [Const _, Pow (Var v) (Const c)]) = isIntegral c && (v == var) && (c >= 0)
isMonomial var (Var v) = v == var
isMonomial var (Pow (Var v) (Const c)) = isIntegral c && (v == var) && (c >= 0)
isMonomial var (Pow (Mul [Const a, Var v]) (Const c)) = (v == var) && isIntegral c && (c >= 0)
isMonomial var (Mul [Const _, Pow (Mul [Const a, Var v]) (Const c)]) = (v == var) && isIntegral c && (c >= 0)
isMonomial var (Const _) = True
isMonomial _ _ = False

--Find the coefficients of the terms in a polynomial.
--Basically the same as above but recording c and k appropriately
polyCoeffs :: (Eq var, Floating const, IsIntegral const) => var -> Expr var const -> [(const, const)]
polyCoeffs var (Sum xs) = concatMap (polyCoeffs var) xs
polyCoeffs var (Var v) | v == var = [(1, 1)]
polyCoeffs var (Pow (Var v) (Const c)) | (v == var) && isIntegral c = [(c, 1)]
polyCoeffs var (Mul [Const a, Var v]) | v == var = [(1, a)]
polyCoeffs var (Mul [Const a, Pow (Var v) (Const c)]) | (v == var) && isIntegral c = [(c, a)]
polyCoeffs var (Pow (Mul [Const a, Var v]) (Const c)) | (v == var) && isIntegral c = [(c, a ** c)]
polyCoeffs var (Mul [Const b, (Pow (Mul [Const a, Var v]) (Const c))]) | (v == var) && isIntegral c = [(c, b * a ** c)]
polyCoeffs var (Const c) = [(0, c)]
polyCoeffs _ _ = []

--Turn a list of coefficients into a polynomial.
repoly :: (Fractional const, Ord const) => var -> [(const, const)] -> Expr var const
repoly var xs = let xs' = concatMap f $ filter ((not . closeEnough 0.000001 0) . snd) xs
        --turn terms into expressions... for each term that is not _almost_ 0, which we just remove.
                    f x = case x of
                            (0, c) -> [Const c]
                            --Zeroth powers are constants
                            (1, c) -> [Const c * Var var]
                            --First powers are just variables
                            (d, c) -> [Const c * Pow (Var var) (Const d)] in
                            --everything else is a regular power
                case xs' of
                    [] -> Sum [Const 0]
                    _ -> Sum xs'
                    --Add up all the expressions
                where closeEnough e a b = abs (a - b) <= e

--simplify a polynomial by converting it into an expression
--and then back to a list of coefficients
simplifyPoly :: Simplifiable var const => var -> [(const, const)] -> [(const, const)]
simplifyPoly var p = polyCoeffs var $ repoly var p

--get the coefficient of a given power, returning 0 if the power is not present
getCoeff :: (Eq const, Num const) => [(const, const)] -> const -> const
getCoeff coeffs k = case lookup k coeffs of
    Just x -> x
    Nothing -> 0

--get the highest present power
degree :: (Num const, Ord const) => [(const, const)] -> const
--by..................filtering those that are not coefficient 0
degree coeffs = case filter ((/= 0) . snd) coeffs of
    [] -> 0
    xs -> maximum $ map fst $ xs
    --and selecting the highest

--integrate square roots of polynomials
rootforms :: IFunc var const
rootforms var (Pow a (Const 0.5)) | a `isPolynomialIn` var = case degree coeffs of
    -- These are just formulas copied from the formula book.
    0 -> return $ Just $ Const (c0 ** 0.5) * (Var var)
    --except this one, which really _should_ be handled by the simplification algorithm
    --but just in case it doesnt, we integrate sqrt(c) as sqrt(c)x for a constant c
    1 -> return $ Just $ (Const (2*c0/(3*c1)) + 2*(Var var)/3) * Pow (Const c0 + (Var var)*Const c1) (0.5)
    2 -> return $ Just $ (Const c1 + (Const $ 2*c2)*(Var var))/(Const $ 4*c2) 
             * Pow ((Const c2)*(Var var)**2 + (Const c1)*(Var var) + (Const c0)) 0.5 +
             (Const $ (4*c2*c0 - c1*c1)/(8*c2**1.5)) * Log ((Const $ 2*c2)*(Var var) + (Const c1) 
             + 2*Pow (Const (c2*c2) * Pow (Var var) 2 + Const (c1*c2) * (Var var) + Const (c2*c0)) 0.5)
    _ -> return Nothing
    where coeffs = polyCoeffs var a
          get = getCoeff coeffs
          c0 = get 0
          c1 = get 1
          c2 = get 2
rootforms _ _ = return Nothing

--integrate logs of some polymomials
logforms :: IFunc var const
logforms var (Log a) | a `isPolynomialIn` var = case degree coeffs of
    0 -> if c0 > 0
            then return $ Just $ Const (log c0) * (Var var)
            else return Nothing
    --for constants, we first check the argument is > 0 as log is not defined otherwise
    --and then we integrate log(c) as xlog(c) where c is constant
    1 -> return $ Just $ (Var var + Const (c0/c1)) * Log(Const c1 * Var var + Const c0) - Var var
    --int(log(ax + b)) = (x + b/a)log(ax + b) - x
    2 -> let ndisc = 4*c2*c0 - c1*c1
             srnd = sqrt ndisc in if ndisc > 0
        --for quadratics, if it has no solutions
        then return $ Just $ Const (srnd/c2) * ATan (((Const $ 2*c2) * (Var var) + (Const c1)) / (Const srnd)) -
             2*(Var var) + (Const (c1 / (2 * c2)) + (Var var)) * Log (Const c2 * (Var var) ** 2 + Const c1 * (Var var) + Const c0)
        --use atan to integrate it (a long and truly boring formula)
        else case solvePoly coeffs of
            --otherwise solve it and
            [] -> error "we just ruled this out above, duh"
            --if its a repeated root, bring down the power as a multiple and integrate
            [r] -> return $ Just $ Const (2 * c2) * (Var var - Const r) * Log (Var var - Const r) - 2 * Var var
            --or if it has two roots, split it up into logs of linear factors and integrate
            [r1, r2] -> return $ Just $  Const c2 * ((Var var - Const r1) * Log (Var var - Const r1) + (Var var - Const r2) * Log (Var var - Const r2) - 2 * Var var)
    --for general polynomials, solve it
    _ -> case solvePoly coeffs of
        [] -> return Nothing
        --if this is a product of only quadratic factors, give up
        xs -> do
            let rcoeffs = foldl1 polyMul $ map (\x -> [(1, 1), (0, negate x)]) xs
            --otherwise, multiply out all the linear factors
            let fcoeffs = fst $ coeffDiv [] coeffs rcoeffs
            --and divide the polynomial by it, leaving the remaining non-linear factor
            final' <- logforms var (Log $ repoly var $ fcoeffs)
            --try and integrate this non-linear bit
            case final' of
                Just final -> return $ Just $ final + Sum (map intlin xs)
                --and if we can, integrate the linear bits and add it together
                Nothing -> return Nothing
    where coeffs = polyCoeffs var a
          get = getCoeff coeffs
          c0 = get 0
          c1 = get 1
          c2 = get 2
          intlin x = (Var var - Const x) * Log (Var var - Const x) - Var var  
          --int(x - c) = (x - c)log(x - c) - x 
logforms _ _ = return Nothing

expforms :: IFunc var const
expforms var (Exp (Mul [Const a, Var v])) | v == var = return $ Just $ Mul [Const (recip a), Exp (Mul [Const a, Var v])]
--int(exp(ax)) = 1/a * exp(ax)
expforms _ _ = return Nothing

--Integrate a _few_ trig functions
--Anything more complicated really requires substitution
--which is waaaaaaay to sophisticated for the current system to handle,
--especially the simplification engine.
trigforms :: IFunc var const
trigforms var (Cos (Mul [Const a, Var v])) | v == var = return $ Just $ Mul [Const (recip a), Sin (Mul [Const a, Var v])]
--int(cos(ax)) = 1/a sin(ax)
trigforms var (Sin (Mul [Const a, Var v])) | v == var = return $ Just $ Mul [Const (negate $ recip a), Cos (Mul [Const a, Var v])]
--int(sin(ax)) = -1/a cos(ax)
trigforms var (Tan (Mul [Const a, Var v])) | v == var = return $ Just $ Mul [Const (negate $ recip a), Log (Cos (Mul [Const a, Var v]))]
--int(tan(ax)) = -1/a log(cos(ax))
trigforms var (Div 1 (Cos (Mul [Const a, Var v]))) | v == var = return $ Just $ 
    Mul [Const (recip a), Log (Div 1 (Cos $ Mul [Const a, Var v]) + Tan (Mul [Const a, Var v]))]
--int(sec(ax)) = 1/a log(sec(ax) + tan(ax))
trigforms var (Div 1 (Sin (Mul [Const a, Var v]))) | v == var = return $ Just $ 
    Mul [Const (recip a), Log (Div 1 (Sin $ Mul [Const a, Var v]) + Div 1 (Tan (Mul [Const a, Var v])))]
--int(cosec(ax)) = 1/a log(cosec(ax) + cot(ax))
trigforms var (Div 1 (Tan (Mul [Const a, Var v]))) | v == var = return $ Just $
    Mul [Const (recip a), Log (Sin (Mul [Const a, Var v]))]
--int(cot(ax)) = 1/a log(sin(ax))
trigforms var (ATan (Mul [Const a, Var v])) | v == var = return $ Just $ Sum [
    Mul [Var var, ATan (Mul [Const a, Var var])], Mul [-0.5, Log (Sum [Pow (Var var) 2, 1])]]
--int(atan(ax)) = x atan(ax) - 1/2 log(x^2 + 1)
trigforms _ _ = return Nothing

--Simple integration by parts
--This will do recursive integration by parts
--operating either until 5 steps are performed
--at which point it gives up
--or until either we get a multiple of our original function
--or a function we know how to integrate
byparts :: IFunc var const
--don't try and integrate cf(x) for c constant, or it will fail.
byparts var (Mul [Const _, _]) = return Nothing
byparts var a@(Mul xs) = do
    sa <- simplify a
    let u:vs' = sortOn ilate xs
    --use the ilate rules to sort out our choice for u
    v' <- simplify $ Mul vs'
    --multiply together all the vs and simplify to get our v'
    let step (u, v', coeff, _) = do
    --one step of integration by parts consists of
            du <- differentiate var u
            --differentiating u
            u' <- simplify du
            maybeV <- integrate' var v'
            --and trying to integrate v'
            case maybeV of
                Nothing -> return Nothing
                --if this doesn't work, we give up
                Just v -> do
                    s <- simplify $ Mul [Const (negate coeff), u', v]
                    --otherwise we multiply these two together, and multiply by
                    -- -1 or 1 depending on which step we are on.
                    return $ Just (u', v, negate coeff, s)
    let testSame (_, _, _, s) = return $ noCoeff s `eqStruc` noCoeff sa
    --test if we got our original function again
    let testIntegrable (_, _, _, s) = isJust <$> (integrate' var s)
    --test if we have an easily integrable function
    let testCombined v = do
    --test both tests and if one works, return a string indicating which.
            same <- testSame v
            if same
                then return $ Just "same"
                else do
                    integrable <- testIntegrable v
                    if integrable
                        then return $ Just "integrable"
                        else return Nothing
    let mulTerm (u, _, coeff, _) (_, v, _, _) =  Mul [Const coeff, u, v]
    let sumUp steps = Sum $ zipWith mulTerm steps (tail steps)
    let startVal = (u, v', 1, a)
    --start on our inital u and v, and coefficient 1
    maybeSteps <- iterateWhileMaybeWithMaxM 5 step testCombined startVal
    --try iterating step while our test is not satisfied but also does not fail, with a maximum number
    --of iterations of 5
    case maybeSteps of
        Nothing -> return Nothing
        --if this fails, give up
        Just (method, steps) -> do
            --else check which method we got
            let result = sumUp $ startVal : steps
            --and sum up all the steps
            let (_, _, _, finalexpr) = last steps
            case method of
                "integrable" -> do
                    maybeFinal <- integrate var finalexpr
                    --if its an integrable function, we need to integrate the last term
                    case maybeFinal of
                        Nothing -> return Nothing
                        Just final -> do
                            s <- simplify $ result + final
                            return $ Just s
                "same" -> do
                    --if its a multiple of our original, we move it to the other side of the equation
                    --and solve for our original integral
                    s <- simplify $ Const (recip $ (1 - (onlyCoeff finalexpr / onlyCoeff a))) * result
                    return $ Just s
    where ilate (ACos _) = 0
          ilate (ASin _) = 0
          ilate (ATan _) = 0
          --ilate rules say inverse trig goes first
          ilate (Log _) = 1
          --then logs
          ilate a | a `isPolynomialIn` var = 2
          ilate (Div a b) | (a `isPolynomialIn` var) && (b `isPolynomialIn` var) = 2
          --then polynomials and rational functions
          ilate (Sin _) = 3
          ilate (Cos _) = 3
          ilate (Tan _) = 3
          --then trig
          ilate (Exp _) = 4
          --then exponential
          ilate _ = 5
          --to check if two things are structurally equal
          --we perform our usual check, but sorting the elements first
          --to avoid the order of our terms messing up our test.
          eqStruc (Sum a) (Sum b) = (length a == length b) && (all id $ zipWith eqStruc (sort a) (sort b))
          eqStruc (Mul a) (Mul b) = (length a == length b) && (all id $ zipWith eqStruc (sort a) (sort b))
          eqStruc (Pow a b) (Pow c d) = (a `eqStruc` c) && (b `eqStruc` d)
          eqStruc (Div a b) (Div c d) = (a `eqStruc` c) && (b `eqStruc` d)
          eqStruc (Sin a) (Sin b) = a `eqStruc` b
          eqStruc (Cos a) (Cos b) = a `eqStruc` b
          eqStruc (Tan a) (Tan b) = a `eqStruc` b
          eqStruc (ASin a) (ASin b) = a `eqStruc` b
          eqStruc (ACos a) (ACos b) = a `eqStruc` b
          eqStruc (ATan a) (ATan b) = a `eqStruc` b
          eqStruc (Exp a) (Exp b) = a `eqStruc` b
          eqStruc (Log a) (Log b) = a `eqStruc` b
          eqStruc (Abs a) (Abs b) = a `eqStruc` b
          eqStruc (Const a) (Const b) = a == b
          eqStruc (Var a) (Var b) = a == b
          eqStruc _ _ = False
byparts _ _ = return Nothing

--Remove the coefficients from a monomial
noCoeff :: Num const => Expr var const -> Expr var const
noCoeff (Const _) = 1
noCoeff (Mul [Const _, a]) = a
noCoeff (Mul ((Const _):xs)) = Mul xs
noCoeff x = x

--Take only the coefficient from a monomial
onlyCoeff :: Num const => Expr var const -> const
onlyCoeff (Const c) = c
onlyCoeff (Mul ((Const c):_)) = c
onlyCoeff _ = 1

--The following function performs the following equivalent imperative code:
--      x = val    
--      i = 0
--      while(!test(x) && i < max) {
--          x = func(x)
--          i++
--      }
--      if (i < max) {
--          return x
--      } else {
--          raise exception
--      }
--where both test and func can raise exceptions
iterateWhileMaybeWithMaxM :: Monad m => Int -> (a -> m (Maybe a)) -> (a -> m (Maybe b)) -> a -> m (Maybe (b, [a]))
iterateWhileMaybeWithMaxM 0 _ _ _ = return Nothing
iterateWhileMaybeWithMaxM max func test val = do
    maybeRet <- func val
    case maybeRet of
        Nothing -> return Nothing
        Just ret -> do
            done <- test ret
            case done of
                Just token -> return $ Just (token, [ret])
                Nothing -> do
                    rest <- iterateWhileMaybeWithMaxM (max - 1) func test ret
                    case rest of
                        Just (token, rest) -> return $ Just (token, ret : rest)
                        Nothing -> return Nothing
