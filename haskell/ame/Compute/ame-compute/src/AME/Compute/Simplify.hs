{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables, ConstraintKinds, FlexibleInstances #-}

module AME.Compute.Simplify (
    substitute,
    evaluate,
    emap,
    simplify,
    simplifyEqn,
    freeVariables,
    emapt,
    Simplifiable
) where

import AME.Compute.Expr
import AME.Compute.Error
import Control.Monad
import Data.Foldable
import Data.List
import Data.Function
import Data.Bifunctor
import Data.Maybe

--  This module contains functions for:
--      * Simplifying algebraic expressions
--      * Evaluating expressions
--      * Substituting values into expressions
--  Technically evaluate and simplify give the same result on expressions 
--  with no free variables but evaluate is _much_ faster.

-- substitute a variable for a value in an expression.
-- i.e) substitute X (Const 1) (Sum [Exp X, Div X (Const 2)]) = Sum [Expr (Const 1), Div (Const 1) (Const 2)]
substitute :: Eq var => var -> Expr var const -> Expr var const -> Compute (Expr var const)
substitute var value expr = return $ emapt (\case
    Var v | v == var -> value
    x -> x) expr

emap :: (Expr var const -> Expr var const) -> (Expr var const -> Expr var const)
emap f (Sum xs) = Sum $ map (f . emap f) xs
emap f (Mul xs) = Mul $ map (f . emap f) xs
emap f (Div a b) = Div (f $ emap f a) (f $ emap f b)
emap f (Pow a b) = Pow (f $ emap f a) (f $ emap f b)
emap f (Exp e) = f $ Exp $ emap f e
emap f (Log e) = f $ Log $ emap f e
emap f (Abs e) = f $ Abs $ emap f e
emap f (Sin e) = f $ Sin $ emap f e
emap f (Cos e) = f $ Cos $ emap f e
emap f (Tan e) = f $ Tan $ emap f e
emap f (ASin e) = f $ ASin $ emap f e
emap f (ACos e) = f $ ACos $ emap f e
emap f (ATan e) = f $ ATan $ emap f e
emap f x = f x

-- emapt will execute a function over all the terminal subexpressions (Const and Var)
emapt :: (Expr var1 const1 -> Expr var2 const2) -> Expr var1 const1 -> Expr var2 const2
emapt f (Sum es) = Sum $ map (emapt f) es
emapt f (Mul es) = Mul $ map (emapt f) es
emapt f (Div ea eb) = Div (emapt f ea) (emapt f eb)
emapt f (Pow ea eb) = Pow (emapt f ea) (emapt f eb)
emapt f (Exp e) = Exp (emapt f e)
emapt f (Log e) = Log (emapt f e)
emapt f (Sin e) = Sin (emapt f e)
emapt f (Cos e) = Cos (emapt f e)
emapt f (Tan e) = Tan (emapt f e)
emapt f (ASin e) = ASin (emapt f e)
emapt f (ACos e) = ACos (emapt f e)
emapt f (ATan e) = ATan (emapt f e)
emapt f (Abs e) = Abs (emapt f e)
emapt f (Var x) = f (Var x)
emapt f (Const c) = f (Const c)

-- evaluate a expression that does not contain any free variables.
evaluate :: (Eq const, Floating const) => Expr var const -> Compute const
evaluate (Var _) = throwError UnexpectedFreeVariable
evaluate (Const c) = return c
evaluate (Sum es) = sum <$> mapM evaluate es
evaluate (Mul es) = product <$> mapM evaluate es
evaluate (Div ea eb) = do
    a <- evaluate ea
    b <- evaluate eb
    if b /= 0 -- We need to check for division by zero: 
              -- Haskell may not throw an error, if we leave it on its own here.
        then return $ a / b
        else throwError DivisionByZero
evaluate (Pow ea eb) = liftM2 (**) (evaluate ea) (evaluate eb)
evaluate (Exp e) = liftM exp $ evaluate e
evaluate (Log e) = liftM log $ evaluate e
evaluate (Sin e) = liftM sin $ evaluate e
evaluate (Cos e) = liftM cos $ evaluate e
evaluate (Tan e) = liftM tan $ evaluate e
evaluate (ASin e) = liftM asin $ evaluate e
evaluate (ACos e) = liftM acos $ evaluate e
evaluate (ATan e) = liftM atan $ evaluate e
evaluate (Abs e) = liftM abs $ evaluate e

-- The strategy for simplification:
-- Simplify obvious cases of sums and products, to give as flat as possible structure.
-- Simplify obvious cases of functions and their inverses: log(exp(x)) = x etc.
-- Multiply out all of the products of sums.
-- Simplify all the constants.
-- Get all fractions over a common denominator in sums and combine them.
-- Cancel variables and constants only - anything else is beyond the scope of this (already complicated) function.
-- This does mean that many divisions with an obvious answer will not work, simply because it can only cancel variables.
-- To anyone asking why i would possibly want to make all the functions monadic, 
-- a) to keep the API consistent and b) because (only) cstsimpl may throw a DivisionByZero error.

-- The constraints on an Expr (in terms of var and const) that must be present in order to simplify it.
type Simplifiable var const = (Ord const, Ord var, Eq var, IsIntegral const, Floating const)

-- Simplify an algebraic expression.
simplify :: Simplifiable var const => Expr var const -> Compute (Expr var const)
simplify e = do
    e' <- simplifyPass e -- Apply one pass of simplification.
    if e' == e -- If it does not change, we're done here.
        then return e'
        else simplify e' -- Otherwise do it again.

simplifyEqn :: Simplifiable var const => Equation var const -> Compute (Equation var const)
simplifyEqn (Equation a b) = Equation <$> (simplify a) <*> (simplify b)

-- Apply one round of simplification.
simplifyPass :: Simplifiable var const => Expr var const -> Compute (Expr var const)
simplifyPass e = foldlM runPass e passes
    where passes = [Pass mulmul True, Pass sumsum True, Pass mulsum True,
                    Pass idsum True, Pass idmul True, Pass idpow True, Pass iddiv True,
                    Pass invle True, Pass intrg True,
                    Pass mulmul True, Pass muldiv True, Pass sumsum True,
                    Pass cstsimpl True,
                    Pass idsum True, Pass idmul True, Pass idpow True, Pass iddiv True,
                    Pass mulmul True, Pass muldiv True, Pass sumsum True,
                    Pass kvmul True, Pass kvpow True, Pass kvdiv True,
                    Pass idsum True, Pass idmul True, Pass idpow True, Pass iddiv True,
                    Pass mulmul True, Pass muldiv True, Pass sumsum True,
                    Pass lksum True,
                    Pass idsum True, Pass idmul True, Pass idpow True, Pass iddiv True,
                    Pass mulmul True, Pass muldiv True, Pass sumsum True,
                    Pass lkmul True, Pass inmul True,
                    Pass idsum True, Pass idmul True, Pass idpow True, Pass iddiv True,
                    Pass mulmul True, Pass muldiv True, Pass sumsum True,
                    Pass idsum True, Pass idmul True, Pass idpow True, Pass iddiv True,
                    Pass smdiv True,
                    Pass idsum True, Pass idmul True, Pass idpow True, Pass iddiv True,
                    Pass mulmul True, Pass muldiv True, Pass sumsum True,
                    Pass kvmul True, Pass kvpow True, Pass kvdiv True,
                    Pass cvdiv True,
                    Pass idsum True, Pass idmul True, Pass idpow True, Pass iddiv True,
                    Pass mulmul True, Pass muldiv True, Pass sumsum True,
                    Pass cstrnd True]
            -- The order of passes was found by trial and error - whatever order you use will give a _technically_ correct answer
            -- but using this order makes it look nice too.

-- A single simplification pass.
-- contains a function to apply to the expression, and a bool stating whether this should be applied recursively.
data Pass = Pass PassFunc Bool

-- A function to apply to an expression during a simplification pass
type PassFunc = forall var const. Simplifiable var const => Expr var const -> Compute (Expr var const)

--Apply a pass to an expression, recursively if needed.
runPass :: Simplifiable var const => Expr var const -> Pass -> Compute (Expr var const)
runPass e (Pass f False) = f e
runPass e p@(Pass _ True) = runPass' e p

-- Recursively apply a pass to an expression, managing the monadic effects of the pass
runPass' :: Simplifiable var const => Expr var const -> Pass -> Compute (Expr var const)
runPass' (Sum as) p@(Pass f _) = (Sum <$> mapM (flip runPass' p) as) >>= f
runPass' (Mul as) p@(Pass f _) = (Mul <$> mapM (flip runPass' p) as) >>= f
runPass' (Div a b) p@(Pass f _) = (liftM2 Div (runPass' a p) (runPass' b p)) >>= f
runPass' (Pow a b) p@(Pass f _) = (liftM2 Pow (runPass' a p) (runPass' b p)) >>= f
runPass' (Exp x) p@(Pass f _) = liftM Exp (runPass' x p) >>= f
runPass' (Log x) p@(Pass f _) = liftM Log (runPass' x p) >>= f
runPass' (Sin x) p@(Pass f _) = liftM Sin (runPass' x p) >>= f
runPass' (Cos x) p@(Pass f _) = liftM Cos (runPass' x p) >>= f
runPass' (Tan x) p@(Pass f _) = liftM Tan (runPass' x p) >>= f
runPass' (ASin x) p@(Pass f _) = liftM ASin (runPass' x p) >>= f
runPass' (ACos x) p@(Pass f _) = liftM ACos (runPass' x p) >>= f
runPass' (ATan x) p@(Pass f _) = liftM ATan (runPass' x p) >>= f
runPass' (Abs x) p@(Pass f _) = liftM Abs (runPass' x p) >>= f
runPass' x (Pass f _) = f x

-- Identity simplification passes: removing additions of zero, and multiplications, divisions and exponentiations by one.
idsum :: PassFunc
idsum (Sum [x]) = return x
idsum (Sum []) = return 0 -- Empty sum
idsum (Sum x) = return $ Sum $ filter (\x -> case x of
    Const v -> not $ closeEnough 0.0000001 0 v
    _ -> True) x
idsum x = return x

idmul :: PassFunc
idmul (Mul [x]) = return x
idmul (Mul []) = return 1 -- Empty product
idmul (Mul x) = return $ Mul $ filter (\x -> case x of
    Const v -> not $ closeEnough 0.0000001 1 v
    _ -> True) x
idmul x = return x

closeEnough :: (Ord e, Num e) => e -> e -> e -> Bool
closeEnough e a b = abs (a - b) <= e

iddiv :: PassFunc
iddiv (Div a 1) = return a
iddiv x = return x

idpow :: PassFunc
idpow (Pow x 1) = return x
idpow x = return x

-- Inverse simplification passes: simplify things like log(exp(x)) = x, sin(asin(x)) = x etc.

invle :: PassFunc
invle (Log (Exp x)) = return x
invle (Exp (Log x)) = return x
invle x = return x

intrg :: PassFunc
intrg (Sin (ASin x)) = return x
intrg (ASin (Sin x)) = return x
intrg (Cos (ACos x)) = return x
intrg (ACos (Cos x)) = return x
intrg (Tan (ATan x)) = return x
intrg (ATan (Tan x)) = return x
intrg x = return x

-- Known value passes: remove multiplications by zero, powers of 1, powers of zero and division of something by itself.
kvmul :: PassFunc
kvmul (Mul x) | 0 `elem` x = return 0
kvmul x = return x

kvpow :: PassFunc
kvpow (Pow 1 x) = return 1
kvpow (Pow x 0) = return 1
kvpow (Pow 0 x) = return 0 -- Putting this line _after_ the previous guarantees 0^0 == 1
kvpow x = return x

kvdiv :: PassFunc
kvdiv (Div a b) | a == b = return 1
kvdiv x = return x

-- Constant simplification pass: evaluates constant arithmetic
cstsimpl :: PassFunc
cstsimpl (Sum x) | any isConst x = let (consts, rest) = partition isConst x in 
    if closeEnough 0.0000001 0 $ sum $ map constPart consts
        then return $ Sum rest 
        else return $ Sum $ (Const $ sum $ map constPart consts) : rest
cstsimpl (Mul x) | any isConst x = let (consts, rest) = partition isConst x in 
    if closeEnough 0.0000001 0 $ product $ map constPart consts 
        then return 0
        else if closeEnough 0.0000001 1 $ product $ map constPart consts
            then return $ Mul rest
            else return $ Mul $ (Const $ product $ map constPart consts) : rest
    where closeEnough e a b = abs (a - b) <= e
cstsimpl (Pow (Const a) (Const b)) = return $ Const $ a ** b
cstsimpl (Div a 0) = throwError DivisionByZero
cstsimpl (Div (Const a) (Const b)) = return $ Const $ a / b
cstsimpl (Div a (Const b)) = return $ Mul [Const (recip b), a]
cstsimpl (Exp (Const a)) = return $ Const $ exp a
cstsimpl (Log (Const a)) = return $ Const $ log a
cstsimpl (Sin (Const a)) = return $ Const $ sin a
cstsimpl (Cos (Const a)) = return $ Const $ cos a
cstsimpl (Tan (Const a)) = return $ Const $ tan a
cstsimpl (ASin (Const a)) = return $ Const $ asin a
cstsimpl (ACos (Const a)) = return $ Const $ acos a
cstsimpl (ATan (Const a)) = return $ Const $ atan a
cstsimpl (Abs (Const a)) = return $ Const $ abs a
cstsimpl x = return x

isConst :: Expr var const -> Bool
isConst (Const _) = True
isConst _ = False

constPart :: Expr var const -> const
constPart (Const a) = a
constPart _ = undefined

-- Round all constants to 5 d.p we really don't need any more than that.

cstrnd :: PassFunc
cstrnd (Const c) = return $ Const $ (fromIntegral $ fromJust $ asInt $ c * (10^5)) / 100000
cstrnd x = return x

-- Simplify combinations of sums and products

--sums of sums are just sums
sumsum :: PassFunc
sumsum (Sum xs) = return $ Sum $ concat $ map parts xs
    where parts (Sum xs) = xs
          parts x = [x]
sumsum x = return x

--products of products are just products
mulmul :: PassFunc
mulmul (Mul xs) = return $ Mul $ concat $ map parts xs
    where parts (Mul xs) = xs
          parts x = [x]
mulmul x = return x

--multiply out products of sums
mulsum :: PassFunc
mulsum (Mul xs) = case sums of -- Do we have any sums in our product?
    [] -> return $ Mul xs -- If no, just give it back unchanged
    _ -> let s@(Sum ys) = head $ sums in -- else get the first one
            return $ Sum $ map (\x -> Mul $ x: removingOne s xs) ys
            -- remove one copy of the thing we just multiplied by 
            -- and distribute the remaining expressions over the elements of the sum
    where sums = filter isSum xs
mulsum x = return x

muldiv :: PassFunc
mulduv (Mul [Const c, b]) = Mul [Const c, b]
muldiv (Mul xs) = case denoms of
    [] -> return $ Mul numers
    _ -> return $ Div (Mul numers) (Mul denoms)
    where numers = map numer xs
          denoms = concatMap denom xs
          numer (Div a b) = a
          numer x = x
          denom (Div a b) = [b]
          denom _ = []
muldiv x = return x

isSum :: Expr var const -> Bool
isSum (Sum _) = True
isSum x = False

--Collect like terms

lksum :: PassFunc
lksum (Sum xs) = return $ Sum [Mul [Const c, x] | (c, x) <- zip coeffs bases]
    where bases = nub $ map noCoeff xs
          coeffs = [sum $ map onlyCoeff $ filter ((== x) . noCoeff) xs | x <- bases]
lksum x = return x

noCoeff :: Expr var const -> Expr var const
noCoeff (Mul xs) | (any isConst xs) && (not $ all isConst xs) = case filter (not . isConst) xs of
    [x] -> x
    xs' -> Mul xs'
noCoeff x = x

onlyCoeff :: Num const => Expr var const -> const
onlyCoeff (Mul xs) | (any isConst xs) && (not $ all isConst xs) = let Const c = head $ filter isConst xs in c
onlyCoeff x = 1

lkmul :: PassFunc
lkmul (Mul xs) = return $ Mul $ [Pow x c | (c, x) <- zip pows bases] ++ sums
    where bases = nub $ map noPower rest
          pows = [sum $ map onlyPower $ filter ((== x) . noPower) rest | x <- bases]
          (sums, rest) = partition isSum xs
lkmul x = return x

noPower :: Expr var const -> Expr var const
noPower (Pow a b) = a
noPower x = x

onlyPower :: Num const => Expr var const -> Expr var const
onlyPower (Pow a b) = b
onlyPower x = 1

-- lkmul will leave sums alone entirely, so sums will only be multiplied out if they are 
-- represented as a Mul of a repeated value so convert sums to integer powers into repeated muls.
inmul :: PassFunc
inmul (Pow s@(Sum _) (Const c)) | isIntegral c = return $ Mul $ replicate (fromJust $ asInt c) s
inmul x = return x

-- Simplify sums of quotients by finding a common denominator

smdiv :: PassFunc
smdiv (Sum xs) | any isDiv xs = return $ Div (Sum $ divs' ++ rest') (Mul cd)
    where (divs, rest) = partition isDiv xs
          divparts = map unDiv divs
          cd = map snd divparts
          divs' = [Mul [Mul $ removingOne d cd, n] | (n, d) <- divparts]
          rest' = [Mul [Mul cd, r] | r <- rest]
          unDiv (Div a b) = (a, b)
smdiv x = return x

isDiv :: Expr var const -> Bool
isDiv (Div _ _) = True
isDiv _ = False

removingOne :: Eq a => a -> [a] -> [a]
removingOne a = uncurry (++) . bimap tail id . partition (== a)
--                   |               |         split the input into two lists:
--                   |               |         one of all the a's, and the other the rest
--                   |        remove one element of the list of a's
--              recombine the lists 


-- Cancel variables on quotients

cvdiv :: PassFunc
cvdiv c@(Div a b) = return $ (Div `on`) (flip (foldr ($)) $ map (uncurry removePower) cancelVars) a b
    where cancelVars = freeVariables $ Sum [a, b]
cvdiv x = return x

removePower :: (Ord const, Num const, Eq var) => var -> const -> Expr var const -> Expr var const
removePower x 1 (Var y) | x == y = 1
removePower x p (Pow (Var y) (Const c)) | x == y = Pow (Var y) (Const $ max (c - p) 0)
removePower x p (Div a b) = Div (removePower x p a) b
removePower x p (Mul xs) = Mul $ map (removePower x p) xs
removePower x p (Sum xs) = Sum $ map (removePower x p) xs
removePower _ _ x = x

-- Find the highest common integer powers of free variables in an expression
freeVariables :: (Ord const, Num const, Eq var) => Expr var const -> [(var, const)]
freeVariables (Var x) = [(x, 1)]
freeVariables (Sum xs) = map (minimumBy (compare `on` snd)) $ -- find the minimum power of each variable. ^
                         filter ((== length xs) . length) $ -- remove the variables which are not present in all elements. ^
                         groupBy ((==) `on` fst) $  -- group them by which variable they correspond to ^
                         concatMap freeVariables xs -- get the free variables of all the components of the sum ^
--When summing, we select the lowest power, as that must be the highest common power.
freeVariables (Mul xs) = filter ((/= 0) . snd) $ -- remove the terms of power 0
                         map (bimap head sum . unzip) $  
                         -- the same thing only we add the powers. and dont care if it doesnt appear in all elements
                         groupBy ((==) `on` fst) $ 
                         concatMap freeVariables xs
--Multiplying adds indices
freeVariables (Div a b) = freeVariables a
freeVariables (Pow (Var x) (Const c)) = [(x, c)]
freeVariables (Pow a _) = []
freeVariables _ = []
