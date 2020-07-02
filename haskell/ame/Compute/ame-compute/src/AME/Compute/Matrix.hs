{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, LambdaCase #-}

module AME.Compute.Matrix (
  Matrix(..),
  MatrixExt(..),
  matrix,
  scale,
  Vector,
  vector,
  dot,
  magnitude,
  normalize,
  row,
  column,
  determinant,
  inverse,
  isMatrix,
  isScalar,
  asMatrix,
  asScalar,
  determinantE,
  dotE,
  angleE,
  magnitudeE,
  characteristic,
  Lambda(..),
  eigenvalues,
  eigenvaluesE,
  eigenvectors,
  eigenvectorsE
) where

import AME.Compute.Expr
import AME.Compute.Error
import AME.Compute.Simplify
import AME.Compute.Solver
import AME.Compute.Matrix.Base
import Control.Monad
import Data.Maybe
    
-- A type which extends a given domain to include arbitrary dimension matrices.
-- i.e if R is the reals, then MatrixExt R is the reals plus real matrices. 
-- As not all operations are defined numeric class instances are defined for Compute (MatrixExt t).
data MatrixExt t = MatrixV (Matrix t) | ScalarV t 
                 deriving (Show, Eq, Ord)

--fmap'ing over a matrixext will map over all data in a matrix,
--or just apply the function to a scalar
instance Functor MatrixExt where
    fmap f (MatrixV m) = MatrixV $ fmap f m
    fmap f (ScalarV v) = ScalarV $ f v

isMatrix :: MatrixExt t -> Bool
isMatrix (MatrixV _) = True
isMatrix _ = False

asMatrix :: MatrixExt t -> Matrix t
asMatrix (MatrixV m) = m

isScalar :: MatrixExt t -> Bool
isScalar = not . isMatrix

asScalar :: MatrixExt t -> t
asScalar (ScalarV t) = t

--an error sensitive version of determinant that
--does type checking to ensure you pass it a matrix.
determinantE :: Num t => Compute (MatrixExt t) -> Compute (MatrixExt t)
determinantE a = do
    a' <- a
    case a' of
        (MatrixV m) -> ScalarV <$> determinant m
        (ScalarV _) -> throwError $ MatrixOperationNotSupported "Can't find determinant of scalar!"
        

dotE :: Num t => Compute (MatrixExt t) -> Compute (MatrixExt t) -> Compute (MatrixExt t)
dotE a b = do
    a' <- a
    b' <- b
    --evaluate the errors in a and b
    case (a', b') of
        --Check that we are passed two vectors
        (MatrixV ma, MatrixV mb) -> if isVector ma && isVector mb
                                        then return $ ScalarV $ sum $ zipWith (*) (concat $ values ma) (concat $ values mb)
                                        --then take the sum of the pointwise product of their values
                                        else throwError $ MatrixOperationNotSupported "Cannot dot two matrices that are not column vectors!"
        (ScalarV _, ScalarV _) -> throwError $ MatrixOperationNotSupported "Cannot dot two scalars!"
        _ -> throwError $ MatrixOperationNotSupported "Cannot dot a vector and scalar!"

magnitudeE :: Floating t => Compute (MatrixExt t) -> Compute (MatrixExt t)
magnitudeE a = do
    a' <- a
    case a' of
        (ScalarV _) -> throwError $ MatrixOperationNotSupported "Cannot find magnitude of a scalar!"
        (MatrixV ma) -> if isVector ma
                            then return $ ScalarV $ magnitude ma
                            else throwError $ MatrixOperationNotSupported "Cannot find magnitude of matrix that is not a vector!"

--Find the angle between two vectors using the following relation:
--a `dot` b = (magnitude a) * (magnitude b) * (cos angle)
angleE :: Floating t => Compute (MatrixExt t) -> Compute (MatrixExt t) -> Compute (MatrixExt t)
angleE a b = do
    a' <- a
    b' <- b
    case (a', b') of
        --Check we are given two vectors
        (MatrixV ma, MatrixV mb) -> if isVector ma && isVector mb
                                        then return $ ScalarV $ acos $ sum (zipWith (*) (concat $ values ma) (concat $ values mb)) / (magnitude ma * magnitude mb)
                                        else throwError $ MatrixOperationNotSupported "Cannot find angle between two matrices that are not column vectors!"
        (ScalarV _, ScalarV _) -> throwError $ MatrixOperationNotSupported "Cannot find angle between two scalars!"
        _ -> throwError $ MatrixOperationNotSupported "Cannot find angle between a scalar and a vector!"

--Compute the eigenvalues by finding the roots of the characteristic equation
eigenvalues :: (Ord t, Floating t, IsIntegral t) => Matrix t -> Compute [t]
eigenvalues m = do
    cr <- characteristic m
    solve Lambda $ Equation cr 0 

eigenvaluesE :: (Ord t, IsIntegral t, Floating t) => Compute (MatrixExt t) -> Compute [MatrixExt t]
eigenvaluesE a = do
    a' <- a
    case a' of
        MatrixV m -> eigenvalues m >>= (return . map ScalarV)
        ScalarV _ -> throwError $ MatrixOperationNotSupported "Cannot find eigenvalues of a scalar!"

--Find the eigenvector of a matrix given an eigenvalue using the inverse power iteration algorithm.
--This works by first computing the inverse of (M - Il') where l' is a value very close to the given eigenvalue
--and then taking a random vector and repeatedly multiplying by this by the matrix we computed earlier.
--It is important that we a) normalize (M' - Il') to keep rounding errors low and
--b) do not use exactly the eigenvalue we are given, otherwise it will multiply to zero.
eigenvector :: (Ord t, IsIntegral t, Floating t) => t -> Matrix t -> Compute (Vector t)
eigenvector v m = do
    im <- inverse m'
    --Find the inverse of M - Il'
    MatrixV res <- (return $ MatrixV $ normalize im) ** 10 * (return $ MatrixV $ vector $ replicate (rows m) 1)
                    -- normalize this......take it to the power 10 .... and multiply it by an arbitrary vector
    return res
    where m' = m `subtract` ((v + 0.01) `scale` identity (rows m))
          --  M    -     l' = l + 0.01     *        I 
          subtract a' b' = matrix $ zipWith (zipWith (-)) (values a') (values b')

--The eigenvectors of a matrix are all the eigenvectors corresponding to all the eigenvalues
eigenvectors :: (Ord t, IsIntegral t, Floating t) => Matrix t -> Compute [Vector t]
eigenvectors m = do
    vs <- eigenvalues m
    mapM (flip eigenvector m) vs

eigenvectorsE :: (Ord t, IsIntegral t, Floating t) => Compute (MatrixExt t) -> Compute [MatrixExt t]
eigenvectorsE a = do
    a' <- a
    case a' of
        MatrixV m -> (map MatrixV) <$> eigenvectors m
        ScalarV _ -> throwError $ MatrixOperationNotSupported "Cannot find eigenvectors of a scalar!"

--Define numeric instances of Compute (MatrixExt t) which has Compute
--as some operations fail on matrices.
instance Num t => Num (Compute (MatrixExt t)) where
    a + b = do
        ra <- a
        rb <- b
        when (isScalar ra && isMatrix rb || isScalar rb && isMatrix ra) $ 
            throwError $ MatrixOperationNotSupported "Cannot add a matrix and a scalar!"
        --Can only add values of the same type.
        case (isScalar ra, isScalar rb) of
            --Scalar addition is defined as usual
            (True, True) -> return $ ScalarV $ asScalar ra + asScalar rb
            (False, False) -> do
                let a' = asMatrix ra
                let b' = asMatrix rb
                if (rows a' == rows b') && (columns a' == columns b')
                    --Matrix addition only works on matrices of the same shape
                    then return $ MatrixV $ matrix $ zipWith (zipWith (+)) (values a') (values b')
                    --If they are, simply add the values pairwise.
                    else throwError $ MatrixHasWrongDimensions (rows a') (columns a') (rows b') (columns b')
    
    a * b = do
        ra <- a
        rb <- b
        case (isScalar ra, isScalar rb) of
            --Every case of matrix and scalar multiplication is defined differently ;p 
            (False, False) -> do
                let a' = asMatrix ra
                let b' = asMatrix rb
                if (rows b' == columns a')
                    --To multiply two matrices we first check that they are of the form
                    --axb and bxc respectively.
                    then return $ MatrixV $ matrix [[(l `row` a') `dot` (k `column` b') | k <- [1..columns b']] | l <- [1..rows a']]
                    --Then we find the (i, j) element of the resultant matrix as the dot product of the ith row of the first matrix
                    --with the jth column of the second matrix.
                    else throwError$ MatrixHasWrongDimensions (rows b') (columns b') (columns a') (columns b')
            (True, False) -> return $ MatrixV $ (asScalar ra) `scale` (asMatrix rb)
            (False, True) -> return $ MatrixV $ (asScalar rb) `scale` (asMatrix ra)
            --Matrix * scalar and scalar * Matrix just scale the matrix
            (True, True) -> return $ ScalarV $ (asScalar ra) * (asScalar rb)
            --scalar * scalar is defined as usual

    negate a = case runCompute a of
        Right (MatrixV a') -> return $ MatrixV $ fmap negate a'
        Right (ScalarV a') -> return $ ScalarV $ negate a'
        Left a' -> throwError a'
    fromInteger = return . ScalarV . fromInteger
    --Abs is defined as expected for scalars
    --and for matrices, it is the matrix with all the elements absolute valued.
    --This does _not_ compute the magnitude of the matrix
    --that is done by magnitude.
    abs a = case runCompute a of
        Right (MatrixV a') -> return $ MatrixV $ fmap abs a'
        Right (ScalarV a') -> return $ ScalarV $ abs a'
        Left a' -> throwError a'

    --I can't think of where we would ever use this.
    signum a = case runCompute a of
        Right (ScalarV a') -> return $ ScalarV $ signum a'
        Right (MatrixV _) -> error "Signum not supported for matrices!"
        Left a' -> throwError a'

instance (Eq t, Fractional t) => Fractional (Compute (MatrixExt t)) where
    recip a = do
        ra <- a
        if isScalar ra
            then return $ ScalarV $ recip $ asScalar ra
            else MatrixV <$> (inverse $ asMatrix ra)
    --Taking the reciprocal of a matrix just yields the inverse matrix,
    --and scalar reciprocals are defind as usual.

    fromRational = return . ScalarV . fromRational

-- But isnt this just a (constrained, endomorphic) fmap on MatrixExt?? (with an extra param) Noooo: there is more than one possible 
-- implementation of fmap for MatrixExt and here we want a slightly different one than we defined earlier.
liftME :: Floating t => (t -> t) -> String -> (Compute (MatrixExt t) -> Compute (MatrixExt t)) 
liftME f n a = do
    a' <- a
    case a' of
        (MatrixV _) -> throwError $ MatrixOperationNotSupported $ n ++ " cannot be applied to matrices!"
        (ScalarV v) -> return $ ScalarV $ f v

instance (Eq t, IsIntegral t, Floating t) => Floating (Compute (MatrixExt t)) where
    --All floating point operations are left undefined for matrices,
    --and defined as usual for scalars.
    pi = return $ ScalarV pi
    exp = liftME exp "exp"
    log = liftME log "log"
    sin = liftME sin "sin"
    cos = liftME cos "cos"
    tan = liftME tan "tan"
    asin = liftME asin "asin"
    acos = liftME acos "acos"
    atan = liftME atan "atan"
    sinh = liftME sinh "sinh"
    cosh = liftME cosh "cosh"
    tanh = liftME tanh "tanh"
    asinh = liftME asinh "asinh"
    acosh = liftME acosh "acosh"
    atanh = liftME atanh "atanh"
    --To raise a MatrixExt to a power we need to check:
    a ** b = do
        ra <- a
        rb <- b
        case (isScalar ra, isScalar rb) of
            --what type of value they are.
            (False, False) -> throwError $ MatrixOperationNotSupported "Cannot raise matrix to matrix power!"
            --If both matrices, give up
            (True, False) -> throwError $ MatrixOperationNotSupported "Cannot raise scalar to matrix power!"
            --If one we try to raise a scalar to a matrix power, give up.
            (False, True) -> if isIntegral (asScalar rb)
            --A matrix to a scalar power, is only defined for integral powers
                then product $ replicate (fromJust $ asInt $ asScalar rb) a
                --In which case we replicate the equation enough times and then multiply them all together
                else throwError $ MatrixOperationNotSupported "Cannot raise matrix to non-integral power!"
                --Otherwise give up
            (True, True) -> return $ ScalarV $ (asScalar ra) ** (asScalar rb)
            --A scalar to a scalar power is defined as usual.

--Defining IsIntegral for MatrixExt is slightly tricky:
--  * IsIntegral is well defined for ScalarV, but for MatrixV,
--  * we definately know that a matrix is _not_ an integral,
--  * but can we really  say it is not _not_ an integral?
--  * its not really meaningful.
--  * But im going to assign it false for all matrices
--  * This will prevent it from confusing the simplifier as well.
instance IsIntegral t => IsIntegral (Compute (MatrixExt t)) where
    isIntegral a = case runCompute a of
        Right (MatrixV _) -> False
        Right (ScalarV t) -> isIntegral t
        Left _ -> False
    
    asInt a = case runCompute a of
        Right (MatrixV _) -> Nothing
        Right (ScalarV t) -> asInt t
        Left _ -> Nothing
