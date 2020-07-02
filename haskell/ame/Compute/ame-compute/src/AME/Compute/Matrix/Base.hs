module AME.Compute.Matrix.Base where

import AME.Compute.Expr
import AME.Compute.Error
import AME.Compute.Simplify
import Control.Monad
import Data.Maybe

--A matrix is represented as a list of rows of data, and is parametrised over
--a type so any data can be stored
data Matrix t = Matrix {
    rows :: Int,
    columns :: Int,
    values :: [[t]]
} deriving (Eq, Show, Ord)

--Construct a matrix from a list of rows
matrix :: [[t]] -> Matrix t
matrix ts = Matrix (length ts) (length $ head ts) ts

--Construct the identity matrix for a given size by
--append rows and columns to a smaller identity matrix
identity :: Num t => Int -> Matrix t
identity 1 = matrix [[1]]
identity n = matrix $ (1 : replicate (n - 1) 0) : [0 : row | row <- values $ identity $ n - 1]

--The instance of functor for matrix
--maps a function over all the data values in the matrix
instance Functor Matrix where
    fmap f = matrix . map (map f) . values

--Scale a matrix by a scalar
scale :: Num t => t -> Matrix t -> Matrix t
scale = fmap . (*)

--A vector is simply a matrix with 1 column of data
type Vector t = Matrix t

--Construct a vector from a list by wrapping each element in its own row.
vector :: [t] -> Vector t
vector = matrix . map return

--Check if a matrix is a column vector by check that every row has only one element
isVector :: Matrix t -> Bool
isVector m = (== 1) $ length $ head $ values m

--The dot product of two vectors is the sum of the pointwise product of their components
dot :: Num t => Vector t -> Vector t -> t
dot a b = sum $ zipWith (*) (head $ values $ a)  (head $ values $ b)

--The generalized pythagorean theorem is used to compute magnitudes
magnitude :: Floating t => Matrix t -> t
magnitude a = sqrt $ sum $ map (**2) $ concat $ values a

normalize :: Floating t => Matrix t -> Matrix t
normalize v = (recip $ magnitude v) `scale` v

row :: Int -> Matrix t -> Vector t
row i m = matrix $ return $ values m !! (i - 1)

column :: Int -> Matrix t -> Vector t
column i m = matrix $ return $ map (!! (i - 1)) $ values m

--The determinant is computed using a laplace expansion on the first row of the matrix
--This is computed as follows:
--  * For the ith element of the first row,
--  * Remove the ith row and ith column of the matrix,
--  * Compute the determinant of that matrix ("(i, i) cofactor")
--  * Multiply it by the element of the first row
--  * Repeat for all elements and sum
--This is particularly inefficient (O(n!)) but will be sufficient for our purposes
determinant :: Num t => Matrix t -> Compute t
determinant m | (rows m == 1) && (columns m == 1) = return $ head $ head $ values m
determinant m | (rows m == columns m) = sum <$> (mapM (\(i, x) -> (x *) <$> (cofactor 1 i m)) $ enumerate $ head $ values m)
determinant m = throwError $ MatrixShouldBeSquare (rows m) (columns m)

--The (i, j) minor is the determinant of the matrix with the ith row and jth columns removed.
minor :: Num t => Int -> Int -> Matrix t -> Compute t
minor i j m = determinant $ matrix [[val | (l, val) <- enumerate row, l /= j] | (k, row) <- enumerate $ values m, k /= i] 

--The (i, j) cofactor matrix is simply the (i, i) minor,
--only negated if i + j is odd 
cofactor :: Num t => Int -> Int -> Matrix t -> Compute t
cofactor i j m = (if even $ i + j then id else negate) <$> minor i j m

--enumerate [a, b, c, d] = [(1, a), (2, b), (3, c), (4, d)]
enumerate :: [t] -> [(Int, t)]
enumerate = zip [1..]

--We compute the inverse matrix using the cofactor formula.
inverse :: (Eq t, Fractional t) => Matrix t -> Compute (Matrix t)
inverse m = do
    when (rows m /= columns m) $ throwError $ MatrixShouldBeSquare (rows m) (columns m)
    --We can only invert square matrices
    det <- determinant m
    --Compute the determinant of the matrix
    if det == 0
        then throwError MatrixNotInvertible
        --If its zero, we give up
        else do
            let row j = mapM id [cofactor i j m | i <- [1..columns m]]
            --Compute a row of the inverse matrix by taking the (i, j) cofactors
            --Where i = 1 -> n, and j is the row number, for an nxn matrix.
            values <- mapM row [1..rows m]
            --Assemble the inverse matrix from the rows
            let mat = matrix values
            return $ (recip det) `scale` mat
            --Scale it down by the determinant

--A dummy variable used for finding eigenvalues.
data Lambda = Lambda deriving (Show, Eq, Ord)

--The characteristic equation is the polynomial whose roots are the eigenvalues of
--a given matrix. We can construct it easily:
--The eigenvalue equation is: Mv = lv (l is the eigenvalue, v the eigenvector)
--So if we replace l with lI, it should make no difference to v, Mv = Ilv
--Now we can move the rhs over to the lhs, Mv - Ilv = 0
--And factorize out the v: (M - Il)v = 0
--since the matrix (M - Il) takes v to zero, we know that (M - Il)
--must have determinant zero. So by forming a matrix of expressions, in terms of l
--we can use our determinant function to form an equation in l.
characteristic :: (Ord t, Floating t) => Matrix t -> Compute (Expr Lambda t)
characteristic m = determinant $ fmap Const m `subtract` ((Var Lambda) `scale` identity (rows m))
    where subtract a' b' = matrix $ zipWith (zipWith (-)) (values a') (values b')