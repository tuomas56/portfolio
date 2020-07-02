module AME.Compute.Statistics where

import AME.Compute.Error
import Data.Function
import Data.List 
import Data.Maybe
import Data.Ord

--A data table has some columns with variable names and then rows of data.
data Table var val = Table [var] [[val]]
                   deriving (Eq, Show)

--get the values of a particular variable in a table
dataValues :: Eq var => var -> Table var val -> [val]
dataValues var (Table labels dat) = snd $ head $ filter ((== var) . fst) $ zip labels dat

--compute the mean as sigma x / n
mean :: (Eq var, Fractional val) => var -> Table var val -> val
mean var tbl = (sum xs) / (fromIntegral $ length xs)
    where xs = dataValues var tbl

--the sample mean is the same
sampleMean :: (Eq var, Fractional val) => var -> Table var val -> val
sampleMean var tbl = (sum xs) / (fromIntegral (length xs))
    where xs = dataValues var tbl

--compute the median
median :: (Eq var, Ord val, Fractional val) => var -> Table var val -> val
median var tbl = median' xs
    where xs = sort $ dataValues var tbl
          median' [a] = a
          median' [a, b] = (a + b) / 2
          median' (_:xs) = median' (init xs)

--compute the mode by finding the element thats mentioned the most times
mode :: (Eq var, Ord val, Eq val) => var -> Table var val -> val
mode var tbl = last $ sortOn count $ sortOn Down $ nub xs
 --by taking one of...finding the longest sublist.. of data grouped by its values
    where xs = dataValues var tbl
          count v = length $ filter (== v) xs

--compute variance as sigma x^2/n - xbar^2
variance :: (Eq var, Fractional val) => var -> Table var val -> val
variance var tbl = (sum $ map (^2) xs) / (fromIntegral $ length xs) - (sum xs)^2 / ((fromIntegral $ length xs)^2)
    where xs = dataValues var tbl

--and sample variance is the same except times n/(n - 1)
sampleVariance :: (Eq var, Fractional val) => var -> Table var val -> val
sampleVariance var tbl = ((sum $ map (^2) xs) - (sum xs)^2 / (fromIntegral $ length xs)) / (fromIntegral (length xs) - 1)
        where xs = dataValues var tbl

--standard deviation = sqrt(variance)
standardDeviation :: (Eq var, Floating val) => var -> Table var val -> val
standardDeviation var tbl = sqrt $ variance var tbl

--sample standard deviation = sqrt(sample variance)
sampleStandardDeviation :: (Eq var, Floating val) => var -> Table var val -> val
sampleStandardDeviation var tbl = sqrt $ sampleVariance var tbl

--compute the chi-squared value by doing sigma (o - e)^2/e for all observed, o,
--and expected, e, values.
chiSquared :: (Eq var, Fractional val) => var -> var -> Table var val -> val
chiSquared vara varb tbl = sum $ zipWith (/) (map (^2) $ zipWith (-) xs ys) ys
--                         sum ..divide by e..square.....subtract o from e
    where xs = dataValues vara tbl
          ys = dataValues varb tbl

--pearson correlation is the pmcc of the raw data
pearsonCorrelation :: (Eq var, Floating val) => var -> var -> Table var val -> val
pearsonCorrelation vara varb tbl = pearsonCorrelation' xs ys
    where xs = dataValues vara tbl
          ys = dataValues varb tbl

-- the product moment correlation coefficient (pmcc)
pearsonCorrelation' :: Floating t => [t] -> [t] -> t
pearsonCorrelation' xs ys = (s xs ys) / (sqrt $ (s xs xs) * (s ys ys))
        --r = sxy / sqrt(sxx * syy)
    where s xs ys = (sum $ zipWith (*) xs ys) - (sum xs) * (sum ys) / (fromIntegral $ length xs)
        --sab = sigma a*b - sigma a * sigma b / n
          
--spearman correlation is almost the same, except its the pmcc of the ranks
spearmanCorrelation :: (Eq var, Eq val, Ord val, Floating val) => var -> var -> Table var val -> val
spearmanCorrelation vara varb tbl = pearsonCorrelation' xs' ys'
    where xsorted = zip (sort xs) ([0..] :: [Int])
          ysorted = zip (sort ys) ([0..] :: [Int])
          --sort them first so they're in the right order
          xs' = map (\x -> fromIntegral $ fromJust $ lookup x xsorted) xs
          ys' = map (\y -> fromIntegral $ fromJust $ lookup y ysorted) ys
          --find the ranks of the values
          xs = dataValues vara tbl
          ys = dataValues varb tbl
