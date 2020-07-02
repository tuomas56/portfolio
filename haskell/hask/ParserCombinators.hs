{-# LANGUAGE Rank2Types #-}

module ParserCombinators where

import Prelude
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Data.Monoid
import Debug.Trace

type Parse a = ExceptT ParseError Identity a

newtype Parser a b = Parser { 
    parse :: Eq a => [a] -> Maybe (b, [a])
}

data ParseError = NoFinishError 
                | NoParseError

instance Show ParseError where
    show NoFinishError = "Parser did not consume the whole input string!"
    show NoParseError = "Parser could not parse input!"

instance Eq a => Functor (Parser a) where
    fmap = liftM
    
instance Eq a => Applicative (Parser a) where
    pure = return
    (<*>) = ap

instance Eq a => Monad (Parser a) where
    return = insert
    a >>= b = Parser $ \s -> do
        (ra, s') <- parse a s
        (rb, s'') <- parse (b ra) s'
        return (rb, s'')

--matches x such that p(x) is true
some :: (a -> Bool) -> Parser a a    
some p = Parser $ some_ p

some_ :: (a -> Bool) -> [a] -> Maybe (a, [a])
some_ a (x:xs) | a x     = Just (x, xs)
some_ _ _ = Nothing

--matches anything but a
isnot :: Eq a => a -> Parser a a
isnot a = some (/= a)

--matches a exactly
unit :: a -> Parser a a
unit a = Parser $ unit_ a

unit_ :: (Eq a) => a -> [a] -> Maybe (a, [a])
unit_ a (x:xs) | x == a    = Just (x, xs)
               | otherwise = Nothing
unit_ _ _                  = Nothing

--consumes nothing and returns a
insert :: a -> Parser b a
insert a = Parser (\x -> Just (a, x))

--consumes nothing and fails
wrong :: Parser b a
wrong = Parser $ const Nothing

--matches a or nothing, returns Maybe a
opt :: (Eq a, Eq b) => Parser a b -> Parser a (Maybe b)
opt a = (a ~> Just) <|> insert Nothing

--matches one or more of a
onePlus :: (Eq a, Eq b) => Parser a b -> Parser a [b]
onePlus a = (a <:> many a) <|> (a ~> (:[]))

--matches zero or more of a
many :: (Eq a, Eq b) => Parser a b -> Parser a [b]
many a = (opt . onePlus) a ~> many_
    where many_ (Just x) = x
          many_ Nothing  = []

--matches consecutive elements of a exactly
string :: Eq a => [a] -> Parser a [a] 
string = foldr ((<:>) . unit) (insert [])

--match a, then b and return b
(<>>) :: Eq a => Parser a b -> Parser a c -> Parser a c
(<>>) = (>>)

--match a, then b and return a
(<<>) :: Eq a => Parser a b -> Parser a c -> Parser a b
(<<>) = liftM2 const

--matches a, then b and cons a to b, for all applicative monoids. For arrays, use <:> as it is more efficient.
(<$:>) :: (Eq a, Applicative m, Monoid (m b), Eq b) => Parser a b -> Parser a (m b) -> Parser a (m b)
(<$:>) = liftM2 (\a b -> pure a <> b)

--matches a, then b and joins them together, for all monoids.
(<<>>) :: (Monoid m, Eq s) => Parser s m -> Parser s m -> Parser s m
(<<>>) = liftM2 (<>)

--efficient eqivalents to <$:> and <<>> for arrays
(<:>) :: Eq a => Parser a b -> Parser a [b] -> Parser a [b]
(<:>) = liftM2 (:)

(<++>) :: Eq a => Parser a [b] -> Parser a [b] -> Parser a [b]
(<++>) = (<<>>)

--matches a then b, returns (a, b)
(<+>) :: (Eq a, Eq b, Eq c) => Parser a b -> Parser a c -> Parser a (b, c)
(<+>) = liftM2 (,)
     
--tries to match a, if it fails, tries to match b
(<|>) :: Eq b => Parser a b -> Parser a b -> Parser a b
(<|>) a b = Parser $ alt_ a b

alt_ :: (Eq a, Eq b) => Parser a b -> Parser a b -> [a] -> Maybe (b, [a])
alt_ a b s = case ra of
        Nothing -> parse b s
        x       -> x
        where ra = parse a s
       
--matches a then returns (f a) 
(~>) :: (Eq a, Eq b, Eq c) => Parser a b -> (b -> c) -> Parser a c
(~>) = flip fmap

choice :: Eq a => [Parser b a] -> Parser b a
choice = foldl1 (<|>)

--parse s with p
run :: (Show a, Eq a, Eq b) => Parser a b -> [a] -> Parse b
run p s = case res of
    Just (x, [])  -> return x
    Just (_, x)   -> trace (show x) $ throwE NoFinishError
    Nothing       -> throwE NoParseError
    where res = parse p s

runParse :: Parse a -> Either ParseError a
runParse = runIdentity . runExceptT
