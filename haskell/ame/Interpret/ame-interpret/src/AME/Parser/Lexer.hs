module AME.Parser.Lexer (
    lex,
    Token(..),
    string,
    number,
    exactSt,
    exactSp,
    exactWords
) where

-- Use our parser combinator library to create a lexer.
-- A lexer takes the raw input string and turns it into a sequence of tokens.
-- This makes it easier to do things like ignoring whitespace, and differentiating 
-- punctuation and identifiers

import Prelude hiding (lex)
import AME.Parser.Combinators
import Control.Applicative
import Control.Monad

-- A token can either be a string, punctuation or a number of a newlines
-- whitespace is not present because it is automatically filtered out.
data Token = String String
           | Special Char
           | Num Double
           | Newline
           deriving (Show, Eq)

isString :: Token -> Bool
isString (String _) = True
isString _ = False

isNum :: Token -> Bool
isNum (Num _) = True
isNum _ = False

-- Convenience function to match any string
-- Used for the main parser
string :: Parser Token String
string = (\(String s) -> s) <$> match isString

-- Match an exact given string
exactSt :: String -> Parser Token ()
exactSt = exact . String

-- Match an exact punctuation character
exactSp :: Char -> Parser Token ()
exactSp = exact . Special

-- Match exactly the given words. This is different from exactSt
-- because due to the lexer, there are no spaces in String's.
-- This allows more freedom in parsing more than one space between words.
exactWords :: String -> Parser Token ()
exactWords = mapM_ (exact . String) . words

number :: Parser Token Double
number = (\(Num n) -> n) <$> match isNum

-- The actual lexer parser functions:

-- Match one newline. All the lexer parsers have nice error messages.
parseNewline :: Parser Char Token
parseNewline = expecting (Describe "a newline") $ do
    exact '\n'
    return Newline

parseNumber :: Parser Char Token
parseNumber = expecting (Describe "a number") $ do
    minus <- opt $ exact '-' -- Maybe a minus sign
    lhs <- some digit -- Followed by some digits
    rhs <- opt $ do
        exact '.' -- and maybe a decimal point
        some digit -- and some more digits.
    let num = case rhs of
            Just rhs' -> lhs ++ "." ++ rhs' -- Put all these bits together as one string
            Nothing -> lhs
    return $ Num $ case minus of
        Nothing -> read num 
        Just () -> negate $ read num -- And read it into a float.
    where digit = match (`elem` "0123456789")

parseSpecial :: Parser Char Token
parseSpecial = expecting (Describe "a punctuation character") $ Special <$> match (`elem` specials)
    where specials = "[]()-_+,:;^*/|={}"

parseString :: Parser Char Token
parseString = expecting (Describe "a string") $ do
    first <- match (`elem` alpha) -- The first character of a string is only alphabetical
                                  -- Not alphanumeric
    rest <- many $ match (`elem` alphanumeric) -- The rest are alphanumeric
    return $ String $ first : rest -- Combine the characters to make a string.
    where alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
          numeric = "0123456789"
          alphanumeric = alpha ++ numeric

-- A token can be any one of the cases decribed above.
parseToken :: Parser Char Token
parseToken = expecting (Describe "a newline, number, punctuation character or string") $
    parseNewline <|> parseNumber <|> parseSpecial <|> parseString

parseSpaces :: Parser Char ()
parseSpaces = void $ many $ exact ' '

-- The main lexer parser allows spaces basically anywhere in the input
-- parsing them and throwing them away when encountered.
lexer :: Parser Char [Token]
lexer = do
    parseSpaces
    toks <- many (parseSpaces >> parseToken)
    parseSpaces
    done
    return toks

lex :: String -> Either (ParseError Char) [Token]
lex = fmap snd . parse lexer
