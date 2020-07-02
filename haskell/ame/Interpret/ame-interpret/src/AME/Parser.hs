module AME.Parser (
  parse  
) where

-- The main parser. This consists of many parsers each of which parses a small
-- amount of the grammar and then a couple of parsers which combine these all together.
-- There were several alternatives to writing a parser like this using parser combinators:
-- 1) I could have used a parser generator, and then just write a grammar specification which
--    could be used to generate a parser. Tools such as happy exist to do this for Haskell.
--    This would produce a much more efficient parser. However, it adds the additional step of 
--    generating the parser on each build, as well as that it requires a lot of additional
--    code to generate the proper AST from the parse tree that it outputs.
-- 2) Use another parsing library such as attoparsec or Parsec. Both of these are alternative
--    parser combinator libraries, however they are both not ideal: attoparsec because of its
--    extremely fast performance, has no support for lexers, and limits me to using ByteString
--    instead of String, which brings other problems such as encoding and how to get it to interact
--    with the rest of the system. Parsec on the other hand, forces you to use its own lexer
--    which does not quite fit my use-case and does not use backtracking (<|>), which
--    would require a redesign of my grammar.
-- The entry point for the parser is parseStatement

import Prelude hiding (lex)
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import AME.Parser.Lexer
import AME.Parser.Combinators hiding (parse)
import qualified AME.Parser.Combinators as C
import AME.Interpret.AST
import AME.Compute.Error
import AME.Compute.Matrix
import AME.Compute.Statistics
import Debug.Trace

-- Optionally allow the user to write "a" or "the"
-- This makes the language have much better flow as
-- the expressions are more like natural english.
determiner :: Parser Token ()
determiner = void $ opt $ exactSt "a" <|> exactSt "the"

-- A combinator that takes three parsers a, b and c
-- and will parse either a then b then c or c then b then a
-- this is useful for when you have a list of properties
-- but dont care about the order. I.e "with radius 4 and center (0, 1)"
-- or "with center (0, 1) and radius 4".
-- It returns a tuple with the result of parsers a and c
eitherWay :: Parser s a -> Parser s b -> Parser s c -> Parser s (a, c)
eitherWay a b c = (do
        a' <- a
        b
        c' <- c
        return (a', c')) <|> (do
        c' <- c
        b
        a' <- a
        return (a', c'))

-- The actual parsers are below. This should be mostly self-explanatory,
-- at least, that is the goal of parser combinators.

-- Importantly, <$> is a synonym for fmap.
-- This mean that it is basically the same as $
-- So it just applies the given function to the result of the parser
-- i.e Variable <$> string, parses a string. This will give a String
-- so Variable <$> takes this string and applies Variable to it
-- giving an Expression.

-- In general, f <$> p can be rewritten as:
-- do 
--    a <- p        -- parse p
--    return $ f a  -- apply f to the result, and return it

parseVariable :: Parser Token Expression
parseVariable = Variable <$> string

-- The reason for (return . ScalarV) here is that Number = Compute (MatrixExt Double)
-- ScalarV takes the Double and converts it to a MatrixExt Double
-- Then return takes this and puts it into Compute.
parseScalar :: Parser Token Number
parseScalar = (return . ScalarV) <$> number

parseRow :: Parser Token [Double]
parseRow = do
    exactSp '['
    vals <- number `sepBy` exactSp ','
    exactSp ']'
    return vals

parseVector :: Parser Token Number
parseVector = (return . MatrixV . vector) <$> parseRow

parseMatrix :: Parser Token Number
parseMatrix = parseVector <|> do
    exactSp '['
    rows <- parseRow `sepBy` exactSp ','
    exactSp ']'
    return $ return $ MatrixV $ matrix rows

parseNumber :: Parser Token Value
parseNumber = Number <$> (parseScalar <|> parseMatrix)

parseTable :: Parser Token Value
parseTable = do
    exactWords "a table where"
            -- Here we just flip the arguments of sepBy
            -- so this is just the whole contents of the do-block,
            -- seperated by (exactSp ';')
    columns <- (`sepBy` exactSp ';') $ do
        var <- string -- A variable name
        exactSp '=' -- Then = 
        ds <- map (return . ScalarV) <$> parseRow -- Then a row of numbers
        return (var, ds)
    return $ Tbl $ uncurry Table $ unzip columns
    -- The internal representation of Table is as two lists
    -- one of variable names, and then the other of the respective lists of values.
    -- but since columns is a list of tuples (variable name, values) we
    -- apply unzip to give a tuple of two lists, as required by Table.
    -- uncurry just makes Table accept a tuple instead of two seperate arguments
    -- (unzip :: [(a, b)] -> ([a], [b]))
    -- (uncurry :: (a -> b -> c) -> ((a, b) -> c))

parseValueExpr :: Parser Token Expression
parseValueExpr = Value <$> (parseNumber <|> parseTable)

parseCircle :: Parser Token Expression
parseCircle = do
    determiner
    exactWords "circle with"
    -- In order two parse both the case where we provide
    -- the center and radius, and the center and a point,
    -- The second parser, which is either parseCirclRadius or parseCirclePoint
    -- returns a function, cont, which we can apply the center to to get
    -- out final AST.
    (center, cont) <- eitherWay
        (exactSt "center" >> parseExpr)
        (exactSt "and")
        (parseCircleRadius <|> parseCirclePoint)
    return $ cont center
    where parseCircleRadius = do
            exactSt "radius" -- In the case of center and radius
            radius <- parseExpr
            return $ flip CircleCenterRadius radius -- The function is CircleCenterRadius
          parseCirclePoint = do
            determiner
            exactSt "point" -- For center and point
            point <- parseExpr
            return $ flip CircleCenterPoint point -- The function is CircleCenterPoint

parseLine :: Parser Token Expression
parseLine = do
    determiner
    exactSt "line"
    parseLinePointGradient <|> parseLineInterceptGradient <|> parseLinePointPoint
    where parseLinePointGradient = do
            (point, gradient) <- eitherWay
                (exactWords "passing through" >> parseExpr)
                (return ()) -- Nothing between these two
                (exactWords "with gradient" >> parseExpr)
            return $ LinePointGradient point gradient
          parseLineInterceptGradient = do
            exactSt "with"
            (intercept, gradient) <- eitherWay 
                (exactSt "intercept" >> parseExpr)
                (exactSt "and")
                (exactSt "gradient" >> parseExpr)
            return $ LineInterceptGradient intercept gradient
          parseLinePointPoint = do
            exactWords "passing through" <|> exactSt "connecting"
            a <- parseExpr
            exactSt "and"
            b <- parseExpr
            return $ LinePointPoint a b

parseEquationOf :: Parser Token Expression
parseEquationOf = do
    opt $ exactSt "the"
    -- We can't use determiner here because "a equation of" would not make sense but "the equation of" does.
    exactWords "equation of"
    shape <- parseExpr
    return $ EquationOf shape

parseIntersectionOf :: Parser Token Expression
parseIntersectionOf = do
    opt $ exactSt "the"
    exactWords "intersection of"
    a <- parseExpr
    exactSt "and"
    b <- parseExpr
    return $ IntersectionOf a b

parsePoint :: Parser Token Expression
parsePoint = do
    opt $ determiner >> exactSt "point" -- "the point", "a point", "point" or nothing
    exactSp '('
    a <- parseExpr
    exactSp ','
    b <- parseExpr
    exactSp ')'
    return $ MakePoint a b

-- Parse a simple numeric function of one argument.
-- Given a function and a name, this parses name(expression)
-- and passes the expression value to the function.
-- i.e) parseListExpr SinE "sin" would parse "sin(1)" and pass 1 to SinE
parseLiftExpr :: (Expression -> Expression) -> String -> Parser Token Expression
parseLiftExpr f n = do
    exactSt n
    exactSp '('
    res <- f <$> parseExpr
    exactSp ')'
    return res

parseAbs :: Parser Token Expression
parseAbs = do
    exactSp '|'
    res <- AbsE <$> parseExpr
    exactSp '|'
    return res

-- We take the whole list of possiblities, and apply parseLiftExpr to each
-- and then foldl1 (<|>) combines these into one parser that can parser any of the list.
-- So this will parse "sin(x)", "cos(x)", .... "sqrt(x)"
parseExprFunc :: Parser Token Expression
parseExprFunc = foldl1 (<|>) $ map (uncurry parseLiftExpr)
                    [(SinE, "sin"),
                     (CosE, "cos"),
                     (TanE, "tan"),
                     (ExpE, "exp"),
                     (LogE, "log"),
                     (ASinE, "asin"),
                     (ACosE, "acos"),
                     (ATanE, "atan"),
                     (SqrtE, "sqrt")]

-- Simply parse a bracketed expression.
parseBracket :: Parser Token Expression
parseBracket = do
    exactSp '('
    e <- parseExpr
    exactSp ')'
    return e

parseSubstitute :: Parser Token Expression
parseSubstitute = do
    (var, e) <- (do                   -- Either parse 
        exactSt "substitute"          -- "substitute"
        var <- string                 -- a variable name
        exactSt "for" <|> exactSp '=' -- "for" or "="
        e <- parseExpr                -- an expression
        return (var, e)) <|> (do      -- Or parse
        exactSt "set"                 -- "set"
        var <- string                 -- a variable name
        exactSp '='                   -- "="
        e <- parseExpr                -- an expression
        return (var, e))
        -- This allows the parser to parse "substitute x for 2 in y"
        -- or "substitute x = 2 in y" or "set x = 2 in y" but not "set x for 2 in y"
    exactSt "in"
    body <- parseExpr
    return $ Substitute var e body

parseEvaluate :: Parser Token Expression
parseEvaluate = do
    exactSt "evaluate" <|> exactWords "the value of"
    e <- parseExpr
    return $ Evaluate e 

parseSolve :: Parser Token Expression
parseSolve = do
    exactSt "solve" <|> exactWords "the solution of" 
                    <|> exactWords "the solutions of" 
                    <|> exactWords "the root of" 
                    <|> exactWords "the roots of"
    e <- parseExpr
    var <- opt $ do    -- Optionally provide the variable to solve for.
        exactSt "for"
        string
    return $ Solve e var

parseSolveSimultaneous :: Parser Token Expression
parseSolveSimultaneous = do
    exactSt "solve" <|> exactWords "the solution of" 
                    <|> exactWords "the solutions of" 
                    <|> exactWords "the root of" 
                    <|> exactWords "the roots of"
    exactSp '{'
    es <- parseExpr `sepBy` (exact Newline <|> exactSp ';')
    exactSp '}'
    return $ SolveSimultaneous es

-- e.g "the variable x", "a variable x", "variable x"
parseMakeVariable :: Parser Token Expression
parseMakeVariable = do
    determiner
    exactSt "variable"
    v <- string
    return $ MakeVariable v

parseIntegral :: Parser Token Expression
parseIntegral = do
    exactSt "integrate" <|> exactWords "the integral of"
                        <|> exactWords "the indefinite integral of"
    e <- parseExpr
    return $ Integral e

parseDefiniteIntegral :: Parser Token Expression
parseDefiniteIntegral = do
    exactSt "integrate" <|> exactWords "the integral of"
                        <|> exactWords "the definite integral of"
    e <- parseExpr
    (a, b) <- (do              -- Either parse:
        exactSt "between"      -- "between"
        a <- parseExpr         -- a
        exactSt "and"          -- "and"
        b <- parseExpr         -- b
        return (a, b)) <|> (do -- Or parse:
        exactSt "from"         -- "from"
        a <- parseExpr         -- a
        exactSt "to"           -- "to"
        b <- parseExpr         -- b
        return (a, b))
    return $ DefiniteIntegral e a b
    
parseDerivative :: Parser Token Expression
parseDerivative = do
    exactSt "differentiate" <|> exactWords "the derivative of"
    e <- parseExpr
    var <- opt $ do
        exactWords "with respect to"
        string
    return $ Derivative e var

-- This is a very general parser. Many of the statistics commands take the form of
-- "the _ of _ in _" for one form of the command (i.e "the standard deviation of x in table1")
-- or "the sample _ of _ in _" for the sample form of the command (i.e "the sample variance of y in table2")
-- This takes two functions, one to apply for the regular version and one for the sample version,
-- as well as the command name, and produces a parser for it.
parseStatSampleFunc :: (String -> Expression -> Expression) -> (String -> Expression -> Expression) -> String -> Parser Token Expression
parseStatSampleFunc m sm n = do
    opt $ exactSt "the"
    sample <- isJust <$> opt (exactSt "sample") 
    -- sample is True if "sample" is present and False otherwise
    exactWords n -- parse the command name
    exactSt "of"
    var <- string
    exactSt "in"
    tbl <- parseExpr
    if sample
        then return $ sm var tbl -- use the alternative function if sample is True
        else return $ m var tbl

-- This is similar, except its for commands which do not have a "sample" variant, like median and mode.
parseStatFunc :: (String -> Expression -> Expression) -> String -> Parser Token Expression
parseStatFunc m n = do
    opt $ exactSt "the"
    exactWords n
    exactSt "of"
    var <- string
    exactSt "in"
    tbl <- parseExpr
    return $ m var tbl

parseStatFuncs :: Parser Token Expression
parseStatFuncs = parseStatSampleFunc Mean SampleMean "mean" <|>
                 parseStatFunc Mode "mode" <|>
                 parseStatFunc Median "median" <|>
                 parseStatSampleFunc Variance SampleVariance "variance" <|>
                 parseStatSampleFunc StdDev SampleStdDev "standard deviation"

-- This is also similar to parseStatFunc, except that its for
-- commands of the form "the _ of _ and _ in _" i.e stats functions of two variables
-- e.g "the spearman correlation of x and y in table1"
parseStatDiFunc :: (String -> String -> Expression -> Expression) -> String -> Parser Token Expression
parseStatDiFunc f n = do
    opt $ exactSt "the"
    exactWords n
    exactSt "of"
    a <- string
    exactSt "and"
    b <- string
    exactSt "in"
    tbl <- parseExpr
    return $ f a b tbl

parseStatFuncs2 :: Parser Token Expression
parseStatFuncs2 = parseStatDiFunc ChiSquared "chi squared value" <|>
                  parseStatDiFunc PearsonCorrelation "pearson correlation" <|>
                  parseStatDiFunc SpearmanCorrelation "spearman correlation"

-- This parses the operations on matrices of the form:
-- "the _ of _", (e.g "the determinant of m")
-- by taking a function to wrap the value in, and a command name
parseMatFunc :: (Expression -> Expression) -> String -> Parser Token Expression
parseMatFunc f n = do
    opt $ exactSt "the"
    exactWords n
    exactSt "of"
    mat <- parseExpr
    return $ f mat

parseMatFuncs :: Parser Token Expression
parseMatFuncs = parseMatFunc Magnitude "magnitude" <|> 
                parseMatFunc Determinant "determinant" <|>
                parseMatFunc InverseMatrix "inverse" <|>
                parseMatFunc Eigenvalues "eigenvalues" <|>
                parseMatFunc Eigenvectors "eigenvectors" <|>
                parseMatFunc CharacteristicEquation "characteristic equation"

-- This is the matrix equivalent of parseStatDiFunc. It parses commands of the form:
-- "the _ of _ and _", (e.g "the dot product of a and b")
parseMatFunc2 :: (Expression -> Expression -> Expression) -> String -> Parser Token Expression
parseMatFunc2 f n = do
    opt $ exactSt "the"
    exactWords n
    a <- parseExpr
    exactSt "and"
    b <- parseExpr
    return $ f a b

parseMatFuncs2 :: Parser Token Expression
parseMatFuncs2 = parseMatFunc2 DotProduct "dot product of" <|>
                 parseMatFunc2 AngleBetween "angle between"

-- This parses all of the "terminal" expressions.
-- This means all of the expressions without any kind of binary operations 
-- i.e "the determinant of m" is a terminal expression, "1 + 2" is not.
parseExprNoOp :: Parser Token Expression
parseExprNoOp = parseBracket <|>
                parseValueExpr <|>
                parseEquationOf <|>
                parseIntersectionOf <|>
                parseCircle <|> 
                parseLine <|>
                parsePoint <|>
                parseAbs <|>
                parseExprFunc <|>
                parseSubstitute <|>
                parseEvaluate <|>
                parseSolve <|>
                parseSolveSimultaneous <|>
                parseMakeVariable <|>
                parseDefiniteIntegral <|>
                parseIntegral <|>
                parseDerivative <|>
                parseStatFuncs <|>
                parseStatFuncs2 <|>
                parseMatFuncs <|>
                parseMatFuncs2 <|>
                parseVariable

-- This takes a function and a character,
-- and when it encounters that character, it returns both the function and the character.
-- It is used to parse the operations that can occur between expressions.
parseBinop :: (Expression -> Expression -> Expression) -> Char -> Parser Token (Expression -> Expression -> Expression, Char)
parseBinop f s = exactSp s >> return (f, s)

parseBinops :: Parser Token (Expression -> Expression -> Expression, Char)
parseBinops = parseBinop SumE '+' <|>
              parseBinop MulE '*' <|>
              parseBinop DivE '/' <|>
              parseBinop PowE '^' <|>
              parseBinop SubE '-' <|>
              parseBinop MakeEquation '='

-- This is the main expression parser. It contains a simple algorithm for determining operator precedence.
-- I could have used an operator precedence parser, but I didnt know at the start which operators i was going to have
-- so i used this, which is very easily extendable. I could also have used
-- Dijkstra's shunting yard algorithm, but this is not very easy to implement on a functional language
-- as it needs state (a stack).
parseExpr :: Parser Token Expression
parseExpr = do
    first <- parseExprNoOp -- parse the first terminal
    rest <- many $ do      -- followed by as many as you want of:
        b <- parseBinops   -- an operation
        e <- parseExprNoOp -- and another terminal.
        return (b, e)
    -- First we take all the terminals and operations and put them into one list of
    -- Either Operation Expression so all the operations are wrapped in Left
    -- and all the terminals in Right.
    let ls = Right first : concatMap (\(a, b) -> [Left a, Right b]) rest
        --   The first is an expression
        --                 Concatenate the lists of
        --                                        Left for each operator and right for each expression
    -- SplitOn is a helper function which takes a list [a] and a predicate
    -- and splits it into ([a], a, [a]). The first list corresponds to the part
    -- of the list _before_ the first element for which the predicate is FALSE.
    -- The second element of the tuple is the first element of the list for which the predicate is FALSE.
    -- The final list is the rest of the list after that element.
    -- I.e) splitOn odd [1, 2, 3, 4, 5, 6] would be ([1], 2, [3, 4, 5, 6])
    --  or  splitOn (== 3) [3, 3, 4, 3, 3] would be ([3, 3], 4, [3, 3])
    let splitOn f ls = (prefix, head suffix, tail suffix)
            where (prefix, suffix) = span f ls
    -- Our predicate for splitOn looks for the first Left value in our list where
    -- the operator character is the one we are looking for.
    let isCorrect op (Left (_, o)) = o == op
        isCorrect _ _ = False
    -- To remove one instance of a given operator from the list, 
    -- We first splitOn the negative of our predicate, this will look
    -- for the first place where we have our operator. Then, the element
    -- just before the operator must be our lhs, and just after the operator
    -- must be the rhs. Then we take the lhs and rhs and combine them into one new
    -- expression according to the operator function. Then, we remove the lhs
    -- rhs and operation from the list and replace them with the new expression.
    let removeOne (op, f) ls = init prefix ++ [Right (f rhs lhs)] ++ tail suffix
                            -- remove lhs 
                            --                replace operation with new expression
                            --                                       remove rhs
            where (prefix, _, suffix) = splitOn (not . isCorrect op) ls
                  Right lhs = last prefix -- The lhs is the last element before the operation
                  Right rhs = head suffix -- The rhs is the first element after the operation
    -- To remove all of the occurrences of a given operator, we simply iterate
    -- removeOne until there are no more occurences of the operator left.
    let removeAll (op, f) ls = head $ dropWhile (any (isCorrect op)) vals
                            --                                       the iterations of removeOne
                            --        drop them while the operator is still present
                            -- the first value of the remaining iterations
            where vals = iterate (removeOne (op, f)) ls
    -- The core of the algorithm is then very simple:
    -- We remove all occurences of all operators. The key to the algorithm
    -- is doing this in order from highest to lower precedence.
    -- The problem with this algorithm is that it assumes all operations are right-associative,
    -- which is not the case for some operators, e.g exponentiation, but it is good enough for now.
    let precs = [('^', PowE),
                 ('/', DivE),
                 ('*', MulE),
                 ('+', SumE),
                 ('-', SubE),
                 ('=', MakeEquation)]
    let Right final = head $ foldr removeAll (reverse ls) (reverse precs)
                          -- Fold the list over removeAll with the all the operators
                          -- because we are using foldr and not foldl, we reverse the lists.
                   -- At the end there should be only one element left so get that
    -- And since it is an expression it should be wrapped in Right, so unwrap it.
    return final

parseExpression :: Parser Token Statement
parseExpression = Expression <$> parseExpr

-- Parse the variable names for an assign statement
-- this is either simple variable or a list of variables in brackets,
-- i.e "a" or "(a, b, c)"
parseNames :: Parser Token [String]
parseNames = parseSingle <|> parseMultiple
    where parseSingle = return <$> string
          parseMultiple = do
                exactSp '('
                names <- string `sepBy` exactSp ','
                exactSp ')'
                return names

-- Parse the signature of a function in an AssignArgs statement:
-- e.g "sin(x)" or "atan2(x, y)"
parseFuncNames :: Parser Token (String, [String])
parseFuncNames = do
    n <- string
    exactSp '('
    as <- string `sepBy` exactSp ','
    exactSp ')'
    return (n, as) -- Return the function name, and the arguments

parseAssign :: Parser Token Statement
parseAssign = do
    opt $ exactSt "let"
    names <- parseNames
    exactSp '='
    e <- parseExpr
    return $ Assign names e

parseFuncAssign :: Parser Token Statement
parseFuncAssign = do
    opt $ exactSt "let"
    (n, as) <- parseFuncNames
    exactSp '='
    e <- parseExpr
    return $ AssignArgs n as e

parseStatement :: Parser Token Statement
parseStatement = do
    st <- parseAssign <|> parseFuncAssign <|> parseExpression
    done -- And then we expect the input to be empty, i.e each input to parse
         -- must contain precisely one statement.
    return st

-- A simple wrapper around parseStatement and lex which takes a string and parses it fully
parse :: String -> Either (ParseError Token) Statement
parse s = case lex s of
    Left err -> Left $ UnderlyingError err
    Right toks -> snd <$> C.parse parseStatement toks