{-# LANGUAGE FlexibleInstances, OverlappingInstances, LambdaCase #-}

module Main where

-- This is the main server side interface to AME.
-- It is a simple ZMQ based socket server that takes an input string
-- and returns a result in JSON.

import Data.IORef
import Control.Monad
import Data.List
import AME.Compute.Geometry
import AME.Compute.Expr
import AME.Compute.Error
import AME.Compute.Matrix
import AME.Compute.Simplify
import AME.Compute.Statistics
import AME.Interpret
import AME.Interpret.AST
import AME.Parser
import AME.Parser.Combinators (ParseError(..), Vals(..))
import AME.Parser.Lexer (Token)
import System.ZMQ4.Monadic
import System.Environment
import Data.ByteString.Char8 (pack, unpack)

-- a "safe" number.. this is what we want to end up with
-- i.e no Compute means no possible errors.
type SNumber = MatrixExt Double

roundSNum :: SNumber -> SNumber
roundSNum (ScalarV s) = ScalarV $ roundToDp 5 s
roundSNum (MatrixV m) = MatrixV $ matrix $ map (map $ roundToDp 5) $ values m

roundToDp :: Int -> Double -> Double
roundToDp n c = fromIntegral (truncate $ c * (10^n)) / 10^n

-- To show these we output a string of Latex.
-- This is then displayed by the client side nicely using MathJax
instance Show (MatrixExt Double) where
    -- Matrices in latex are represented like so:
    -- [[1, 2, 3], [4, 5, 6]] would be
    -- \begin{bmatrix}
    --  1 & 2 & 3 \\
    --  4 & 5 & 6
    -- \end{bmatrix}
    show (MatrixV m) = "\\\\begin{bmatrix}" ++ intercalate "\\\\\\\\" (map (intercalate "&" . map show) $ values m) ++ "\\\\end{bmatrix}"
                        -- initial prefix     join the rows with "\\"  take each column and join it with "&"s           final suffix
    show (ScalarV s) = show s
                       -- Scalars are just numbers

-- A "safe" value type.. i.e the same but nothing wrapped in Compute.
data SVal = SNumber SNumber
          | SShape (Shape SNumber)
          | SExpr (Expr String SNumber)
          | SList [SVal]
          | SEqn (Equation String SNumber)
          | STbl (Table String SNumber)
          | SPoint SNumber SNumber
          deriving (Show, Eq)

-- Combine all the errors into one big error type.
data CombinedError = InterpretError InterpretError
                   | ParseError (ParseError Token)
                   deriving (Show, Eq)

-- Take the Computes from the inside of Value and
-- move it to the outside, leaving the rest of the Value
-- as an SVal.
-- This is easy because Compute is a Monad
toSVal :: Value -> Compute SVal
toSVal (Number n) = (SNumber . roundSNum) <$> n -- Evaluate n, get the inside and apply SNumber
toSVal (Shape c) = SShape <$> case c of
    Circle (x, y) r -> do 
        x' <- roundSNum <$> x -- Evaluate all the parameters of the circle
        y' <- roundSNum <$> y
        r' <- roundSNum <$> r
        return $ Circle (x', y') r' -- And wrap it back up in a Circle
    Line (x, y) m -> do
        x' <- roundSNum <$> x
        y' <- roundSNum <$> y
        m' <- roundSNum <$> m
        return $ Line (x', y') m' -- Do the same for Line
toSVal (Expr e) = SExpr <$> toSExpr e -- Expressions use toSExpr as defined later
toSVal (Eqn (Equation lhs rhs)) = do
    lhs' <- toSExpr lhs
    rhs' <- toSExpr rhs -- Equations are the same but operator on both sides
    return $ SEqn (Equation lhs' rhs')
toSVal (List vs) = SList <$> mapM toSVal vs -- And lists map toSVal over all their values first.
toSVal (Tbl (Table vars vals)) = do
    vals' <- mapM sequence vals -- Evaluate all the values in the table.
                                -- This works as sequence (sequence :: Monad m => [m a] -> m [a])
                                -- takes a row and turns it into a normal array wrapped in Compute
                                -- And then mapM applies this to every row and combines the results into
                                -- one big Compute.
    return $ STbl (Table vars vals')
toSVal (Point a b) = do
    a' <- a
    b' <- b
    return $ SPoint a' b'

-- This just takes all the Numbers in an Expr and turns them into SNumbers
toSExpr :: Expr String Number -> Compute (Expr String SNumber)
toSExpr (Const c) = (Const . roundSNum) <$> c -- Evaluate c, then wrap it back in Const
toSExpr (Var v) = return (Var v) -- Variables stay the same
toSExpr (Mul xs) = Mul <$> mapM toSExpr xs -- Mul and Sum evaluate all of their arguments with mapM
toSExpr (Sum xs) = Sum <$> mapM toSExpr xs
toSExpr (Div a b) = liftM2 Div (toSExpr a) (toSExpr b) -- liftM2 is like <$> but for functions of two arguments
toSExpr (Pow a b) = liftM2 Pow (toSExpr a) (toSExpr b)
toSExpr (Sin a) = Sin <$> toSExpr a -- All the rest just recursively apply toSExpr to their contents in order
toSExpr (Cos a) = Cos <$> toSExpr a -- to propagate it down the tree.
toSExpr (Tan a) = Tan <$> toSExpr a
toSExpr (ASin a) = ASin <$> toSExpr a
toSExpr (ACos a) = ACos <$> toSExpr a
toSExpr (ATan a) = ATan <$> toSExpr a
toSExpr (Exp a) = Exp <$> toSExpr a
toSExpr (Log a) = Log <$> toSExpr a
toSExpr (Abs a) = Abs <$> toSExpr a

showTy :: Type -> String
showTy NumberT = "number"
showTy ShapeT = "shape"
showTy ExprT = "expression"
showTy EqnT = "equation"
showTy ListT = "list"
showTy TblT = "data table"
showTy PointT = "point"

-- Describe any of the possible errors in english to display to the user.
describeError :: CombinedError -> String
describeError (InterpretError e) = case e of
    TypeError tys ty ex -> "Type Error: Expected " ++ intercalate " or " (map showTy tys) ++ " but got " ++ showTy ty ++ " while " ++ ex
                                                    -- For each possible type, show it and seperate them by " or "
    NumericalError t -> describeNumericalError t
                      -- Numerical errors handled seperately below.
    UnpackError e -> "Error while unpacking: " ++ e
    NoVariable v -> "The variable " ++ v ++ " does not exist."
    SpecifyVariable -> "Please specify which variable the operation is with respect to."
    CantIntegrate -> "This function cannot be integrated."
    OnlyForMatrices -> "Can't find a characteristic equation of a scalar."
describeError (ParseError e) = case e of
    Expected a b -> "Expected " ++ showVals a ++ " got " ++ showVals b
    WrongVal a -> "Got the wrong value: " ++ show a
    UnknownError -> "Unknown parser error!"
    UnderlyingError e -> case e of
    -- Handle UnderlyingErrors exactly the same because theyre just ParseErrors that occured in the lexer.
        Expected a b -> "Expected " ++ showVals a ++ " got " ++ showVals b
        WrongVal a -> "Got the wrong value: " ++ show a
        UnknownError -> "Unknown parser error!"

showVals :: Show s => Vals s -> String
showVals (S s) = "\"" ++ show s ++ "\""
showVals (Describe s) = s
showVals EOF = "end-of-file"
showVals ANY = "any"

describeNumericalError :: NumericalError -> String
describeNumericalError DivisionByZero = "Tried to divide by zero."
describeNumericalError UnexpectedFreeVariable = "The expression still has free variables."
describeNumericalError (MatrixHasWrongDimensions a b c d) = "The matrix has wrong dimensions: expected " ++ show (a, b) ++ " got " ++ show (c, d)
describeNumericalError (MatrixShouldBeSquare a b) = "The matrix should be square, but instead has dimensions " ++ show (a, b)
describeNumericalError (MatrixOperationNotSupported reason) = "The operation is not supported: " ++ reason
describeNumericalError MatrixNotInvertible = "The matrix is not invertible!"
describeNumericalError IntersectsEverywhere = "The two shapes intersect everywhere!"
describeNumericalError ShouldBeLinear = "Cannot solve non-linear simultaneous equations."
describeNumericalError (WrongNumberOfEquations a b) = "Wrong number of equations: expected " ++ show a ++ " got " ++ show b
describeNumericalError NoPartialIntegrals = "Cannot find partial integrals!"

-- To show a given SVal, we need to produce a string of Latex for the front-end to render.
showValue :: SVal -> String
showValue (SNumber s) = show s -- For numbers we use the Show SNumber instance defined above
showValue (SShape s) = case s of
    -- Circles are given in the form of center and radius
    Circle (x, y) r -> "\\\\text{the circle with center }" ++ show (x, y) ++ "\\\\text{ and radius }" ++ show r
    -- And lines are given as a point and a gradient
    Line (x, y) m -> "\\\\text{the line passing through }" ++ show (x, y) ++ "\\\\text{ with gradient }" ++ show m
    -- The LaTex "\text{...}" includes some text in the output verbatim instead of treating the words as variables.
showValue (SExpr expr) = showSExpr expr
-- Expressions handled seperately below
showValue (SEqn (Equation lhs rhs)) = showSExpr lhs ++ " = " ++ showSExpr rhs
showValue (SPoint a b) = show (a, b)

showSExpr :: Expr String SNumber -> String
                    -- Wrap in "{}"s to avoid precedence issues.
                    -- Map showSExpr over all of the constituents
                    -- And then seperate them with either "+" or "*" for Sum or Mul
showSExpr (Sum xs) = "{" ++ intercalate "+" (map showSExpr xs) ++ "}"
showSExpr (Mul xs) = "{" ++ intercalate "\\\\cdot " (map showSExpr xs) ++ "}"
showSExpr (Pow a b) = "{" ++ showSExpr a ++ "}^{" ++ showSExpr b ++ "}"
                    -- Latex for a/b is "\frac{a}{b}"
showSExpr (Div a b) = "\\\\frac{" ++ showSExpr a ++ "}{" ++ showSExpr b ++ "}"
-- Variables are just a string
showSExpr (Var v) = v
-- Constants use the Show SNumber definition provided earlier.
showSExpr (Const c) = show c
-- \sin, \cos etc work as expected. \left( is a starting ( and \right) an end bracket.
showSExpr (Sin x) = "\\\\sin{\\\\left(" ++ showSExpr x ++ "\\\\right)}"
showSExpr (Cos x) = "\\\\cos{\\\\left(" ++ showSExpr x ++ "\\\\right)}"
showSExpr (Tan x) = "\\\\tan{\\\\left(" ++ showSExpr x ++ "\\\\right)}"
showSExpr (ASin x) = "\\\\asin{\\\\left(" ++ showSExpr x ++ "\\\\right)}"
showSExpr (ACos x) = "\\\\acos{\\\\left(" ++ showSExpr x ++ "\\\\right)}"
showSExpr (ATan x) = "\\\\atan{\\\\left(" ++ showSExpr x ++ "\\\\right)}"
showSExpr (Exp x) = "\\\\exp{\\\\left(" ++ showSExpr x ++ "\\\\right)}"
showSExpr (Log x) = "\\\\log{\\\\left(" ++ showSExpr x ++ "\\\\right)}"
showSExpr (Abs x) = "{\\\\left|" ++ showSExpr x ++ "\\\\right|}"

-- Take an Interpret value as well as the current variable state and lift out all the errors and state changes.
liftInterpret :: Env -> Interpret a -> Either CombinedError (a, Env)
liftInterpret e a = case runInterpret e a of -- Run the interpret.
        (Right val, e') -> Right (val, e') -- If it is succesfull, return it
        (Left err, _) -> Left (InterpretError err) -- Otherwise wrap the given error in CombinedError.

-- Similarly, Lift a Parse value by simply wrapping the errors in CombinedError
liftParse :: Either (ParseError Token) a -> Either CombinedError a
liftParse a = case a of
    Left err -> Left (ParseError err)
    Right val -> Right val

-- In order to show valid string in the JSON output
-- We need to escape the quote and escape characters
escape :: String -> String
escape = concatMap $ \case
    '"' -> "\\\""
    x -> return x

-- One evaluation step. Takes a reference to the current variable state
-- as well as an input string, and returns a string of JSON to be sent to the client.
replStep :: IORef Env -> String -> ZMQ z String
replStep ref line = do
    -- Read the value of the current state
    env <- liftIO $ readIORef ref
    let res = do
            -- Try parse the input
            ast <- liftParse $ parse line
            -- Try executing the parsed expression, maybe returning an unsafe Value.
            (unsafeVal, e) <- liftInterpret env $ exec ast
            val <- case unsafeVal of
                -- If a value is returned, convert it to a safe SVal.
                Just unsafeVal' -> (Just . fst) <$> liftInterpret env (liftCompute $ toSVal unsafeVal')
                -- Otherwise leave it with nothing.
                Nothing -> return Nothing
            return (val, e)
    case res of
        -- If all of that succeeded
        Right (val, e) -> do
            -- Write the updated state back to the reference
            liftIO $ writeIORef ref e
            -- The serialize to JSON
            case val of 
                -- Nothing returns an empty object.
                Nothing -> return "{\"type\": \"empty\"}"
                Just val' -> case val' of
                    -- Lists are Tables are handeled differently from other values so the can be displayed differently on 
                    -- the front-end.
                    -- Lists have a values field which is a list of strings of all of the list values displayed.
                    -- Tables have two fields, one for the variables which is simple a list of strings
                    -- And one for all the values, similar to the representation for Lists.
                    SList ls -> return $ "{ \"type\": \"list\", \"values\": [" ++ intercalate "," (map (show . showValue) ls) ++ "]}"
                    STbl (Table vars vals) -> return $ "{ \"type\": \"table\", \"vars\": [" ++ intercalate "," (map show vars) ++ "], \"vals\": " ++ show vals ++ "}"
                    other -> return $ "{ \"type\": \"value\", \"value\": \"" ++ escape (showValue other) ++ "\"}"
        -- If its an error, indicate this and then also return its description.
        Left err -> return $ "{\"type\": \"error\", \"error\": \"" ++ escape (describeError err) ++ "\"}"
    
-- Main entry point for the entire server side.
main :: IO ()
--     Run a ZMQ server.
main = runZMQ $ do
    -- Create a new empty variable state.
    ref <- liftIO $ newIORef (Env [])
    -- Create a fresh socket
    sock <- socket Rep
    -- Bind it to the place specified by the program arguments
    -- Because ZMQ operates exactly the same regardless of the actual
    -- Socket type, this could use ipc, tcp etc. So the server
    -- Could be on the same machine or remote.
    [loc] <- liftIO getArgs
    bind sock loc 
    forever $ do
        -- recieve one line of input from the socket
        line <- receive sock
        -- Apply replStep to this
        ret <- replStep ref (unpack line)
        -- Print the return JSON
        liftIO $ print ret
        -- And then send it back through the socket.
        send sock [] (pack ret)
