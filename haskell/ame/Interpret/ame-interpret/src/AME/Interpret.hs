{-# LANGUAGE LambdaCase #-}

module AME.Interpret (
  Env(..),
  InterpretError(..),
  Interpret,
  Type(..),
  runInterpret,
  liftCompute,
  eval,
  exec  
) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad
import Data.Maybe
import Data.List
import AME.Compute.Error
import AME.Compute.Expr
import AME.Compute.Simplify
import AME.Compute.Geometry
import AME.Compute.Solver
import AME.Compute.Calculus.Integrals
import AME.Compute.Calculus.Derivatives
import AME.Compute.Statistics
import AME.Compute.Matrix
import AME.Interpret.AST

--An error that can occur in the interpreter.
--Includes all the matrix and numerical errors from ame-compute
--As well as some semantic errors.
data InterpretError = NumericalError NumericalError 
                    | TypeError [Type] Type String
                    | UnpackError String
                    | NoVariable String
                    | SpecifyVariable
                    | CantIntegrate
                    | OnlyForMatrices
                    deriving (Eq, Show)

--A scope, that contains a list of names and values for variable.
newtype Env = Env [(String, Value)] 
            deriving (Show, Eq)

--The main interpreter monad, has ExceptT InterpretError (can raise exceptions of type InterpretError)
--and State Env (keeps a mutable copy of an Env)
type Interpret a = ExceptT InterpretError (State Env) a

--The type of all the types of data.
--Mainly used for generating nice error messages.
data Type = NumberT 
          | ShapeT 
          | ExprT 
          | EqnT
          | ListT
          | TblT
          | PointT
          deriving (Show, Eq)

--Take an initial value for the mutable Env, an Interpret value
--and return the final value of the Env, and either an answer or an error
runInterpret :: Env -> Interpret a -> (Either InterpretError a, Env)
runInterpret env f = runState (runExceptT f) env

--Take a Compute expression and run it in the interpreter
--this is basically a Except NumericalError, so all we need to do
--is wrap it in an identity State Env (return) and then map the
--NumericalError to the appropriate InterpretError
liftCompute :: Compute a -> Interpret a
liftCompute a = case runCompute a of
    Left err -> throwError $ NumericalError err
    Right a -> return a

--Get a variable value from the interpreter or throw an error if it doesnt exist.
getVar :: String -> Interpret Value
getVar s = do
    Env vars <- lift get
    case lookup s vars of
        Just val -> return val
        Nothing -> throwError $ NoVariable s 

typeOf :: Value -> Type
typeOf (Number _) = NumberT
typeOf (Shape _) = ShapeT
typeOf (Expr _) = ExprT
typeOf (Eqn _) = EqnT
typeOf (List _) = ListT
typeOf (Tbl _) = TblT
typeOf (Point _ _) = PointT

--Ensure that val has one of the given types, and throw a TypeError if it does not.
ensure :: [Type] -> Value -> String -> Interpret ()
ensure ty val name = if realty `elem` ty
    then return ()
    else throwError $ TypeError ty realty name
    where realty = typeOf val

--Convert a mathematical expression into an Expr (the underlying representation for mathematical expressions)
--with two functions, one for if the value in the expression is not constant
--and one to provide a shortcut for when it is constant.
--I.e liftExpr a Sin sin "sin" will use Sin to form a symbolic expression involving Sin when the argument
--is not constant, but will just evaluate sin a when a is constant.
liftExpr :: Expression -> (Expr String Number -> Expr String Number) -> (Number -> Number) -> String -> Interpret Value
liftExpr a func numfunc name = do
    a' <- eval a
    ensure [NumberT, ExprT] a' $ "finding " ++ name
    --ensure the argument is either a number or an expression
    case a' of
        Number a'' -> return $ Number $ numfunc a''
        --if its constant, use the shortcut.
        Expr a'' -> do
            let fa = func a''
            --otherwise make the symbolic expression
            fa' <- liftCompute $ simplify fa
            --simplify it
            return $ Expr fa'
            --and return that instead.

--Lift an expression into a function that takes a variable, and a table.
liftTable :: String -> Expression -> (String -> Table String Number -> Number) -> String -> Interpret Value
liftTable s t f n = do
    t' <- eval t
    ensure [TblT] t' $ "finding a " ++ n
    --We only want the argument to be a table
    let Tbl t'' = t'
    return $ Number $ f s t''
    --Then apply our function

--The same as above but with two variables and a table.
liftTable2 :: String -> String -> Expression -> (String -> String -> Table String Number -> Number) -> String -> Interpret Value
liftTable2 a b t f n = do
    t' <- eval t
    ensure [TblT] t' $ "finding a " ++ n
    let Tbl t'' = t'
    return $ Number $ f a b t''

--Almost the same thing, except this is for functions of matrices
--The difference here is that we only need to ensure that the argument
--is a number... if the operation can't be applied to matrices, the underlying function
--_should_ catch that itself.
liftMatrix :: Expression -> (Number -> Number) -> String -> Interpret Value
liftMatrix a f s = do
    a' <- eval a
    ensure [NumberT] a' $ "finding a " ++ s
    let Number a'' = a'
    return $ Number $ f a''

-- This almost certainly not the best way to design this system.
-- Initially I had thought about some kind of system which combined the 
-- parser, interpreter and typechecker for each case, so we could write one combined
-- expression for each case, but this would be more difficult to implement,
-- so instead we just look throught each case of the huge Expression type
-- and evaluate them seperately.
eval :: Expression -> Interpret Value
eval (Variable s) = getVar s
eval (MakeVariable s) = return $ Expr $ Var s
--Making a new variable just returns a var expression
eval (Value v) = return v
--Values evaluate to themselves.
eval (SumE a b) = do
    a' <- eval a
    b' <- eval b
    ensure [NumberT, ExprT] a' "adding"
    ensure [NumberT, ExprT] b' "adding"
    --Adding two values, we want them to be both either expressions or numbers
    case (a', b') of
        (Number a'', Number b'') -> return $ Number $ a'' + b''
        --If they are both numbers, evaluate the sum now
        (Number a'', Expr b'') -> Expr <$> liftCompute (simplify $ Sum [Const a'', b''])
        (Expr a'', Number b'') -> Expr <$> liftCompute (simplify $ Sum [Const b'', a''])
        (Expr a'', Expr b'') -> Expr <$> liftCompute (simplify $ Sum [a'', b''])
        --Otherwise make a symbolic expression, lifting numbers into expressions where needed.
--Do the exact same thing for multiplying, dividing and exponentiating.
--I couldn't abstract this into a seperate function because each case is different in subtle ways.
--i.e) they all have different functions, but also division and exponentiation are non-commutative, etc.
eval (MulE a b) = do
    a' <- eval a
    b' <- eval b
    ensure [NumberT, ExprT] a' "multiplying"
    ensure [NumberT, ExprT] b' "multiplying"
    case (a', b') of
        (Number a'', Number b'') -> return $ Number $ a'' * b''
        (Number a'', Expr b'') -> Expr <$> liftCompute (simplify $ Mul [Const a'', b''])
        (Expr a'', Number b'') -> Expr <$> liftCompute (simplify $ Mul [Const b'', a''])
        (Expr a'', Expr b'') -> Expr <$> liftCompute (simplify $ Mul [a'', b''])
eval (DivE a b) = do
    a' <- eval a
    b' <- eval b
    ensure [NumberT, ExprT] a' "dividing"
    ensure [NumberT, ExprT] b' "dividing"
    case (a', b') of
        (Number a'', Number b'') -> return $ Number $ a'' / b''
        (Number a'', Expr b'') -> Expr <$> liftCompute (simplify $ Div (Const a'') b'')
        (Expr a'', Number b'') -> Expr <$> liftCompute (simplify $ Div a'' (Const b''))
        (Expr a'', Expr b'') -> Expr <$> liftCompute (simplify $ Div a'' b'')
eval (PowE a b) = do
    a' <- eval a
    b' <- eval b
    ensure [NumberT, ExprT] a' "exponentiating"
    ensure [NumberT, ExprT] b' "exponentiating"
    case (a', b') of
        (Number a'', Number b'') -> return $ Number $ a'' ** b''
        (Number a'', Expr b'') -> Expr <$> liftCompute (simplify $ Pow (Const a'') b'')
        (Expr a'', Number b'') -> Expr <$> liftCompute (simplify $ Pow a'' (Const b''))
        (Expr a'', Expr b'') -> Expr <$> liftCompute (simplify $ Pow a'' b'')
eval (SubE a b) = do
    a' <- eval a
    b' <- eval b
    ensure [NumberT, ExprT] a' "subtracting"
    ensure [NumberT, ExprT] b' "subtracting"
    case (a', b') of
        (Number a'', Number b'') -> return $ Number $ a'' - b''
        (Number a'', Expr b'') -> Expr <$> liftCompute (simplify $ Sum [Const a'', Mul [-1, b'']])
        (Expr a'', Number b'') -> Expr <$> liftCompute (simplify $ Sum [a'', Const (negate b'')])
        (Expr a'', Expr b'') -> Expr <$> liftCompute (simplify $ Sum [a'', Mul [-1, b'']])
--All the trig and algebraic functions use the liftExpr we defined earlier.
eval (SinE a) = liftExpr a Sin sin "sine"
eval (CosE a) = liftExpr a Cos cos "cosine"
eval (TanE a) = liftExpr a Tan tan "tangent"
eval (ASinE a) = liftExpr a ASin asin "inverse sine"
eval (ACosE a) = liftExpr a ACos acos "inverse cosine"
eval (ATanE a) = liftExpr a ATan atan "inverse tangent"
eval (AbsE a) = liftExpr a Abs abs "absolute value"
eval (ExpE a) = liftExpr a Exp exp "exponential"
eval (LogE a) = liftExpr a Log log "logarithm"
--Square root is similar but we dont have a dedicated function so we translate
--it as a power of 0.5.
eval (SqrtE a) = do
    a' <- eval a
    ensure [NumberT, ExprT] a' "finding square root"
    case a' of
        Number a'' -> return $ Number $ sqrt a''
        Expr a'' -> return $ Expr $ Pow a'' (Const 0.5)
--Make an equation by wrapping two expressions up as the lhs and rhs
--As usual with expressions, lift Numbers to Expressions
--and type check to make sure the arguments are either numbers or expressions.
eval (MakeEquation a b) = do
    a' <- eval a
    b' <- eval b
    ensure [ExprT, NumberT] a' "forming equation"
    ensure [ExprT, NumberT] b' "forming equation"
    let a'' = case a' of
            Number n -> Const n
            Expr e -> e
    let b'' = case b' of
            Number n -> Const n
            Expr e -> e
    sa <- liftCompute $ simplify a''
    sb <- liftCompute $ simplify b''
    return $ Eqn $ Equation sa sb
--Make a point from two numbers.
eval (MakePoint a b) = do
    a' <- eval a
    b' <- eval b
    ensure [NumberT] a' "making a point"
    ensure [NumberT] b' "making a point"
    let Number a'' = a'
        Number b'' = b'
    return $ Point a'' b''
--All the following are simple wrappers around constructing shapes
--checking for points and numbers appropriately
eval (LinePointGradient a b) = do
    a' <- eval a
    b' <- eval b
    ensure [PointT] a' "making a line"
    ensure [NumberT] b' "making a line"
    let Point cx cy = a'
        Number m = b'
    return $ Shape $ linePointGradient (cx, cy) m
eval (LineInterceptGradient a b) = do
    a' <- eval a
    b' <- eval b
    ensure [NumberT] a' "making a line"
    ensure [NumberT] b' "making a line"
    let Number a'' = a'
        Number b'' = b'
    return $ Shape $ lineInterceptGradient a'' b''
eval (LinePointPoint a b) = do
    a' <- eval a
    b' <- eval b
    ensure [PointT] a' "making a line"
    ensure [PointT] b' "making a line"
    let Point ax ay = a'
        Point bx by = b'
    return $ Shape $ linePointPoint (ax, ay) (bx, by)
eval (CircleCenterRadius a b) = do
    a' <- eval a
    b' <- eval b
    ensure [PointT] a' "making a circle"
    ensure [NumberT] b' "making a circle"
    let Point cx cy = a'
        Number r = b'
    return $ Shape $ circleCenterRadius (cx, cy) r
eval (CircleCenterPoint a b) = do
    a' <- eval a
    b' <- eval b
    ensure [PointT] a' "making a circle"
    ensure [PointT] b' "making a circle"
    let Point cx cy = a'
        Point rx ry = b'
    return $ Shape $ circleCenterPoint (cx, cy) (rx, ry)
--Find the equation of a shape
eval (EquationOf a) = do
    a' <- eval a
    ensure [ShapeT] a' "finding the equation of a shape"
    let Shape s = a'
    --Map over all the constants and expressions ("terminals")
    --And change the variables to the equivalent strings.
    --keeping constants the same.
    let revarExpr = emapt $ \case
            Var X -> Var "x"
            Var Y -> Var "y"
            Const c -> Const (c :: Number)
    let Equation lhs rhs = equationOf s
    lhs' <- liftCompute $ simplify $ revarExpr lhs
    rhs' <- liftCompute $ simplify $ revarExpr rhs
    return $ Eqn $ Equation lhs' rhs'
-- Find the intersection of two shapes.
eval (IntersectionOf a b) = do
    a' <- eval a
    b' <- eval b
    ensure [ShapeT] a' "finding the intersection of two shapes"
    ensure [ShapeT] b' "finding the interseciton of two shapes"
    let Shape sa = a'
        Shape sb = b'
    is <- liftCompute $ intersectionOf sa sb
    return $ List $ map (uncurry Point) is
-- Substitute a variable in one expression for another expression
eval (Substitute v e a) = do
    e' <- eval e
    a' <- eval a
    ensure [ExprT, NumberT] e' "substituting an expression"
    ensure [ExprT, EqnT] a' "substituting an expression"
    let dosub a'' = liftCompute $ case e' of
            Number e'' -> substitute v (Const e'') a'' -- To substitute a number wrap it in constant first
            Expr e'' -> substitute v e'' a''
    case a' of
        Eqn (Equation lhs rhs) -> Eqn <$> (Equation <$> dosub lhs <*> dosub rhs)
        -- To substitute into an equation, substitute on both sides.
        Expr a'' -> Expr <$> dosub a''
eval (Evaluate e) = do
    e' <- eval e
    ensure [ExprT] e' "evaluating an expression"
    let Expr e'' = e'
    res <- liftCompute $ evaluate e''
    return $ Number res
eval (Solve e v) = do
    e' <- eval e
    ensure [EqnT] e' "solving an equation"
    let Eqn e''@(Equation lhs rhs) = e'
    let fv = case freeVariables lhs of -- If we dont provide a variable to solve for look through these:
            [] -> case freeVariables rhs of
                [] -> return "x" -- If we have no variables, just use x
                [(var, _)] -> return var -- If we have one variable, great solve for that
                _ -> throwE SpecifyVariable -- If more than one, throw an error
            [(var, _)] -> return var -- Again, if we have only one use that
            _ -> throwE SpecifyVariable -- Otherwise throw an error.
    var <- maybe fv return v
    vals <- liftCompute $ solve var e''
    case vals of 
        [x] -> return $ Number x -- If we only have one solution, use that
        _ -> return $ List $ map Number vals -- Otherwise wrap the list of solutions in a List
eval (SolveSimultaneous es) = do
    es' <- mapM eval es
    mapM_ (flip (ensure [EqnT]) "solving simultaneous equations") es' 
    -- mapM ensure over all the expressions to make sure they're all equations.
    let getInner (Eqn e) = e
    vals <- liftCompute $ solveSimultaneous $ map getInner es'
    return $ List $ map (Number . snd) $ sortOn fst vals
    -- Take the solutions, and sort them in alphabetical order,
    -- then discard the variable names and return the solutions as a list.
eval (Integral e) = do
    e' <- eval e
    ensure [ExprT] e' "integrating"
    let Expr e'' = e'
    let fv = case freeVariables e'' of 
            ((var, _):_) -> var  -- If we have only a free variable, integrate with respect to that
            [] -> "x" -- Otherwise integrate with respect to x
    res <- liftCompute $ integrate fv e'' 
    case res of
        Nothing -> throwE CantIntegrate -- If integrate returns Nothing, throw an error.
        Just res' -> return $ Expr res'
eval (DefiniteIntegral e a b) = do
    e' <- eval e
    a' <- eval a
    b' <- eval b
    ensure [ExprT] e' "computing a definite integral"
    ensure [NumberT] a' "computing a definite integral"
    ensure [NumberT] b' "computing a definite integral"
    let Number a'' = a'
        Number b'' = b'
        Expr e'' = e'
    let fv = case freeVariables e'' of -- The same variable finding procedure as for Integral
            ((var, _):_) -> var
            [] -> "x"
    res <- liftCompute $ definiteIntegral fv e'' a'' b''
    case res of
        Nothing -> throwE CantIntegrate
        Just res' -> return $ Number res'
eval (Derivative e s) = do
    e' <- eval e
    ensure [ExprT] e' "finding a derivative"
    let Expr e'' = e'
    fv <- case s of
            Just v -> return v -- If we have are given a variable to differentiate by, good
            Nothing -> case freeVariables e'' of
                [(var, _)] -> return var -- Otherwise if we have only one use that
                [] -> return "x" -- If we have none, use x
                _ -> throwE SpecifyVariable -- And if we have more than one throw an error.
    res <- liftCompute $ differentiate fv e''
    sres <- liftCompute $ simplify res
    return $ Expr sres -- differentiate, simplify and return the result.
-- For all of the stats functions, use liftTable we defined earlier.
eval (Mean s e) = liftTable s e mean "mean"
eval (SampleMean s e) = liftTable s e sampleMean "sample mean"
eval (Variance s e) = liftTable s e variance "variance"
eval (SampleVariance s e) = liftTable s e sampleVariance "sample variance"
eval (StdDev s e) = liftTable s e standardDeviation "standard deviation"
eval (SampleStdDev s e) = liftTable s e sampleStandardDeviation "sample standard deviation"
eval (Median s e) = liftTable s e median "median"
eval (Mode s e) = liftTable s e mode "mode"
eval (ChiSquared a b e) = liftTable2 a b e chiSquared "chi squared value"
eval (PearsonCorrelation a b e) = liftTable2 a b e pearsonCorrelation "Pearson correlation"
eval (SpearmanCorrelation a b e) = liftTable2 a b e spearmanCorrelation "Spearman correlation"
eval (DotProduct a b) = do
    a' <- eval a
    b' <- eval b
    ensure [NumberT] a' "computing a dot product"
    ensure [NumberT] b' "computing a dot product"
    let Number a'' = a'
        Number b'' = b'
    return $ Number $ a'' `dotE` b''
eval (AngleBetween a b) = do
    a' <- eval a
    b' <- eval b
    ensure [NumberT] a' "computing an angle between vectors"
    ensure [NumberT] b' "computing an angle between vectors"
    let Number a'' = a'
        Number b'' = b'
    return $ Number $ a'' `angleE` b''
-- The matrix functions use liftMatrix as defined earlier
eval (Magnitude e) = liftMatrix e magnitudeE "magnitude"
eval (Determinant e) = liftMatrix e determinantE "determinant"
eval (InverseMatrix e) = liftMatrix e recip "inverse matrix"
eval (CharacteristicEquation e) = do
    e' <- eval e
    ensure [NumberT] e' "finding a characteristic equation"
    let Number e'' = e'
    n <- liftCompute e''
    case n of -- Since numbers can be both matrices and scalars
        ScalarV _ -> throwE OnlyForMatrices -- Throw an error if we have a scalar
        MatrixV m -> do -- Otherwise
            expr <- liftCompute $ characteristic m -- Compute the characteristic
            let revarExpr = emapt $ \case -- And remap all the terminals:
                    Var Lambda -> Var "x" -- Variables (characteristic uses Lambda) go to string variables
                    Const c -> Const (return $ ScalarV c) -- And the constants go from Double to Number
            expr' <- liftCompute $ simplify expr
            return $ Expr $ revarExpr expr'
eval (Eigenvalues e) = do
    e' <- eval e
    ensure [NumberT] e' "finding eigenvalues"
    let Number e'' = e'
    vals <- liftCompute $ eigenvaluesE e''
    -- Although we can only find the eigenvalues of a matrix,
    -- eigenvaluesE will handle throwing the error for us if we get a scalar.
    return $ List $ map (Number . return) vals
eval (Eigenvectors e) = do
    e' <- eval e
    ensure [NumberT] e' "finding eigenvectors"
    let Number e'' = e'
    vals <- liftCompute $ eigenvectorsE e''
    -- This is the same as above with the eigenvalue errors.
    return $ List $ map (Number . return) vals

-- To Assign a value to a variable
assign :: String -> Value -> Interpret ()
assign s v = do
    Env vars <- lift get -- Get the current variables
    lift $ put $ Env $ (s, v) : vars -- And put this new one on the front
    -- Because of the way that reading variables is implemented (with lookup)
    -- It will always get the first matching definition of a variable
    -- so putting one on the front effectively "updates" the variable if it is already defined.

exec :: Statement -> Interpret (Maybe Value)
exec (Expression expr) = Just <$> eval expr
exec (Assign ss expr) = do
    val <- eval expr
    case ss of -- Look at the variables we are trying to unpack to
        -- If there are none, we have made mistake.
        [] -> error "Someone tried to unpack into no values."
        -- If there's one, great put the whole value into that.
        [s] -> assign s val
        _ -> do -- If there's more than one:
            -- First we have to make sure that its a list we're unpacking
            ensure [ListT] val "trying to unpack a value"
            let List vs = val
            case length ss `compare` length vs of -- Then look at how many values there are
                GT -> throwE $ UnpackError "Too few values!" -- Throw an error if thats not right
                LT -> throwE $ UnpackError "Too many values!"
                EQ -> zipWithM_ assign ss vs -- And if it is, assign each variable to each corresponding value.
    return Nothing
exec (AssignArgs s as e) = do
    mapM_ (\a -> assign a $ Expr (Var a)) as -- For a function, take every argument and assign it a fresh variable
    val <- eval e 
    assign s val -- Then just assign the function body to the function name.
    return Nothing
                
