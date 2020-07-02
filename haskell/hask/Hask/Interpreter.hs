module Hask.Interpreter where
    
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Hask.AST
import Data.List (groupBy)

runEval :: Eval a -> IO (Either HaskError a, (FuncEnv, ProgEnv))
runEval = flip runStateT ([], []) . runExceptT

runProg :: Prog a -> (a, ProgEnv)
runProg = runIdentity . flip runStateT [] 

runProgram :: Program -> Prog ()
runProgram (Program decls) = do
    ds' <- ds
    put $ zip (map decName ds') ds'
    where decName (ConstantDecl (Name name) _) = name
          ds = mergeStep decls
          mergeStep d = mapM mergeCase $ groupBy (\a b -> decName a == decName b) d
          mergeCase [c@(ConstantDecl _ _)] = return c
          mergeCase ((ConstantDecl _ _):_) = error "Can't have multiple definitions of constant value!"

getVal :: String -> Eval Value
getVal name = do
    (fenv, _) <- lift get
    case lookup name fenv of
        Just x -> return x
        Nothing -> throwE $ NameError name

getDecl :: String -> Eval Decl
getDecl name = do
    (_, penv) <- lift get
    case lookup name penv of
        Just x -> return x
        Nothing -> throwE $ NameError name

eval :: Expr -> Eval Value
eval (NumLiteral l) = return $ Number l
eval (Brackets b) = eval b
eval (Arg (ArgName n)) = do
    catchE (getVal n) $ \e ->
        case e of
            NameError x | x == n -> do
                ConstantDecl _ val <- getDecl n
                eval val
            _ -> throwE e 
eval (Call a b) = do
    a' <- eval a
    b' <- eval b
    case a' of
        Function func -> func (Function func) (QuoteExpr b')
        x -> throwE $ TypeError $ "Can't call non-function!"
eval (LetExpr e (Name n) b) = do
    (fenv, penv) <- lift get
    e' <- eval e
    lift $ put $ ((n, e'):fenv, penv)
    val <- eval b
    lift $ put (fenv, penv)
    return val
eval (LambdaExpr (ArgName n) body) = do
    env@(fenv, penv) <- lift get
    return . Function $ \rec arg -> do
        arg' <- eval arg
        let newenv = ((n, arg'):fenv, penv)
        lift $ put newenv
        val <- eval body
        lift $ put env
        return val
eval (QuoteExpr v) = return v
eval (CaseExpr v pats) = do
    v' <- eval v
    let vals = flip map pats $ \(pat, expr) ->
            if v' `canMatch` pat
                then [introduce pat v' expr]
                else []
    case concat vals of
        [] -> throwE $ CaseError $ "Cannot match: " ++ show v'
        x -> eval $ head x
    where _ `canMatch` (IdentPattern _) = True
          v `canMatch` NilPattern = case v of
              List [] -> True
              _ -> False
          v `canMatch` (ConstantPattern c) = v == c
          v `canMatch` (ConsPattern a b) = case v of
              List (x:xs) -> x `canMatch` a && (List xs) `canMatch` b
              _ -> False
          introduce NilPattern _ expr = expr
          introduce (ConstantPattern _) _ expr = expr
          introduce (IdentPattern n) value expr = LetExpr (QuoteExpr value) n expr
          introduce (ConsPattern a b) value expr = case value of
              List (x:xs) -> introduce b (List xs) (introduce a x expr)

evalProgram :: ProgEnv -> Program -> Eval Value
evalProgram pstate prog = do
    let ((), env) = runProg $ runProgram prog
    lift $ put ([], env ++ pstate)
    ConstantDecl _ v <- getDecl "main"
    eval v
