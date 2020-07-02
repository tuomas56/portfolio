module Hask.Runtime.Prelude where
    
import Control.Monad.Trans.Except
import Hask.AST
import Hask.Interpreter

numericFunction :: String -> (Int -> Int -> Int) -> Decl
numericFunction n f = ConstantDecl (Name n) $ QuoteExpr $
    Function $ \_ x -> return $ Function $ \_ y -> do
        x' <- eval x
        y' <- eval y
        case (x', y') of
            (Number a, Number b) -> return $ Number $ a `f` b
            _ -> throwE $ TypeError $ "`" ++ n ++ "` requires numeric arguments!"

nilValue :: Decl
nilValue = ConstantDecl (Name "[]") $ QuoteExpr $ List []

consFunction :: Decl
consFunction = ConstantDecl (Name ":") $ QuoteExpr $
    Function $ \_ v -> return $ Function $ \_ l -> do
        v' <- eval v
        l' <- eval l
        case l' of
            List x -> return $ List $ v':x
            _ -> throwE $ TypeError "Second argument to `:` must be a list!"

primitives :: ProgEnv
primitives = [
    ("+", numericFunction "+" (+)),
    ("-", numericFunction "-" (-)),
    ("*", numericFunction "*" (*)),
    ("/", numericFunction "/" quot),
    ("[]", nilValue),
    (":", consFunction)]