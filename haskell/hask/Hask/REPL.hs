module Hask.REPL where
    
import ParserCombinators
import Hask.Parser
import Hask.Interpreter
import Hask.AST
import Hask.Runtime.Prelude
import System.IO
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import Data.List (intercalate)
import Control.Monad.IO.Class
    
readPrompt :: String -> String -> [String] -> InputT IO String
readPrompt prompt cont curval = do
    val <- getInputLine prompt
    case val of
        Just ""  -> return $ intercalate "\n" $ reverse curval
        Just val -> readPrompt cont cont (val : curval)
        Nothing  -> error "Can't read line!"
    
mainLoopIT :: ProgEnv -> InputT IO ()
mainLoopIT env = do
    s <- readPrompt "(Hask)> " "(Hask)| " []
    let prog = readProgram s
    case runParse prog of
        Right val -> do
            (val', env') <- liftIO $ runEval $ evalProgram (primitives ++ env) val
            case val' of
                Right val -> outputStrLn $ show val
                Left  err -> outputStrLn $ "Error: " ++ show err
        Left err -> outputStrLn $ "Syntax Error: " ++ show err
    mainLoopIT env
    
mainLoop :: String -> IO ()
mainLoop s = do
    t <- readFile s
    (_, (_, env')) <- runEval $ evalProgram primitives $ unpackEither $ runParse $ readProgram t
    runInputT (setComplete cf defaultSettings) $ mainLoopIT env'
    where cf = completeWord Nothing " ()" (\_ -> return [simpleCompletion "    "])
          unpackEither (Right val) = val
          unpackEither (Left err) = error $ show err
