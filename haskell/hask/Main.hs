{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Hask.REPL
import System.Environment

intro :: IO ()
intro = putStr $ unlines [
    " _    _              _     |  Version 0.0.1",
    "| |  | |            | |    |  (c) Tuomas Laakkonen 2016",
    "| |__| |  __ _  ___ | | __ |",
    "|  __  | / _` |/ __|| |/ / |",
    "| |  | || (_| |\\__ \\|   <  |",
    "|_|  |_| \\__,_||___/|_|\\_\\ |  Use Ctrl-C to quit."]

main :: IO ()
main = getArgs >>= \case
    [x] -> intro >> Hask.REPL.mainLoop x