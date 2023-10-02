{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import PromptWindow
import System.Exit        (exitFailure, exitSuccess)
import System.Environment (getArgs, getProgName)
import System.Process     (callCommand, shell)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [prog, prompt] -> do
      r <- promptWindow prompt 
      case r of
        True  -> (callCommand prog) >>= \_ -> exitSuccess
        False -> exitSuccess
    _ -> do
      name <- getProgName
      putStrLn ("usage: " ++ name ++ " command prompt")
      exitFailure
