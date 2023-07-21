{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Lib
import System.Exit        (exitFailure, exitSuccess)
import System.Environment (getArgs, getProgName)
import System.Process     qualified as Proc

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [prog, prompt] -> do
      r <- windowPrompt prompt 
      case r of
        True  -> (Proc.createProcess (Proc.shell prog)) >>= \_ -> exitSuccess
        False -> exitSuccess
    _                -> do
      name <- getProgName
      putStrLn ("usage: " ++ name ++ " command prompt")
      exitFailure
