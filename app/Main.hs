{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.Text.IO as T
import Grammar
import Parser
import System.Environment (getArgs)
import System.IO
import Test.QuickCheck (arbitrary, generate)

main :: IO ()
main = do
  cmds <- getArgs
  case cmds of
    []             -> hPutStrLn stderr "commands: `generate` or `parse`"
    ("generate":_) -> T.putStrLn . showTune @Tune =<< generate arbitrary
    ("parse":_)    -> do
      inp <- T.getContents
      case parseTune inp of
        Left err -> hPrint stderr err
        Right tn -> T.putStrLn $ showTune tn
    (cmd:_) -> hPutStrLn stderr $ "Unknown command: " ++ cmd
