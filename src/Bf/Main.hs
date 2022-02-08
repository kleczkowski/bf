module Bf.Main
  ( main
  , run
  ) where

import Bf.Compiler
import Bf.Opts
import Bf.Syntax

import qualified Data.Text.IO as Text

-- | Entry point of interpreter.
main :: IO ()
main = runWithOpts run

-- | Runs an interpreter with options.
run :: Opts -> IO ()
run opts = do
  inputContents <- maybe
    Text.getContents
    readFileText
    (optsInput opts)
  prog <- either
    fail
    pure
    (parseProgram inputContents)
  runCmds prog opts
