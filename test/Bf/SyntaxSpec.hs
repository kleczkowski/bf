module Bf.SyntaxSpec
  ( spec
  ) where

import Bf.Syntax (Cmd (..), parseProgram)

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec = testGroup "Syntax"
  [ testCase "Empty input" $ do
      assertEqual "returns empty command list" 
        (parseProgram "") (Right [])
      assertEqual "returns empty command list"
        (parseProgram "qwerty") (Right [])
  , testCase "Simple input" $ do
      let 
        text = "[><+-,.]><+-,."
        expected = [IncPtr, DecPtr, IncDat, DecDat, ChrGet, ChrPut]
      assertEqual "returns a list of commands"
        (parseProgram text)
        (Right (Loop expected : expected))
  , testCase "Whitespaces" $ do
      let 
        text = "OOOO > < +aaa-bbb,ccc.?*()"
        expected = [IncPtr, DecPtr, IncDat, DecDat, ChrGet, ChrPut]
      assertEqual "returns a list of commands"
        (parseProgram text) 
        (Right expected)
  , testCase "Nested loops" $ do
      let
        text = "[[>]]"
        expected = [Loop [Loop [IncPtr]]]
      assertEqual "returns a list of commands"
        (parseProgram text)
        (Right expected)
  ]