module Main 
  ( main
  ) where

import Test.Tasty

import qualified Bf.SyntaxSpec as SyntaxSpec

main :: IO ()
main = defaultMain $ testGroup "Bf"
  [ SyntaxSpec.spec
  ]
