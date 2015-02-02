module Main where

import qualified Testing.Parser.Language as L

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Control.Monad

tests = [
  testGroup "Parser Testing" L.tests
  ]

main = defaultMain tests
