--{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Testing.Parser.Language(tests) where

import Control.Monad

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Parser.Language
import AST
import Printer.Language
import Text.Parsec

instance Arbitrary AST where
  arbitrary = oneof [
    liftM Identifier arbitrary
    ]

instance Eq ParseError where
  _ == _ = False

multiParse s = do
  first <- parse language "" s
  printed <- Right $ pretty first
  second <- parse language "" printed
  return second

prop_idempotent s =
  (not (null s)) ==>
  parse language "" s == multiParse s

prop_invalid s = False


--identifierTests = testGroup "Identifier lexing" [testIdent1]

tests = [testProperty "Parser idempotency" prop_idempotent]
