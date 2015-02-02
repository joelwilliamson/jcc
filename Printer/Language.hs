module Printer.Language where

-- This is the printer for the result of a parse
import AST

class Printable a where
  pretty :: a -> String

instance Printable AST where
  pretty (Identifier s) = " " ++ s ++ " "
