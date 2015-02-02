module Printer.Language where

-- This is the printer for the result of a parse
import AST

class Printable a where
  pretty :: a -> String

instance Printable Identifier where
  pretty (Identifier s) = " " ++ s ++ " "

instance Printable Integer where
  pretty = show

instance Printable Double where
  pretty = show

instance Printable Char where
  pretty c = "'" ++ show c ++ "'"

instance Printable Constant where
  pretty (Integer i) = pretty i
  pretty (Float f) = pretty f
  pretty (Char c) = pretty c
  pretty (Enumeration i) = pretty i
