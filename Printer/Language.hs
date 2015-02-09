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
  pretty = show

instance Printable Constant where
  pretty (Integer i) = pretty i
  pretty (Float f) = pretty f
  pretty (Char c) = pretty c
  pretty (Enumeration i) = pretty i

instance Printable StringLiteral where
  pretty (StringLiteral s) = "\"" ++ s ++ "\""

instance Printable Expression where
  pretty (AssignmentExpression a) = pretty a
  pretty (Comma e a) = pretty e ++ ", " ++ pretty a

instance Printable AssignmentExpression where
  pretty (ConditionalExpression c) = pretty c
  pretty (NestedAssignmentExpression u op a) = pretty u ++ pretty op ++ pretty a

instance Printable AssignmentOperator where
  pretty SimpleAssign = "="
  pretty MultiplyAssign = "*="
  pretty DivideAssign = "/="
  pretty ModulusAssign = "%="
  pretty MinusAssign = "-="
  pretty LShiftAssign = "<<="
  pretty RShiftAssign = ">>="
  pretty AndAssign = "&="
  pretty OrAssign = "|="
  pretty XorAssign = "^="

instance Printable ConditionalExpression where
  pretty (LogicalOrExpression o) = pretty o
  pretty (Ternary cond t e) = pretty cond ++ " ? " ++ pretty t ++ " : " ++ pretty e

instance Printable LogicalOrExpression where
  pretty (LogicalAndExpression a) = pretty a
  pretty (LogicalOr o a) = pretty o ++ " || " ++ pretty a

instance Printable LogicalAndExpression where
  pretty (BitwiseOrExpression o) = pretty o
  pretty (LogicalAnd a o) = pretty a ++ " && " ++ pretty o

instance Printable BitwiseOrExpression where
  pretty (BitwiseXorExpression x) = pretty x
  pretty (BitwiseOr o x) = pretty o ++ " | " ++ pretty x

instance Printable BitwiseXorExpression where
  pretty (BitwiseAndExpression a) = pretty a
  pretty (BitwiseXor x a) = pretty x ++ " ^ " ++ pretty a

instance Printable BitwiseAndExpression where
  pretty (EqualityExpression e) = pretty e
  pretty (BitwiseAnd a e) = pretty a ++ " & " ++ pretty e

instance Printable EqualityExpression where
  pretty (RelationalExpression r) = pretty r
  pretty (Equal e r) = pretty e ++ " == " ++ pretty r
  pretty (UnEqual e r) = pretty e ++ " != " ++ pretty r

instance Printable RelationalExpression where
  pretty (ShiftExpression s) = pretty s
  pretty (Comparison r c s) = pretty r ++ pretty c ++ pretty s

instance Printable CompareOperator where
  pretty AST.LT = " < "
  pretty AST.GT = " > "
  pretty LTE = " <= "
  pretty GTE = " >= "

instance Printable ShiftExpression where
  pretty (AdditiveExpression a) = pretty a
  pretty (LShift s a) = pretty s ++ " << " ++ pretty a
  pretty (RShift s a) = pretty s ++ " >> " ++ pretty a

instance Printable AdditiveExpression where
  pretty (MultiplicativeExpression m) = pretty m
  pretty (Add a m) = pretty a ++ " + " ++ pretty m
  pretty (Subtract a m) = pretty a ++ " - " ++ pretty m

instance Printable MultiplicativeExpression where
  pretty (CastExpression c) = pretty c
  pretty (Multiply m c) = pretty m ++ " * " ++ pretty c
  pretty (Divide m c) = pretty m ++ " / " ++ pretty c
  pretty (Modulus m c) = pretty m ++ " % " ++ pretty c

instance Printable CastExpression where
  pretty (UnaryExpression e) = pretty e
  pretty (NestedCastExpression i c) = "("++pretty i++")"++pretty c

instance Printable UnaryExpression where
  pretty (PostfixExpression p) = pretty p
  pretty (PreIncrement u) = "++"++pretty u
  pretty (PreDecrement u) = "--"++pretty u
  pretty (UnaryOp op c) = pretty op ++ pretty c
  pretty (ExpressionSize u) = "sizeof " ++ pretty u
  pretty (TypeSize t) = "sizeof("++pretty t++")"

instance Printable UnaryOp where
  pretty AddressOf = "&"
  pretty Dereference = "*"
  pretty UnaryPlus = "+"
  pretty UnaryMinus = "-"
  pretty BitwiseNot = "~"
  pretty LogicalNot = "!"

instance Printable PostfixExpression where
  pretty (PrimaryExpression p) = pretty p
  pretty (ArraySubscript p e) = pretty p++"["++pretty e++"]"
  pretty (FunctionCall f args) = pretty f++"( " ++ pretty args ++ " )"
  pretty (Member p i) = pretty p++"."++pretty i
  pretty (PointerToMember p i) = pretty p++"->"++pretty i
  pretty (PostIncrement p) = pretty p++"++"
  pretty (PostDecrement p) = pretty p++"--"
  pretty (CompoundLiteral id il) = "("++pretty id++"){"++pretty il++"}"

instance Printable ArgumentExpressionList where
  pretty (ArgListHead a) = pretty a
  pretty (ArgList al a) = pretty al++", " ++ pretty a

instance Printable PrimaryExpression where
  pretty (IdentifierExpression i) = pretty i
  pretty (Constant c) = pretty c
  pretty (StringLiteralExpression s) = pretty s
  pretty (Expr e) = pretty e

instance Printable InitializerList where
  pretty _ = error "Initializer lists not implemented"
