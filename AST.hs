
module AST where

-- The syntax is taken from C99 (ISO/IEC 9899:1999), section 6

-- 6.4.2
data Identifier = Identifier String
                deriving (Eq,Show)

-- 6.4.4
data Constant = Integer Integer
              | Float Double
              | Char Char
              | Enumeration Identifier
              deriving (Eq,Show)

--6.4.5
data StringLiteral = StringLiteral String
                     deriving (Eq,Show)


-- Expressions: 6.5
data Expression = AssignmentExpression AssignmentExpression
                | Comma Expression AssignmentExpression
                deriving (Eq,Show)

data AssignmentExpression = ConditionalExpression ConditionalExpression
                          | NestedAssignmentExpression UnaryExpression AssignmentOperator AssignmentExpression
                          deriving (Eq,Show)

data AssignmentOperator = SimpleAssign
                        | MultiplyAssign
                        | DivideAssign
                        | ModulusAssign
                        | PlusAssign
                        | MinusAssign
                        | LShiftAssign
                        | RShiftAssign
                        | AndAssign
                        | XorAssign
                        | OrAssign
                        deriving (Eq,Show)

data ConditionalExpression = LogicalOrExpression LogicalOrExpression
                           | Ternary LogicalOrExpression Expression ConditionalExpression
                           deriving (Eq,Show)

data LogicalOrExpression = LogicalAndExpression LogicalAndExpression
                         | LogicalOr LogicalOrExpression LogicalAndExpression
                         deriving (Eq,Show)

data LogicalAndExpression = BitwiseOrExpression BitwiseOrExpression
                          | LogicalAnd LogicalAndExpression BitwiseOrExpression
                          deriving (Eq,Show)

data BitwiseOrExpression = BitwiseXorExpression BitwiseXorExpression
                         | BitwiseOr BitwiseOrExpression BitwiseXorExpression
                         deriving (Eq,Show)

data BitwiseXorExpression = BitwiseAndExpression BitwiseAndExpression
                          | BitwiseXor BitwiseXorExpression BitwiseAndExpression
                          deriving (Eq,Show)

data BitwiseAndExpression = EqualityExpression EqualityExpression
                          | BitwiseAnd BitwiseAndExpression EqualityExpression
                          deriving (Eq,Show)

data EqualityExpression = RelationalExpression RelationalExpression
                        | Equal EqualityExpression RelationalExpression
                        | UnEqual EqualityExpression RelationalExpression
                        deriving (Eq,Show)

data RelationalExpression = ShiftExpression ShiftExpression
                          | Comparison RelationalExpression CompareOperator ShiftExpression
                          deriving (Eq,Show)

data CompareOperator = LT | GT | LTE | GTE
                     deriving (Eq,Show)

data ShiftExpression = AdditiveExpression AdditiveExpression
                     | LShift ShiftExpression AdditiveExpression
                     | RShift ShiftExpression AdditiveExpression
                     deriving (Eq,Show)

data AdditiveExpression = MultiplicativeExpression MultiplicativeExpression
                        | Add AdditiveExpression MultiplicativeExpression
                        | Subtract AdditiveExpression MultiplicativeExpression
                        deriving (Eq,Show)

data MultiplicativeExpression = CastExpression CastExpression
                              | Multiply MultiplicativeExpression CastExpression
                              | Divide MultiplicativeExpression CastExpression
                              | Modulus MultiplicativeExpression CastExpression
                              deriving (Eq,Show)

data CastExpression = UnaryExpression UnaryExpression
                    | NestedCastExpression Identifier CastExpression
                    deriving (Eq,Show)

data UnaryExpression = PostfixExpression PostfixExpression
                     | PreIncrement UnaryExpression
                     | PreDecrement UnaryExpression
                     | UnaryOp UnaryOp CastExpression
                     | ExpressionSize UnaryExpression
                     | TypeSize Identifier
                     deriving (Eq,Show)

data UnaryOp = AddressOf | Dereference | UnaryPlus | UnaryMinus | BitwiseNot | LogicalNot
             deriving (Eq,Show)

-- Every postfix expression starts with either a '(' (Compound Literals) or a
-- primary expression
data PostfixExpression = PrimaryExpression PrimaryExpression
                       | ArraySubscript PostfixExpression Expression
                       | FunctionCall PostfixExpression ArgumentExpressionList
                       | Member PostfixExpression Identifier
                       | PointerToMember PostfixExpression Identifier
                       | PostIncrement PostfixExpression
                       | PostDecrement PostfixExpression
                       | CompoundLiteral Identifier InitializerList
                       deriving (Eq,Show)

data ArgumentExpressionList = ArgListHead AssignmentExpression
                            | ArgList ArgumentExpressionList AssignmentExpression
                            deriving (Eq,Show)

data PrimaryExpression = IdentifierExpression Identifier
                       | Constant Constant
                       | StringLiteralExpression StringLiteral
                       | Expr Expression
                       deriving (Eq,Show)

-- 6.6 Constant Expressions
-- A constant expression had different semantic constraints, so it should be
-- represented as a different type
data ConstantExpression = ConstantExpression ConditionalExpression
                        deriving (Eq,Show)

-- §6.7 Declarations
-- The spec calls for init-declarator-list_opt. It then gives a constraint
-- requiring each declaration to have at least one declarator. The
-- reasoning here is not clear, so for now I am just going to require
-- a declarator list
data Declaration = Declaration [DeclarationSpecifier] [InitDeclarator]
                 deriving (Eq,Show)

data DeclarationSpecifier = StorageClass StorageClass
                          | TypeSpecifier TypeSpecifier
                          | TypeQualifier TypeQualifier
                          | FunctionSpecifier FunctionSpecifier
                          deriving (Eq,Show)

data InitDeclarator = Declarator Declarator
                    | InitDeclarator Declarator Initializer
                    deriving (Eq,Show)

-- §6.7.1 Storage-Class Specifiers
data StorageClass = Typedef | Extern | Static | Auto | Register
                  deriving (Eq,Show)

-- §6.7.2 Type Specifiers
-- Only a subset of possible combinations are allowed. The data type should
-- reflect this
data TypeSpecifier = VoidT | CharT | ShortT | IntT | LongT | FloatT | DoubleT
                   | Signed | Unsigned | Bool | Complex | Imaginary
                   | StructUnionSpecifier StructUnionSpecifier
                   | EnumSpecifier EnumSpecifier
           --        | TypedefName TypedefName
                   deriving (Eq,Show)

data StructUnionSpecifier = StructLocalDefinition StructUnion (Maybe Identifier) [StructDeclaration]
                          | StructElseDefinition StructUnion Identifier
                          deriving (Eq,Show)

data StructUnion = Struct-- Union
                 deriving (Eq,Show)

data StructDeclaration = StructDeclaration [TypeSpecifier] [StructDeclarator]
                       deriving (Eq,Show)

data StructDeclarator = RegularMember-- Declarator
                      | Bitfield-- (Maybe Declarator) ConstantExpression
                      deriving (Eq,Show)

data EnumSpecifier = Enum (Maybe Identifier) [Enumerator]
                   deriving (Eq,Show)
-- There is a constructor for enumeration-constants, but it isn't clear why
-- we don't just use an identifier here.
data Enumerator = Enumerator Identifier
                deriving (Eq,Show)

-- §6.7.3 Type Qualifiers
data TypeQualifier = Const | Restrict | Volatile
                   deriving (Eq,Show)
-- §6.7.4 Function Specifiers
data FunctionSpecifier = Inline
                       deriving (Eq,Show)
-- 6.7.8
data Initializer = Assignment AssignmentExpression
                 | IList InitializerList
                 deriving (Eq,Show)

-- The Initializer list specified in the C standard is left recursive.
newtype InitializerList = InitializerList [((Maybe Designation),Initializer)]
                        deriving (Eq,Show)

newtype Designation = DesignationList [Designator]
                    deriving (Eq,Show)

data Designator = SubscriptDesignator ConstantExpression
                | MemberDesignator Identifier
                deriving (Eq,Show)
