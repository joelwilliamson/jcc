module AST where

data Identifier = Identifier String
         deriving (Eq,Show)

data Constant = Integer Integer
              | Float Double
              | Char Char
              | Enumeration Identifier
              deriving (Eq,Show)

data StringLiteral = StringLiteral String
                     deriving (Eq,Show)

data Expression = AssignmentExpression AssignmentExpression
                | Comma Expression AssignmentExpression

data AssignmentExpression = ConditionalExpression ConditionalExpression
                          | NestedAssignmentExpression UnaryExpression AssignmentOperator AssignmentExpression

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

data ConditionalExpression = LogicalOrExpression LogicalOrExpression
                           | Ternary LogicalOrExpression Expression ConditionalExpression

data LogicalOrExpression = LogicalAndExpression LogicalAndExpression
                         | LogicalOr LogicalOrExpression LogicalAndExpression

data LogicalAndExpression = BitwiseOrExpression BitwiseOrExpression
                          | LogicalAnd LogicalAndExpression BitwiseOrExpression

data BitwiseOrExpression = BitwiseXorExpression BitwiseXorExpression
                         | BitwiseOr BitwiseOrExpression BitwiseXorExpression

data BitwiseXorExpression = BitwiseAndExpression BitwiseAndExpression
                          | Xor BitwiseXorExpression BitwiseAndExpression

data BitwiseAndExpression = EqualityExpression EqualityExpression
                          | BitwiseAnd BitwiseAndExpression EqualityExpression

data EqualityExpression = RelationalExpression RelationalExpression
                        | Equal EqualityExpression RelationalExpression
                        | DisEqual EqualityExpression RelationalExpression

data RelationalExpression = ShiftExpression ShiftExpression
                          | Comparison RelationalExpression CompareOperator ShiftExpression

data CompareOperator = LT | GT | LTE | GTE

data ShiftExpression = AdditiveExpression AdditiveExpression
                     | LShift ShiftExpression AdditiveExpression
                     | RShift ShiftExpression AdditiveExpression

data AdditiveExpression = MultiplicativeExpression MultiplicativeExpression
                        | Add AdditiveExpression MultiplicativeExpression
                        | Subtract AdditiveExpression MultiplicativeExpression

data MultiplicativeExpression = CastExpression CastExpression
                              | Multiply MultiplicativeExpression CastExpression
                              | Divide MultiplicativeExpression CastExpression
                              | Modulus MultiplicativeExpression CastExpression

data CastExpression = UnaryExpression UnaryExpression
                    | NestedCastExpression Identifier CastExpression

data UnaryExpression = PostfixExpression PostfixExpression
                     | PreIncrement UnaryExpression
                     | PreDecrement UnaryExpression
                     | UnaryOp UnaryOp CastExpression
                     | ExpressionSize UnaryExpression
                     | TypeSize Identifier

data UnaryOp = AddressOf | Dereference | UnaryPlus | UnaryMinus | BitwiseNot | LogicalNot

data PostfixExpression = PrimaryExpression PrimaryExpression
                       | ArraySubscript PostfixExpression Expression
                       | FunctionCall PostfixExpression ArgumentExpressionList
                       | Member PostfixExpression Identifier
                       | PointerToMember PostfixExpression Identifier
                       | PostIncrement PostfixExpression
                       | PostDecrement PostfixExpression
                       | CompoundLiteral Identifier InitializerList

data ArgumentExpressionList = ArgListHead AssignmentExpression
                            | ArgList ArgumentExpressionList AssignmentExpression

data PrimaryExpression = IdentifierExpression Identifier
                       | Constant Constant
                       | StringLiteralExpression StringLiteral
                       | Expr Expression

data InitializerList = Null -- See section 6.7.8 for details
