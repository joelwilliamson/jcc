module Parser.Language
       (language) where

import Parser.Keywords(keywords)
import Parser.Ops(ops)

import AST

import Text.Parsec
import qualified Text.Parsec.Token as T

import Data.Functor.Identity(Identity)

import Control.Monad
import Control.Applicative((<*))
import Data.List(foldl',foldl1')

-- This functions similarly to chainl1, but the first parse's result has
-- baseConstructor applied to it, allowing the overall result's type to
-- be different from the sub-parses
chainl1' :: (Stream s m t) =>
            ParsecT s u m a ->
            ParsecT s u m (b -> a -> b) ->
            (a -> b) ->
            ParsecT s u m b
chainl1' parser op baseConstructor =  parser >>= (rest . baseConstructor)
  where
    rest x =
      do
        f <- op
        y <- parser
        rest (f x y)
      <|> return x

languageDef :: T.LanguageDef st
languageDef = T.LanguageDef {
  T.commentStart = "/*"
  , T.commentEnd = "*/"
  , T.commentLine = "//"
  , T.nestedComments = True -- This doesn't match the C standard, but will be convenient
  , T.identStart = letter <|> char '_' -- This matches all Unicode letters
  , T.identLetter = alphaNum <|> char '_'
  , T.opStart = oneOf "~!%^&*-+=|:<>,?/."
  , T.opLetter = oneOf "|&=+-><"
  , T.reservedNames = keywords
  , T.reservedOpNames = ops
  , T.caseSensitive = True
  }

lexer = T.makeTokenParser languageDef
identifier = liftM Identifier $ T.identifier lexer

whitespace = T.whiteSpace lexer
comma = T.comma lexer
commaOp = comma >> return Comma
reserved = T.reserved lexer
reservedOp op = (T.reservedOp lexer) op >> return op
parens = T.parens lexer
brackets = T.brackets lexer
braces = T.braces lexer
dot = T.dot lexer

constant = integerConstant <|> floatingConstant <|> characterConstant <|> enumerationConstant
integerConstant = liftM Integer $ T.integer lexer
floatingConstant = liftM Float $ T.float lexer
characterConstant = liftM Char $ T.charLiteral lexer
enumerationConstant = liftM (Enumeration . Identifier) $ T.identifier lexer

stringLiteral = liftM StringLiteral $ T.stringLiteral lexer

expression = chainl1' assignmentExpression commaOp AssignmentExpression

assignSymbols = [("=",SimpleAssign)
                ,("*=",MultiplyAssign)
                ,("/=",DivideAssign)
                ,("%=",ModulusAssign)
                ,("+=",PlusAssign)
                ,("-=",MinusAssign)
                ,("<<=",LShiftAssign)
                ,(">>=",RShiftAssign)
                ,("&=",AndAssign)
                ,("^=",XorAssign)
                ,("|=",OrAssign)]

assignOps = choice $ map (\(sym,cons) -> reservedOp sym >> return cons) assignSymbols

assignmentExpression =
  try (do
          ue <- unaryExpression
          op <- choice $ map (reservedOp . fst) assignSymbols
          ae <- assignmentExpression
          case op `lookup` assignSymbols of
            Nothing -> parserFail $ "Invalid assignment operator " ++ op
            Just op -> return $ NestedAssignmentExpression ue op ae
      ) <|> liftM ConditionalExpression conditionalExpression

conditionalExpression =
  try (do
          loe <- logicalOrExpression
          reservedOp "?"
          e <- expression
          reservedOp ":"
          ce <- conditionalExpression
          return $ Ternary loe e ce
      ) <|> liftM LogicalOrExpression logicalOrExpression

binaryOperatorExpression cons first op second fallbackCons fallbackParse=
  binaryOpExpression cons first op second  <|> liftM fallbackCons fallbackParse

binaryOpExpression cons first op second =
    try (do
          f <- first
          reservedOp op
          s <- second
          return $ cons f s)

makeOp op constructor = reservedOp op >> return constructor

logicalOrOp = makeOp "||" LogicalOr
logicalOrExpression = chainl1' logicalAndExpression logicalOrOp LogicalAndExpression

logicalAndOp = makeOp "&&" LogicalAnd
logicalAndExpression = chainl1' bitwiseOrExpression logicalAndOp BitwiseOrExpression

bitwiseOrOp = makeOp "|" BitwiseOr
bitwiseOrExpression = chainl1' bitwiseXorExpression bitwiseOrOp BitwiseXorExpression

bitwiseXorOp = makeOp "^" BitwiseXor
bitwiseXorExpression = chainl1' bitwiseAndExpression bitwiseXorOp BitwiseAndExpression

bitwiseAndOp = makeOp "&" BitwiseAnd
bitwiseAndExpression = chainl1' equalityExpression bitwiseAndOp EqualityExpression

equalityOps = makeOp "==" Equal <|> makeOp "!=" UnEqual
equalityExpression = chainl1' relationalExpression equalityOps RelationalExpression

relationalOps = 
  foldl1 (<|>) $ map (\(sym,op) ->
                       reservedOp sym >> return (\re se ->
                                                  Comparison re op se))
  [("<",AST.LT)
  ,(">",AST.GT)
  ,("<=",LTE)
  ,(">+",GTE)]
relationalExpression = chainl1' shiftExpression relationalOps ShiftExpression

shiftOps = makeOp "<<" LShift <|> makeOp ">>" RShift
shiftExpression = chainl1' additiveExpression shiftOps AdditiveExpression

additiveOps = makeOp "+" Add <|> makeOp "-" Subtract
additiveExpression = chainl1' multiplicativeExpression additiveOps MultiplicativeExpression

multiplicativeOps = makeOp "*" Multiply <|> makeOp "/" Divide <|> makeOp "%" Modulus
multiplicativeExpression = chainl1' castExpression multiplicativeOps CastExpression

castExpression =
  try (do
          typeName <- parens identifier
          ce <- castExpression
          return $ NestedCastExpression typeName ce)
  <|> liftM UnaryExpression unaryExpression

unaryExpression =
  (reservedOp "++" >> unaryExpression >>= return . PreIncrement)
  <|> (reservedOp "--" >> unaryExpression >>= return . PreDecrement)
  <|> (reserved "sizeof" >> unaryExpression >>= return . ExpressionSize)
  <|> (reserved "sizeof" >> parens identifier >>= return . TypeSize)
  <|> prefixExpression
  <|> liftM PostfixExpression postfixExpression
  
prefixExpression =
  foldl1' (<|>)
  $ map (\(sym,cons) -> reservedOp sym >> castExpression >>= return . UnaryOp cons)
  [("&",AddressOf)
  ,("*",Dereference)
  ,("+",UnaryPlus)
  ,("-",UnaryMinus)
  ,("~",BitwiseNot)
  ,("!",LogicalNot)]

compoundLiteral = liftM2 CompoundLiteral (parens identifier) (braces initializerList)

postfixExpression = rest (liftM PrimaryExpression primaryExpression
                     <|> compoundLiteral)
  where rest postEx = (rest $ liftM2 ArraySubscript postEx $ brackets expression)
                      <|> (rest $ liftM2 FunctionCall postEx
                           $ parens argumentExpressionList)
                      <|> (rest $ liftM2 Member postEx $ dot >> identifier)
                      <|> (rest $ liftM2 PointerToMember postEx
                           $ reservedOp "->" >> identifier)
                      <|> (rest $ liftM PostIncrement $ postEx <* reservedOp "++")
                      <|> (rest $ liftM PostDecrement $ postEx <* reservedOp "--")
                      <|> postEx

argumentExpressionList = try (do
                                 cdr <- argumentExpressionList
                                 comma
                                 car <- assignmentExpression
                                 return $ ArgList cdr car)
                         <|> liftM ArgListHead assignmentExpression

primaryExpression =
  liftM IdentifierExpression identifier
  <|> liftM Constant constant
  <|> liftM StringLiteralExpression stringLiteral
  <|> (liftM Expr $ parens expression)

constantExpression = liftM ConstantExpression conditionalExpression

initializer = try $ liftM Assignment assignmentExpression
              <|> liftM IList initializerList

initializerList = liftM InitializerList
                  $ (flip sepBy) comma
                  $ liftM2 (,) (optionMaybe designation) initializer

designation = liftM DesignationList
  $ many (choice [liftM SubscriptDesignator $ brackets constantExpression
                 , liftM MemberDesignator $ between dot (return ()) identifier])

language = do
  ident <- identifier
  return ident
