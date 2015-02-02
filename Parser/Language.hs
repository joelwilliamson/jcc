module Parser.Language
       (language) where

import Parser.Keywords(keywords)
import Parser.Ops(ops)

import AST

import Text.Parsec
import qualified Text.Parsec.Token as T

import Control.Monad

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
identifier = do
  ident <- T.identifier lexer
  return $ Identifier ident

whitespace = T.whiteSpace lexer

constant = integerConstant <|> floatingConstant <|> characterConstant <|> enumerationConstant
integerConstant = (liftM Integer) $ T.integer lexer
floatingConstant = (liftM Float) $ T.float lexer
characterConstant = (liftM Char) $ T.charLiteral lexer
enumerationConstant = (liftM $ Enumeration . Identifier) $ T.identifier lexer

stringLiteral = T.stringLiteral lexer

language = do
  whitespace
  ident <- identifier
  whitespace
  return ident
