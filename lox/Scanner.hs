{-# LANGUAGE OverloadedStrings #-}

module Scanner (scan, ScannerResult, Parser) where

import Data.Char (isLetter)
import Data.Functor.Identity
import Data.Text
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (ParseErrorBundle, ParsecT, choice, empty, eof, many, manyTill, parse, satisfy, try, (<|>))
import Text.Megaparsec.Char (char, space, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Token
import TokenType

type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void Text a

-- idk what the second Text does, custom component
type ScannerResult = Either (ParseErrorBundle Text Void) [LoxTok]

-- type ScannerResult = Either String [Token]

scan :: String -> ScannerResult
scan src = parse (sc *> many (scanToken <* sc) <* eof) "" (T.pack src)

sc :: Parser ()
sc = L.space space1 lineCmnt Text.Megaparsec.empty
  where
    lineCmnt = L.skipLineComment "//"

scanToken :: Parser LoxTok
scanToken =
  try $
    scanCommentToken 1
      <|> try (scanNumberToken 1)
      <|> try (scanDoubleToken 1)
      <|> try (scanSingleToken 1)
      <|> try (scanStringToken 1)
      <|> try (scanKeywordToken 1)

scanSingleToken :: Int -> Parser LoxTok
scanSingleToken line =
  choice
    [ makeToken LeftParen "(",
      makeToken RightParen ")",
      makeToken LeftBrace "{",
      makeToken RightBrace "}",
      makeToken Comma ",",
      makeToken Dot ".",
      makeToken Minus "-",
      makeToken Plus "+",
      makeToken Semicolon ";",
      makeToken Star "*",
      makeToken Slash "/",
      makeToken Bang "!",
      makeToken Equal "=",
      makeToken Greater ">",
      makeToken Less "<"
    ]
  where
    makeToken :: TokenType -> String -> Parser LoxTok
    makeToken tt str = LoxTok tt Nothing line <$ L.symbol space (T.pack str)

scanDoubleToken :: Int -> Parser LoxTok
scanDoubleToken line =
  choice
    [ makeToken BangEqual "!=",
      makeToken EqualEqual "==",
      makeToken GreaterEqual ">=",
      makeToken LessEqual "<="
    ]
  where
    makeToken :: TokenType -> String -> Parser LoxTok
    makeToken tt str = LoxTok tt Nothing line <$ L.symbol space (T.pack str)

scanStringToken :: Int -> Parser LoxTok
scanStringToken line = do
  c <- char '\"' *> manyTill L.charLiteral (char '\"')
  return $ LoxTok (StringLit c) Nothing line

scanNumberToken :: Int -> Parser LoxTok
scanNumberToken line = do
  n <- try L.float <|> L.decimal
  return $ LoxTok (Number n) Nothing line

keywordMapping :: [(TokenType, String)]
keywordMapping =
  [ (And, "and"),
    (Class, "class"),
    (Else, "else"),
    (False_, "false"),
    (Fun, "fun"),
    (For, "for"),
    (If, "if"),
    (Nil, "nil"),
    (Or, "or"),
    (Print, "print"),
    (Return, "return"),
    (Super, "super"),
    (This, "this"),
    (True_, "true"),
    (Var, "var"),
    (While, "while")
  ]

scanIdentifierToken :: Int -> Parser LoxTok
scanIdentifierToken line = do
  fc <- firstChar
  rest <- many otherChar
  return $ LoxTok (Identifier (fc : rest)) Nothing line
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    otherChar = satisfy isLetter

scanKeywordToken :: Int -> Parser LoxTok
scanKeywordToken line = do
  (LoxTok tt _ _) <- scanIdentifierToken 1
  case tt of
    Identifier ident -> do
      let xs = [(tt', a) | (tt', a) <- keywordMapping, a == ident]
      case xs of
        ((tt'', _) : _) -> return $ LoxTok tt'' Nothing line
        [] -> return $ LoxTok (Identifier ident) Nothing line
    _ -> fail "Expected Identifier"

scanCommentToken :: Int -> Parser LoxTok
scanCommentToken line = LoxTok (Comment "") Nothing line <$ L.skipLineComment "//"
