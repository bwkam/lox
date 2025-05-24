{-# LANGUAGE OverloadedStrings #-}

module Scanner (scan) where

import Data.Char (isLetter)
import Data.Functor.Identity
import Data.Text
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (ParseErrorBundle, ParsecT, choice, eof, many, manyTill, parse, satisfy, try, (<|>))
import Text.Megaparsec.Char (char, space)
import qualified Text.Megaparsec.Char.Lexer as L
import Token
import TokenType

type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void Text a

-- idk what the second Text does, custom component
type ScannerResult = Either (ParseErrorBundle Text Void) [Token]

-- type ScannerResult = Either String [Token]

scan :: String -> ScannerResult
scan src = parse (many scanToken <* eof) "" (T.pack src)

scanToken :: Parser Token
scanToken =
  try $
    scanCommentToken 1
      <|> try (scanDoubleToken 1)
      <|> try (scanSingleToken 1)
      <|> try (scanStringToken 1)
      <|> try (scanNumberToken 1)
      <|> try (scanKeywordToken 1)

scanSingleToken :: Int -> Parser Token
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
    makeToken :: TokenType -> String -> Parser Token
    makeToken tt str = Token tt Nothing line <$ L.symbol space (T.pack str)

scanDoubleToken :: Int -> Parser Token
scanDoubleToken line =
  choice
    [ makeToken BangEqual "!=",
      makeToken EqualEqual "==",
      makeToken GreaterEqual ">=",
      makeToken LessEqual "<="
    ]
  where
    makeToken :: TokenType -> String -> Parser Token
    makeToken tt str = Token tt Nothing line <$ L.symbol space (T.pack str)

scanStringToken :: Int -> Parser Token
scanStringToken line = do
  c <- char '\"' *> manyTill L.charLiteral (char '\"')
  return $ Token (StringLit c) Nothing line

scanNumberToken :: Int -> Parser Token
scanNumberToken line = do
  n <- try L.float <|> L.decimal
  return $ Token (Number n) Nothing line

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

scanIdentifierToken :: Int -> Parser Token
scanIdentifierToken line = do
  fc <- firstChar
  rest <- many otherChar
  return $ Token (Identifier (fc : rest)) Nothing line
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    otherChar = satisfy isLetter

scanKeywordToken :: Int -> Parser Token
scanKeywordToken line = do
  (Token tt _ _) <- scanIdentifierToken 1
  case tt of
    Identifier ident -> do
      let xs = [(tt', a) | (tt', a) <- keywordMapping, a == ident]
      case xs of
        ((tt'', _) : _) -> return $ Token tt'' Nothing line
        [] -> return $ Token (Identifier ident) Nothing line
    _ -> fail "Expected Identifier"

scanCommentToken :: Int -> Parser Token
scanCommentToken line = Token (Comment "") Nothing line <$ L.skipLineComment "//"
