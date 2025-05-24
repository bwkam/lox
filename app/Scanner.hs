{-# LANGUAGE OverloadedStrings #-}

module Scanner (scan) where

import Data.Functor.Identity
import Data.Text
import qualified Data.Text as T
import Data.Void
import qualified Lox
import qualified Lox as Lox
import Text.Megaparsec (ParsecT, choice, errorBundlePretty, many, parse)
import qualified Text.Megaparsec as Text.Megaparsec.Error
import Text.Megaparsec.Char (char, space, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Token
import TokenType

type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void Text a

scan :: String -> Either String [Token]
scan src = case parse (scanTokens 1) "" (T.pack src) of
  Right tokens -> Right tokens
  Left err -> Left (errorBundlePretty err)

scanTokens :: Int -> ParsecT Void Text Identity [Token]
scanTokens line = many (scanToken line)

scanToken :: Int -> Parser Token
scanToken line =
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
      makeToken Star "*"
    ]
  where
    makeToken :: TokenType -> String -> Parser Token
    makeToken tt str = Token tt str line <$ L.symbol space (T.pack str)
