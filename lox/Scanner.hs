{-# LANGUAGE OverloadedStrings #-}

module Scanner (Parser, scan, scan', ScannerResult) where

import Data.Char (isLetter)
import Data.Functor.Identity
import Data.Text hiding (length)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (ParseErrorBundle, ParsecT, SourcePos (SourcePos), choice, empty, eof, errorBundlePretty, getOffset, getSourcePos, many, manyTill, parse, satisfy, try, (<|>))
import Text.Megaparsec.Char (char, space, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Token
import TokenType

type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void Text a

type ScannerResult = Either (ParseErrorBundle Text Void) LoxTokStream

scan' :: String -> IO ()
scan' src = case parse (sc *> many (scanToken <* sc) <* eof) "" (T.pack src) of
  Right ts -> print ts
  Left err -> putStrLn $ errorBundlePretty err

scan :: String -> ScannerResult
scan src = case parse (sc *> many (scanToken <* sc) <* eof) "" (T.pack src) of
  Right ts -> Right $ LoxTokStream src ts
  Left err -> Left err -- assume no scan errors for now

sc :: Parser ()
sc = L.space space1 lineCmnt Text.Megaparsec.empty
  where
    lineCmnt = L.skipLineComment "//"

scanToken :: Parser (WithPos LoxTok)
scanToken =
  try $
    scanCommentToken
      <|> try scanNumberToken
      <|> try scanDoubleToken
      <|> try scanSingleToken
      <|> try scanStringToken
      <|> try scanKeywordToken

scanSingleToken :: Parser (WithPos LoxTok)
scanSingleToken =
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
    makeToken :: TokenType -> String -> Parser (WithPos LoxTok)
    makeToken tt str = do
      start <- getSourcePos
      _ <- L.symbol space (T.pack str)
      end <- getSourcePos
      pure $ WithPos start end 1 (LoxTok tt (Just str))

scanDoubleToken :: Parser (WithPos LoxTok)
scanDoubleToken =
  choice
    [ makeToken BangEqual "!=",
      makeToken EqualEqual "==",
      makeToken GreaterEqual ">=",
      makeToken LessEqual "<="
    ]
  where
    makeToken :: TokenType -> String -> Parser (WithPos LoxTok)
    makeToken tt str = do
      start <- getSourcePos
      _ <- L.symbol space (T.pack str)
      end <- getSourcePos
      pure $ WithPos start end 2 (LoxTok tt (Just str))

scanStringToken :: Parser (WithPos LoxTok)
scanStringToken = do
  start <- getSourcePos
  c <- char '\"' *> manyTill L.charLiteral (char '\"')
  end <- getSourcePos
  pure $ WithPos start end (length c) (LoxTok (StringLit c) Nothing)

scanNumberToken :: Parser (WithPos LoxTok)
scanNumberToken = do
  start <- getSourcePos
  n <- try L.float <|> L.decimal
  end <- getSourcePos

  return $ WithPos start end (length . show $ n) (LoxTok (Number n) Nothing)

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

scanIdentifierToken :: Parser (WithPos LoxTok)
scanIdentifierToken = do
  start <- getSourcePos
  fc <- firstChar
  rest <- many otherChar
  end <- getSourcePos
  let str = fc : rest
  return $ WithPos start end (length str) (LoxTok (Identifier str) Nothing)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    otherChar = satisfy isLetter

scanKeywordToken :: Parser (WithPos LoxTok)
scanKeywordToken = do
  start <- getSourcePos
  (WithPos _ _ _ (LoxTok tt _)) <- scanIdentifierToken
  end <- getSourcePos
  case tt of
    Identifier ident -> do
      let xs = [(tt', a) | (tt', a) <- keywordMapping, a == ident]
      case xs of
        ((tt'', a') : _) -> return $ WithPos start end (length a') (LoxTok tt'' Nothing)
        [] -> return $ WithPos start end (length ident) (LoxTok (Identifier ident) Nothing)
    _ -> fail "Expected Identifier"

-- FIXME
scanCommentToken :: Parser (WithPos LoxTok)
scanCommentToken = do
  start <- getSourcePos
  startOffset <- getOffset
  _ <- L.skipLineComment "//"
  end <- getSourcePos
  endOffset <- getOffset

  return $ WithPos start end (endOffset - startOffset) (LoxTok (Comment "") Nothing)
