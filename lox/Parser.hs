{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Parser where

import Data.Functor.Identity (Identity)
import qualified Data.Set as Set
import Data.Void
import Expr (Expr (Binary, Literal, Unary), LiteralValue (Number))
import Text.Megaparsec (MonadParsec (lookAhead, try), ParsecT, Stream (Token), many, satisfy, single, token, (<|>))
import qualified Text.Megaparsec.Char.Lexer as L
import Token (LoxTok (LoxTok))
import TokenType (TokenType (..))

type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void [LoxTok] a

-- parse :: [LoxTok] -> Parser Expr
-- parse src = parse

expression :: Parser Expr
expression = equality

equality :: Parser Expr
equality = do
  expr <- comparison

  go expr
  where
    parseOp expr = do
      op <- matchMany [BangEqual, EqualEqual]
      right <- comparison
      go (Binary expr op right)

    go expr = do
      parseOp expr <|> return expr

comparison :: Parser Expr
comparison = do
  expr <- term

  go expr
  where
    parseOp expr = do
      op <- matchMany [Greater, GreaterEqual, Less, LessEqual]
      right <- primary
      go (Binary expr op right)

    go expr = do
      parseOp expr <|> return expr

term :: Parser Expr
term = do
  expr <- factor

  go expr
  where
    parseOp expr = do
      op <- matchMany [Plus, Minus]
      right <- term
      go (Binary expr op right)

    go expr = do
      parseOp expr <|> return expr

factor :: Parser Expr
factor = do
  expr <- unary

  go expr
  where
    parseOp expr = do
      op <- matchMany [Slash, Star]
      right <- term
      go (Binary expr op right)

    go expr = do
      parseOp expr <|> return expr

unary :: Parser Expr
unary = do
  go <|> primary
  where
    go = do
      op <- matchMany [Bang, Minus]
      right <- unary
      return $ Unary op right

primary :: Parser Expr
primary = do
  (LoxTok (TokenType.Number x) _ _) <- matchSingle (TokenType.Number 0)
  pure $ Literal (Expr.Number x)

matchMany :: [TokenType] -> Parser LoxTok
matchMany tts = satisfy (\(LoxTok tt' _ _) -> any (matches tt') tts)
  where
    matches :: TokenType -> TokenType -> Bool
    matches tt' tt = case (tt, tt') of
      (Identifier _, Identifier _) -> True
      (TokenType.Number _, TokenType.Number _) -> True
      (Comment _, Comment _) -> True
      (StringLit _, StringLit _) -> True
      _ -> tt == tt'

matchSingle :: TokenType -> Parser LoxTok
matchSingle tt = satisfy f
  where
    f (LoxTok tt' _ _) =
      case (tt, tt') of
        (Identifier _, Identifier _) -> True
        (TokenType.Number _, TokenType.Number _) -> True
        (Comment _, Comment _) -> True
        (StringLit _, StringLit _) -> True
        _ -> tt == tt'