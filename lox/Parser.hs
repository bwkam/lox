{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Parser where

import Control.Monad (replicateM_, when)
import Data.Functor.Identity (Identity)
import Data.List (intercalate, intersperse)
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Void
import Expr (Expr (Binary, Literal, Unary), LiteralValue (Boolean, Nil, Number, String))
import Text.Megaparsec (ErrorItem (EndOfInput, Label, Tokens), MonadParsec (eof, withRecovery), ParseError (FancyError, TrivialError), ParseErrorBundle (ParseErrorBundle), ParsecT, PosState (pstateSourcePos), SourcePos (sourceColumn, sourceLine), Stream (take1_), between, choice, getInput, many, parse, sepBy, sepBy1, skipManyTill, sourcePosPretty, token, unPos, (<|>))
import Token (LoxTok (LoxTok))
import TokenType (TokenType (..))

type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void [LoxTok] a

type ParserResult = Either (ParseErrorBundle [LoxTok] Void) Expr

-- case Text.Megaparsec.parse (withRecovery (\err -> skipManyTill (expression) (Literal Expr.Nil <$ semicolon)) (expression <* eof) `sepBy` semicolon) "" src of
-- parse' src = case Text.Megaparsec.parse ((many expression) `sepBy` semicolon) "" src of

parse' :: [LoxTok] -> IO ()
parse' src = putStrLn $ Parser.parse src

parse :: [LoxTok] -> String
parse src = case Text.Megaparsec.parse (expression <* eof) "" src of
  Right res -> show res
  Left (ParseErrorBundle x pos_state) ->
    let errors = toList x
     in unlines (map (f pos_state) errors)
  where
    f :: PosState [LoxTok] -> ParseError [LoxTok] Void -> String
    f pos_state (TrivialError offset unexpected_tokens expected_tokens) =
      let line = show . unPos $ sourceLine (pstateSourcePos pos_state)
          column = show . (+ offset) . unPos $ sourceColumn (pstateSourcePos pos_state)
          unexpected = case unexpected_tokens of
            Just (Tokens t) -> "unexpected tokens: " <> mconcat (map show (toList t))
            Just (Label l) -> "unexpected label: " <> toList l
            Just EndOfInput -> "unexpected: end of input"
            Nothing -> "no unexpected token"
          expected_tokens' = Set.toList expected_tokens
          f' x = case x of
            Tokens t -> mconcat (map show (toList t))
            Label l -> toList l
            EndOfInput -> "end of input"
          expected = "expected tokens: " <> intercalate ", " (map f' expected_tokens')
       in unlines
            [ "Error at line " <> line <> ", column " <> column,
              unexpected,
              expected
            ]
    f _ (FancyError _ _) = "not handled [fancy error]"

expression :: Parser Expr
expression = equality

equality :: Parser Expr
equality = do
  expr <- comparison

  go expr
  where
    parseOp expr = do
      op <- choice [bangEqual, equalEqual]
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
      op <- choice [greater, greaterEqual, less, lessEqual]
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
      op <- choice [plus, minus]
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
      op <- choice [slash, star]
      right <- term
      go (Binary expr op right)

    go expr = do
      parseOp expr <|> return expr

unary :: Parser Expr
unary = do
  go <|> primary
  where
    go = do
      op <- choice [bang, minus]
      right <- unary
      return $ Unary op right

primary :: Parser Expr
primary = do
  parseBool <|> parseNumOrString <|> parseGrouping
  where
    parseBool = choice [Literal (Boolean True) <$ true, Literal (Boolean False) <$ false, Literal Expr.Nil <$ nil]
    parseNumOrString = choice [Literal . Expr.String <$> string, Literal . Expr.Number <$> number]
    parseGrouping = between leftParen rightParen expression

identifier :: Parser String
identifier = token getIdent (Set.singleton (Label (fromJust $ nonEmpty "identifier")))
  where
    getIdent (LoxTok (Identifier ident) _ _) = Just ident
    getIdent _ = Nothing

number :: Parser Double
number = token getNumber (Set.singleton (Label $ nonEmpty' "identifier"))
  where
    getNumber (LoxTok (TokenType.Number n) _ _) = Just n
    getNumber _ = Nothing

string :: Parser String
string = token getString (Set.singleton (Label $ nonEmpty' "string"))
  where
    getString (LoxTok (TokenType.StringLit s) _ _) = Just s
    getString _ = Nothing

bang :: Parser LoxTok
bang = token getBang (Set.singleton (Label $ nonEmpty' "!"))
  where
    getBang (LoxTok Bang _ _) = Just (LoxTok Bang Nothing 0)
    getBang _ = Nothing

minus :: Parser LoxTok
minus = token getMinus (Set.singleton (Label $ nonEmpty' "-"))
  where
    getMinus (LoxTok Minus _ _) = Just (LoxTok Minus Nothing 0)
    getMinus _ = Nothing

bangEqual :: Parser LoxTok
bangEqual = token getBangEqual (Set.singleton (Label $ nonEmpty' "!="))
  where
    getBangEqual (LoxTok BangEqual _ _) = Just (LoxTok BangEqual Nothing 0)
    getBangEqual _ = Nothing

slash :: Parser LoxTok
slash = token getSlash (Set.singleton (Label $ nonEmpty' "/"))
  where
    getSlash (LoxTok Slash _ _) = Just (LoxTok Slash Nothing 0)
    getSlash _ = Nothing

star :: Parser LoxTok
star = token getStar (Set.singleton (Label $ nonEmpty' "*"))
  where
    getStar (LoxTok Star _ _) = Just (LoxTok Star Nothing 0)
    getStar _ = Nothing

plus :: Parser LoxTok
plus = token getPlus (Set.singleton (Label $ nonEmpty' "+"))
  where
    getPlus (LoxTok Plus _ _) = Just (LoxTok Plus Nothing 0)
    getPlus _ = Nothing

greater :: Parser LoxTok
greater = token getGreater (Set.singleton (Label $ nonEmpty' ">"))
  where
    getGreater (LoxTok Greater _ _) = Just (LoxTok Greater Nothing 0)
    getGreater _ = Nothing

less :: Parser LoxTok
less = token getLess (Set.singleton (Label $ nonEmpty' "<"))
  where
    getLess (LoxTok Less _ _) = Just (LoxTok Less Nothing 0)
    getLess _ = Nothing

greaterEqual :: Parser LoxTok
greaterEqual = token getGreaterEqual (Set.singleton (Label $ nonEmpty' ">="))
  where
    getGreaterEqual (LoxTok GreaterEqual _ _) = Just (LoxTok GreaterEqual Nothing 0)
    getGreaterEqual _ = Nothing

lessEqual :: Parser LoxTok
lessEqual = token getLessEqual (Set.singleton (Label $ nonEmpty' "<="))
  where
    getLessEqual (LoxTok LessEqual _ _) = Just (LoxTok LessEqual Nothing 0)
    getLessEqual _ = Nothing

equalEqual :: Parser LoxTok
equalEqual = token getEqualEqual (Set.singleton (Label $ nonEmpty' "=="))
  where
    getEqualEqual (LoxTok EqualEqual _ _) = Just (LoxTok EqualEqual Nothing 0)
    getEqualEqual _ = Nothing

true :: Parser LoxTok
true = token getTrue (Set.singleton (Label $ nonEmpty' "true"))
  where
    getTrue (LoxTok True_ _ _) = Just (LoxTok True_ Nothing 0)
    getTrue _ = Nothing

false :: Parser LoxTok
false = token getFalse (Set.singleton (Label $ nonEmpty' "false"))
  where
    getFalse (LoxTok False_ _ _) = Just (LoxTok False_ Nothing 0)
    getFalse _ = Nothing

rightParen :: Parser LoxTok
rightParen = token getRightParen (Set.singleton (Label $ nonEmpty' ")"))
  where
    getRightParen (LoxTok RightParen _ _) = Just (LoxTok RightParen Nothing 0)
    getRightParen _ = Nothing

leftParen :: Parser LoxTok
leftParen = token getLeftParen (Set.singleton (Label $ nonEmpty' "("))
  where
    getLeftParen (LoxTok LeftParen _ _) = Just (LoxTok LeftParen Nothing 0)
    getLeftParen _ = Nothing

nil :: Parser LoxTok
nil = token getNil (Set.singleton (Label $ nonEmpty' "nil"))
  where
    getNil (LoxTok TokenType.Nil _ _) = Just (LoxTok TokenType.Nil Nothing 0)
    getNil _ = Nothing

semicolon :: Parser LoxTok
semicolon = token getSemicolon (Set.singleton (Label $ nonEmpty' ";"))
  where
    getSemicolon (LoxTok TokenType.Semicolon _ _) = Just (LoxTok TokenType.Semicolon Nothing 0)
    getSemicolon _ = Nothing

nonEmpty' :: String -> NonEmpty Char
nonEmpty' s = fromJust $ nonEmpty s