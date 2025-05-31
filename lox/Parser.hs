module Parser where

import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void
import Error.Diagnose.Report
import Expr (Expr (Binary, Literal, Unary), LiteralValue (Boolean, Nil, Number, String))
import Scanner (ScannerResult, scan)
import Text.Megaparsec (ErrorItem (Label), MonadParsec (eof, lookAhead, withRecovery), ParseErrorBundle, ParsecT, anySingle, between, choice, errorBundlePretty, many, parse, registerParseError, skipManyTill, token, (<|>))
import Token (LoxTok (LoxTok), LoxTokStream, WithPos (WithPos))
import TokenType (TokenType (..))

type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void LoxTokStream a

type ParserResult = Either (Maybe (ParseErrorBundle LoxTokStream Void), Maybe (ParseErrorBundle Text Void)) Expr

parse' :: String -> IO ()
parse' src = case Scanner.scan src of
  Right lexemes -> case Text.Megaparsec.parse (expression <* eof) "" lexemes of
    Right tokens -> print tokens
    Left pErrors -> putStrLn $ errorBundlePretty pErrors
  Left sErrors -> putStrLn $ errorBundlePretty sErrors

parse :: String -> ParserResult
parse src = case Scanner.scan src of
  Right lexemes -> case Text.Megaparsec.parse (expression <* eof) "" lexemes of
    Right tokens -> Right tokens
    Left pErrors -> Left (Just pErrors, Nothing)
  Left sErrors -> Left (Nothing, Just sErrors)

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
    getIdent wp@(WithPos _ _ _ (LoxTok (Identifier ident) _)) = Just ident
    getIdent _ = Nothing

number :: Parser Double
number = token getNumber (Set.singleton (Label $ nonEmpty' "identifier"))
  where
    getNumber (WithPos _ _ _ (LoxTok (TokenType.Number n) _)) = Just n
    getNumber _ = Nothing

string :: Parser String
string = token getString (Set.singleton (Label $ nonEmpty' "string"))
  where
    getString (WithPos _ _ _ (LoxTok (StringLit str) _)) = Just str
    getString _ = Nothing

bang :: Parser (WithPos LoxTok)
bang = token getBang (Set.singleton (Label $ nonEmpty' "!"))
  where
    getBang wp@(WithPos _ _ _ (LoxTok Bang _)) = Just wp
    getBang _ = Nothing

minus :: Parser (WithPos LoxTok)
minus = token getMinus (Set.singleton (Label $ nonEmpty' "-"))
  where
    getMinus wp@(WithPos _ _ _ (LoxTok Minus _)) = Just wp
    getMinus _ = Nothing

bangEqual :: Parser (WithPos LoxTok)
bangEqual = token getBangEqual (Set.singleton (Label $ nonEmpty' "!="))
  where
    getBangEqual wp@(WithPos _ _ _ (LoxTok BangEqual _)) = Just wp
    getBangEqual _ = Nothing

slash :: Parser (WithPos LoxTok)
slash = token getSlash (Set.singleton (Label $ nonEmpty' "/"))
  where
    getSlash wp@(WithPos _ _ _ (LoxTok Slash _)) = Just wp
    getSlash _ = Nothing

star :: Parser (WithPos LoxTok)
star = token getStar (Set.singleton (Label $ nonEmpty' "*"))
  where
    getStar wp@(WithPos _ _ _ (LoxTok Star _)) = Just wp
    getStar _ = Nothing

plus :: Parser (WithPos LoxTok)
plus = token getPlus (Set.singleton (Label $ nonEmpty' "+"))
  where
    getPlus wp@(WithPos _ _ _ (LoxTok Plus _)) = Just wp
    getPlus _ = Nothing

greater :: Parser (WithPos LoxTok)
greater = token getGreater (Set.singleton (Label $ nonEmpty' ">"))
  where
    getGreater wp@(WithPos _ _ _ (LoxTok Greater _)) = Just wp
    getGreater _ = Nothing

less :: Parser (WithPos LoxTok)
less = token getLess (Set.singleton (Label $ nonEmpty' "<"))
  where
    getLess wp@(WithPos _ _ _ (LoxTok Less _)) = Just wp
    getLess _ = Nothing

greaterEqual :: Parser (WithPos LoxTok)
greaterEqual = token getGreaterEqual (Set.singleton (Label $ nonEmpty' ">="))
  where
    getGreaterEqual wp@(WithPos _ _ _ (LoxTok GreaterEqual _)) = Just wp
    getGreaterEqual _ = Nothing

lessEqual :: Parser (WithPos LoxTok)
lessEqual = token getLessEqual (Set.singleton (Label $ nonEmpty' "<="))
  where
    getLessEqual wp@(WithPos _ _ _ (LoxTok LessEqual _)) = Just wp
    getLessEqual _ = Nothing

equalEqual :: Parser (WithPos LoxTok)
equalEqual = token getEqualEqual (Set.singleton (Label $ nonEmpty' "=="))
  where
    getEqualEqual wp@(WithPos _ _ _ (LoxTok EqualEqual _)) = Just wp
    getEqualEqual _ = Nothing

true :: Parser (WithPos LoxTok)
true = token getTrue (Set.singleton (Label $ nonEmpty' "true"))
  where
    getTrue wp@(WithPos _ _ _ (LoxTok True_ _)) = Just wp
    getTrue _ = Nothing

false :: Parser (WithPos LoxTok)
false = token getFalse (Set.singleton (Label $ nonEmpty' "false"))
  where
    getFalse wp@(WithPos _ _ _ (LoxTok False_ _)) = Just wp
    getFalse _ = Nothing

rightParen :: Parser (WithPos LoxTok)
rightParen = token getRightParen (Set.singleton (Label $ nonEmpty' ")"))
  where
    getRightParen wp@(WithPos _ _ _ (LoxTok RightParen _)) = Just wp
    getRightParen _ = Nothing

leftParen :: Parser (WithPos LoxTok)
leftParen = token getLeftParen (Set.singleton (Label $ nonEmpty' "("))
  where
    getLeftParen wp@(WithPos _ _ _ (LoxTok LeftParen _)) = Just wp
    getLeftParen _ = Nothing

nil :: Parser (WithPos LoxTok)
nil = token getNil (Set.singleton (Label $ nonEmpty' "nil"))
  where
    getNil wp@(WithPos _ _ _ (LoxTok TokenType.Nil _)) = Just wp
    getNil _ = Nothing

semicolon :: Parser (WithPos LoxTok)
semicolon = token getSemicolon (Set.singleton (Label $ nonEmpty' ";"))
  where
    getSemicolon wp@(WithPos _ _ _ (LoxTok Semicolon _)) = Just wp
    getSemicolon _ = Nothing

nonEmpty' :: String -> NonEmpty Char
nonEmpty' s = fromJust $ nonEmpty s