module Parser where

import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Void
import Error.Diagnose.Report
import Expr (Expr (Binary, Literal, Unary), LiteralValue (Boolean, Nil, Number, String))
import Text.Megaparsec (ErrorItem (Label), MonadParsec (eof, lookAhead, withRecovery), ParseErrorBundle, ParsecT, anySingle, between, choice, errorBundlePretty, many, parse, registerParseError, skipManyTill, token, (<|>))
import Token (LoxTok (LoxTok), LoxTokStream, WithPos (WithPos))
import TokenType (TokenType (..))

type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void LoxTokStream a

type ParserResult = Either (ParseErrorBundle [LoxTok] Void) Expr

parse' :: LoxTokStream -> IO ()
parse' src = case Text.Megaparsec.parse go "" src of
  Right res -> print res
  Left e -> putStrLn $ errorBundlePretty e
  where
    go = many (expression' <* semicolon') <* eof
    expression' =
      withRecovery
        ( \e -> do
            registerParseError e
            skipManyTill anySingle (Literal Expr.Nil <$ lookAhead semicolon)
        )
        expression
    semicolon' =
      withRecovery
        ( \e -> do
            registerParseError e
            skipManyTill anySingle semicolon
        )
        semicolon

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
    getIdent (WithPos _ _ _ (LoxTok (Identifier ident) _)) = Just ident
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

bang :: Parser LoxTok
bang = token getBang (Set.singleton (Label $ nonEmpty' "!"))
  where
    getBang (WithPos _ _ _ (LoxTok Bang _)) = Just (LoxTok Bang Nothing)
    getBang _ = Nothing

minus :: Parser LoxTok
minus = token getMinus (Set.singleton (Label $ nonEmpty' "-"))
  where
    getMinus (WithPos _ _ _ (LoxTok Minus _)) = Just (LoxTok Minus Nothing)
    getMinus _ = Nothing

bangEqual :: Parser LoxTok
bangEqual = token getBangEqual (Set.singleton (Label $ nonEmpty' "!="))
  where
    getBangEqual (WithPos _ _ _ (LoxTok BangEqual _)) = Just (LoxTok BangEqual Nothing)
    getBangEqual _ = Nothing

slash :: Parser LoxTok
slash = token getSlash (Set.singleton (Label $ nonEmpty' "/"))
  where
    getSlash (WithPos _ _ _ (LoxTok Slash _)) = Just (LoxTok Slash Nothing)
    getSlash _ = Nothing

star :: Parser LoxTok
star = token getStar (Set.singleton (Label $ nonEmpty' "*"))
  where
    getStar (WithPos _ _ _ (LoxTok Star _)) = Just (LoxTok Star Nothing)
    getStar _ = Nothing

plus :: Parser LoxTok
plus = token getPlus (Set.singleton (Label $ nonEmpty' "+"))
  where
    getPlus (WithPos _ _ _ (LoxTok Plus _)) = Just (LoxTok Plus Nothing)
    getPlus _ = Nothing

greater :: Parser LoxTok
greater = token getGreater (Set.singleton (Label $ nonEmpty' ">"))
  where
    getGreater (WithPos _ _ _ (LoxTok Greater _)) = Just (LoxTok Greater Nothing)
    getGreater _ = Nothing

less :: Parser LoxTok
less = token getLess (Set.singleton (Label $ nonEmpty' "<"))
  where
    getLess (WithPos _ _ _ (LoxTok Less _)) = Just (LoxTok Less Nothing)
    getLess _ = Nothing

greaterEqual :: Parser LoxTok
greaterEqual = token getGreaterEqual (Set.singleton (Label $ nonEmpty' ">="))
  where
    getGreaterEqual (WithPos _ _ _ (LoxTok GreaterEqual _)) = Just (LoxTok GreaterEqual Nothing)
    getGreaterEqual _ = Nothing

lessEqual :: Parser LoxTok
lessEqual = token getLessEqual (Set.singleton (Label $ nonEmpty' "<="))
  where
    getLessEqual (WithPos _ _ _ (LoxTok LessEqual _)) = Just (LoxTok LessEqual Nothing)
    getLessEqual _ = Nothing

equalEqual :: Parser LoxTok
equalEqual = token getEqualEqual (Set.singleton (Label $ nonEmpty' "=="))
  where
    getEqualEqual (WithPos _ _ _ (LoxTok EqualEqual _)) = Just (LoxTok EqualEqual Nothing)
    getEqualEqual _ = Nothing

true :: Parser LoxTok
true = token getTrue (Set.singleton (Label $ nonEmpty' "true"))
  where
    getTrue (WithPos _ _ _ (LoxTok True_ _)) = Just (LoxTok True_ Nothing)
    getTrue _ = Nothing

false :: Parser LoxTok
false = token getFalse (Set.singleton (Label $ nonEmpty' "false"))
  where
    getFalse (WithPos _ _ _ (LoxTok False_ _)) = Just (LoxTok False_ Nothing)
    getFalse _ = Nothing

rightParen :: Parser LoxTok
rightParen = token getRightParen (Set.singleton (Label $ nonEmpty' ")"))
  where
    getRightParen (WithPos _ _ _ (LoxTok RightParen _)) = Just (LoxTok RightParen Nothing)
    getRightParen _ = Nothing

leftParen :: Parser LoxTok
leftParen = token getLeftParen (Set.singleton (Label $ nonEmpty' "("))
  where
    getLeftParen (WithPos _ _ _ (LoxTok LeftParen _)) = Just (LoxTok LeftParen Nothing)
    getLeftParen _ = Nothing

nil :: Parser LoxTok
nil = token getNil (Set.singleton (Label $ nonEmpty' "nil"))
  where
    getNil (WithPos _ _ _ (LoxTok TokenType.Nil _)) = Just (LoxTok TokenType.Nil Nothing)
    getNil _ = Nothing

semicolon :: Parser LoxTok
semicolon = token getSemicolon (Set.singleton (Label $ nonEmpty' ";"))
  where
    getSemicolon (WithPos _ _ _ (LoxTok Semicolon _)) = Just (LoxTok Semicolon Nothing)
    getSemicolon _ = Nothing

nonEmpty' :: String -> NonEmpty Char
nonEmpty' s = fromJust $ nonEmpty s