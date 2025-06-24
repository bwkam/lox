module Parser where

import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void
import Expr (Expr (And, Assign, Binary, Block, Call, Expression, Function, If, Literal, Or, Print, Return, Unary, Var, Variable, While), LiteralValue (Boolean, Nil, Number, String))
import Scanner (scan)
import Text.Megaparsec (ErrorItem (Label), MonadParsec (eof, lookAhead, try, withRecovery), ParseErrorBundle, ParsecT, anySingle, between, choice, errorBundlePretty, many, optional, parse, registerParseError, satisfy, skipManyTill, token, (<|>))
import Token (LoxTok (LoxTok), LoxTokStream, WithPos (WithPos))
import TokenType (TokenType (..))

type Parsec e s a = ParsecT e s Identity a

type Parser a = Parsec Void LoxTokStream a

type ParserResult = Either (Maybe (ParseErrorBundle LoxTokStream Void), Maybe (ParseErrorBundle Text Void)) [Expr]

parse' :: String -> IO ()
parse' src = case Scanner.scan src of
  Right lexemes -> case Text.Megaparsec.parse program "" lexemes of
    Right tokens -> print tokens
    Left pErrors -> putStrLn $ errorBundlePretty pErrors
  Left sErrors -> putStrLn $ errorBundlePretty sErrors

parse :: String -> ParserResult
parse src = case Scanner.scan src of
  Right lexemes -> case Text.Megaparsec.parse program "" lexemes of
    Right tokens -> Right tokens
    Left pErrors -> Left (Just pErrors, Nothing)
  Left sErrors -> Left (Nothing, Just sErrors)

addRecovery :: Parser Expr -> Parser Expr
addRecovery =
  withRecovery
    ( \e -> do
        registerParseError e
        skipManyTill anySingle (Literal Expr.Nil <$ semicolon)
    )

program :: Parser [Expr]
program = many (addRecovery declaration) <* eof

declaration :: Parser Expr
declaration = choice [funDecl, varDecl, statement]

funDecl :: Parser Expr
funDecl = do
  _ <- fun
  function

function :: Parser Expr
function = do
  name <- identifier
  _ <- leftParen
  p <- parameters
  _ <- rightParen
  Expr.Function name p <$> block

varDecl :: Parser Expr
varDecl = do
  _ <- var
  ident <- identifier
  e <- optional (equal_ *> expression)
  _ <- semicolon

  case e of
    Just e' -> pure $ Expr.Var ident (Just e')
    Nothing -> pure $ Expr.Var ident Nothing

statement :: Parser Expr
statement = choice [printStmt, exprStmt, forStmt, whileStmt, block, ifStmt, retStmt]

retStmt :: Parser Expr
retStmt = do
  _ <- return_
  e <- optional expression
  _ <- semicolon
  case e of
    Just e' -> pure $ Expr.Return e'
    Nothing -> pure $ Expr.Return (Literal Expr.Nil)

whileStmt :: Parser Expr
whileStmt = do
  while >> leftParen
  c <- expression
  _ <- rightParen
  Expr.While c <$> statement

forStmt :: Parser Expr
forStmt = do
  for >> leftParen
  init <- pInit
  cond <- pCond
  inc <- pInc

  body' <-
    statement
      >>= ( \b -> case inc of
              Just i -> pure $ Expr.Block [b, i]
              Nothing -> pure b
          )

  cond' <-
    case cond of
      Just c -> pure c
      Nothing -> pure $ Expr.Literal (Boolean True)

  let whileExpr = case init of
        Just i -> Expr.Block [i, Expr.While cond' body']
        Nothing -> Expr.While cond' body'

  pure whileExpr
  where
    pInit =
      do
        semicolon >> pure Nothing
        <|> Just
        <$> varDecl
          <|> (Just <$> exprStmt)
    pCond =
      do
        semicolon >> pure Nothing
        <|> (expression >>= (\e -> semicolon >> pure (Just e)))
    pInc =
      do
        rightParen >> pure Nothing
        <|> do
          e <- expression
          _ <- rightParen
          pure (Just e)

block :: Parser Expr
block = do
  _ <- leftBrace
  expr <- many declaration
  _ <- rightBrace
  pure $ Expr.Block expr

logicOr :: Parser Expr
logicOr = do
  expr <- logicAnd

  go expr
  where
    parseOp expr = do
      _ <- or_
      right <- logicAnd
      go (Expr.Or expr right)

    go expr = do
      parseOp expr <|> return expr

logicAnd :: Parser Expr
logicAnd = do
  expr <- equality

  go expr
  where
    parseOp expr = do
      _ <- and_
      right <- equality
      go (Expr.And expr right)

    go expr = do
      parseOp expr <|> return expr

exprStmt :: Parser Expr
exprStmt = Expr.Expression <$> expression <* semicolon

ifStmt :: Parser Expr
ifStmt = do
  if_ >> leftParen
  e <- expression
  _ <- rightParen
  s <- statement
  e' <- optional (else_ >> statement)
  pure $ Expr.If e s e'

printStmt :: Parser Expr
printStmt = Expr.Print <$> (print_ *> expression <* semicolon)

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment = choice [try f, try logicOr]
  where
    f = do
      expr <- equality
      _ <- equal_

      go expr
      where
        parseOp expr = do
          right <- assignment

          go (Assign expr right)

        go expr = do
          parseOp expr <|> return expr

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
  go <|> call
  where
    go = do
      op <- choice [bang, minus]
      Unary op <$> unary

call :: Parser Expr
call = do
  expr <- primary
  go expr
  where
    parseCall expr = do
      l <- leftParen
      es <- arguments
      _ <- rightParen

      go (Expr.Call expr l es)

    go expr = parseCall expr <|> return expr

arguments :: Parser [Expr]
arguments = do
  expr <- expression
  go [expr]
  where
    parseMore es = do
      _ <- comma
      r <- expression
      go $ es <> [r]

    go es = parseMore es <|> return es

parameters :: Parser [WithPos LoxTok]
parameters = do
  name <- identifier
  go [name]
  where
    parseMore es = do
      _ <- comma
      r <- identifier
      go $ es <> [r]

    go es = parseMore es <|> return es

primary :: Parser Expr
primary = do
  parseBool <|> parseNumOrString <|> parseGrouping <|> parseIdent
  where
    parseBool = choice [Literal (Boolean True) <$ true, Literal (Boolean False) <$ false, Literal Expr.Nil <$ nil]
    parseNumOrString = choice [Literal . Expr.String <$> string, Literal . Expr.Number <$> number]
    parseGrouping = between leftParen rightParen expression
    parseIdent = Variable <$> identifier

identifier :: Parser (WithPos LoxTok)
identifier = token getIdent (Set.singleton (Label (fromJust $ nonEmpty "identifier")))
  where
    getIdent wp@(WithPos _ _ _ (LoxTok (Identifier _) _)) = Just wp
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

equal_ :: Parser (WithPos LoxTok)
equal_ = token getEqual (Set.singleton (Label $ nonEmpty' "="))
  where
    getEqual wp@(WithPos _ _ _ (LoxTok Equal _)) = Just wp
    getEqual _ = Nothing

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

rightBrace :: Parser (WithPos LoxTok)
rightBrace = token getRightBrace (Set.singleton (Label $ nonEmpty' "}"))
  where
    getRightBrace wp@(WithPos _ _ _ (LoxTok RightBrace _)) = Just wp
    getRightBrace _ = Nothing

leftBrace :: Parser (WithPos LoxTok)
leftBrace = token getLeftBrace (Set.singleton (Label $ nonEmpty' "{"))
  where
    getLeftBrace wp@(WithPos _ _ _ (LoxTok LeftBrace _)) = Just wp
    getLeftBrace _ = Nothing

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

print_ :: Parser (WithPos LoxTok)
print_ = token getPrint (Set.singleton (Label $ nonEmpty' "print"))
  where
    getPrint wp@(WithPos _ _ _ (LoxTok TokenType.Print _)) = Just wp
    getPrint _ = Nothing

if_ :: Parser (WithPos LoxTok)
if_ = token getIf (Set.singleton (Label $ nonEmpty' "if"))
  where
    getIf wp@(WithPos _ _ _ (LoxTok TokenType.If _)) = Just wp
    getIf _ = Nothing

else_ :: Parser (WithPos LoxTok)
else_ = token getElse (Set.singleton (Label $ nonEmpty' "else"))
  where
    getElse wp@(WithPos _ _ _ (LoxTok TokenType.Else _)) = Just wp
    getElse _ = Nothing

or_ :: Parser (WithPos LoxTok)
or_ = token getOr (Set.singleton (Label $ nonEmpty' "or"))
  where
    getOr wp@(WithPos _ _ _ (LoxTok TokenType.Or _)) = Just wp
    getOr _ = Nothing

and_ :: Parser (WithPos LoxTok)
and_ = token getAnd (Set.singleton (Label $ nonEmpty' "and"))
  where
    getAnd wp@(WithPos _ _ _ (LoxTok TokenType.And _)) = Just wp
    getAnd _ = Nothing

var :: Parser (WithPos LoxTok)
var = token getVar (Set.singleton (Label $ nonEmpty' "var"))
  where
    getVar wp@(WithPos _ _ _ (LoxTok TokenType.Var _)) = Just wp
    getVar _ = Nothing

fun :: Parser (WithPos LoxTok)
fun = token getFun (Set.singleton (Label $ nonEmpty' "fun"))
  where
    getFun wp@(WithPos _ _ _ (LoxTok TokenType.Fun _)) = Just wp
    getFun _ = Nothing

while :: Parser (WithPos LoxTok)
while = token getWhile (Set.singleton (Label $ nonEmpty' "while"))
  where
    getWhile wp@(WithPos _ _ _ (LoxTok TokenType.While _)) = Just wp
    getWhile _ = Nothing

for :: Parser (WithPos LoxTok)
for = token getFor (Set.singleton (Label $ nonEmpty' "for"))
  where
    getFor wp@(WithPos _ _ _ (LoxTok TokenType.For _)) = Just wp
    getFor _ = Nothing

return_ :: Parser (WithPos LoxTok)
return_ = token getReturn (Set.singleton (Label $ nonEmpty' "return"))
  where
    getReturn wp@(WithPos _ _ _ (LoxTok TokenType.Return _)) = Just wp
    getReturn _ = Nothing

comma :: Parser (WithPos LoxTok)
comma = token getComma (Set.singleton (Label $ nonEmpty' "comma"))
  where
    getComma wp@(WithPos _ _ _ (LoxTok TokenType.Comma _)) = Just wp
    getComma _ = Nothing

nonEmpty' :: String -> NonEmpty Char
nonEmpty' s = fromJust $ nonEmpty s
