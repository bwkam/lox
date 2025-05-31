module Interpreter (eval, eval') where

import Expr (Expr (Binary, Grouping, Literal, Unary), LiteralValue (..))
import qualified Parser
import Text.Megaparsec (SourcePos (SourcePos), errorBundlePretty, unPos)
import Token (LoxTok (LoxTok, tokenType), WithPos (WithPos))
import TokenType (TokenType (Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Plus, Slash, Star))

data Value = Nil | Boolean Bool | Number Double | String String deriving (Show)

type RuntimeResult = Either (WithPos LoxTok, String) Value

-- Expr -> [Expr]
eval' :: String -> String
eval' src = case Parser.parse src of
  Right es -> case eval es of
    Right e -> show e
    Left (WithPos (SourcePos _ l _) _ _ _, e) -> show e <> "\n" >> ("[line " <> (show . unPos) l <> "]")
  Left es -> case es of
    (Nothing, sErrors) -> maybe "" errorBundlePretty sErrors
    (pErrors, Nothing) -> case pErrors of
      Just e -> errorBundlePretty e
    (Just _, Just _) -> "no way!!!!"

eval :: Expr -> RuntimeResult
eval (Literal inner) = case inner of
  Expr.Number n -> Right $ Interpreter.Number n
  Expr.Boolean b -> Right $ Interpreter.Boolean b
  Expr.Nil -> Right Interpreter.Nil
  Expr.String s -> Right $ Interpreter.String s
eval (Grouping expr) = eval expr
eval (Unary wp@(WithPos _ _ _ (LoxTok tt _)) expr) = case (tt, expr) of
  (Minus, Literal (Expr.Number n)) -> Right $ Interpreter.Number $ -n
  (Bang, e) -> Right $ Interpreter.Boolean $ not $ isTruthy e
  (_, _) -> Left (wp, "wrong operands " ++ show tt)
eval (Binary e1 wp@(WithPos _ _ _ t) e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, tokenType t, v2) of
    (Interpreter.Number v1', Minus, Interpreter.Number v2') -> Right $ Interpreter.Number $ v1' - v2'
    (Interpreter.Number v1', Slash, Interpreter.Number v2') -> Right $ Interpreter.Number $ v1' / v2'
    (Interpreter.Number v1', Star, Interpreter.Number v2') -> Right $ Interpreter.Number $ v1' * v2'
    (Interpreter.Number v1', Plus, Interpreter.Number v2') -> Right $ Interpreter.Number $ v1' + v2'
    (Interpreter.String v1', Plus, Interpreter.String v2') -> Right $ Interpreter.String $ v1' ++ v2'
    (Interpreter.Number v1', Greater, Interpreter.Number v2') -> Right $ Interpreter.Boolean $ v1' > v2'
    (Interpreter.Number v1', GreaterEqual, Interpreter.Number v2') -> Right $ Interpreter.Boolean $ v1' >= v2'
    (Interpreter.Number v1', Less, Interpreter.Number v2') -> Right $ Interpreter.Boolean $ v1' < v2'
    (Interpreter.Number v1', LessEqual, Interpreter.Number v2') -> Right $ Interpreter.Boolean $ v1' <= v2'
    (_, BangEqual, _) -> Right $ Interpreter.Boolean $ not $ isEqual e1 e2
    (_, EqualEqual, _) -> Right $ Interpreter.Boolean $ isEqual e1 e2
    _ -> Left (wp, "wrong operands " ++ show (tokenType t))
eval _ = undefined

isEqual :: Expr -> Expr -> Bool
isEqual (Literal Expr.Nil) (Literal Expr.Nil) = True
isEqual (Literal Expr.Nil) _ = False
isEqual (Literal (Expr.Boolean b1)) (Literal (Expr.Boolean b2)) = b1 == b2
isEqual (Literal (Expr.String s1)) (Literal (Expr.String s2)) = s1 == s2
isEqual (Literal (Expr.Number n1)) (Literal (Expr.Number n2)) = n1 == n2
isEqual _ _ = False

isTruthy :: Expr -> Bool
isTruthy (Literal Expr.Nil) = False
isTruthy (Literal (Expr.Boolean False)) = False
isTruthy _ = True
