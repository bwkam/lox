{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Interpreter (eval, eval') where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity), IdentityT (runIdentityT))
import Control.Monad.State (MonadState (get, put, state), MonadTrans (lift), StateT (runStateT), modify)
import Data.Foldable (for_)
import Expr (Expr (Binary, Expression, Grouping, Literal, Print, Unary, Var, Variable), LiteralValue (..))
import qualified Parser
import Text.Megaparsec (SourcePos (SourcePos), errorBundlePretty, unPos)
import Token (LoxTok (LoxTok, tokenType), WithPos (WithPos))
import TokenType (TokenType (Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Identifier, Less, LessEqual, Minus, Plus, Slash, Star))
import Prelude hiding (error)

data Value = Nil | Boolean Bool | Number Double | String String deriving (Show)

type Name = String

type Env = [(Name, Value)]

type Result = ExceptT (WithPos LoxTok, String) (StateT Env Identity) Value

eval' :: String -> IO ()
eval' src = case Parser.parse src of
  Right es -> mconcat $ map (runIdentity . go) es
  Left es -> case es of
    (Nothing, sErrors) -> for_ sErrors print
    (pErrors, Nothing) -> case pErrors of
      Just e -> putStrLn $ errorBundlePretty e
    (Just _, Just _) -> putStrLn "no way!!!!"
  where
    go e = do
      (exprs, _) <- runStateT (runExceptT (eval e)) []
      case exprs of
        Right v -> pure $ print v
        Left (WithPos (SourcePos _ l _) _ _ _, error) -> pure $ putStrLn $ error <> " " <> "at line " <> (show . unPos) l

eval :: Expr -> Result
eval (Literal inner) = case inner of
  Expr.Number n -> pure $ Interpreter.Number n
  Expr.Boolean b -> pure $ Interpreter.Boolean b
  Expr.Nil -> pure Interpreter.Nil
  Expr.String s -> pure $ Interpreter.String s
eval (Grouping expr) = eval expr
eval (Unary wp@(WithPos _ _ _ (LoxTok tt _)) expr) = case (tt, expr) of
  (Minus, Literal (Expr.Number n)) -> pure $ Interpreter.Number $ -n
  (Bang, e) -> pure $ Interpreter.Boolean $ not $ isTruthy e
  (_, _) -> throwError (wp, "wrong operands for " ++ show tt)
eval (Binary e1 wp@(WithPos _ _ _ t) e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, tokenType t, v2) of
    (Interpreter.Number v1', Minus, Interpreter.Number v2') -> pure $ Interpreter.Number $ v1' - v2'
    (Interpreter.Number v1', Slash, Interpreter.Number v2') -> pure $ Interpreter.Number $ v1' / v2'
    (Interpreter.Number v1', Star, Interpreter.Number v2') -> pure $ Interpreter.Number $ v1' * v2'
    (Interpreter.Number v1', Plus, Interpreter.Number v2') -> pure $ Interpreter.Number $ v1' + v2'
    (Interpreter.String v1', Plus, Interpreter.String v2') -> pure $ Interpreter.String $ v1' ++ v2'
    (Interpreter.Number v1', Greater, Interpreter.Number v2') -> pure $ Interpreter.Boolean $ v1' > v2'
    (Interpreter.Number v1', GreaterEqual, Interpreter.Number v2') -> pure $ Interpreter.Boolean $ v1' >= v2'
    (Interpreter.Number v1', Less, Interpreter.Number v2') -> pure $ Interpreter.Boolean $ v1' < v2'
    (Interpreter.Number v1', LessEqual, Interpreter.Number v2') -> pure $ Interpreter.Boolean $ v1' <= v2'
    (_, BangEqual, _) -> pure $ Interpreter.Boolean $ not $ isEqual e1 e2
    (_, EqualEqual, _) -> pure $ Interpreter.Boolean $ isEqual e1 e2
    _ -> throwError (wp, "wrong operands for " ++ show (tokenType t))
eval (Expression e) = eval e
eval (Expr.Print e) = do
  e' <- eval e
  case e' of
    (Interpreter.Number n) -> pure $ Interpreter.String $ show n
    Interpreter.Nil -> pure $ Interpreter.String "nil"
    (Interpreter.String s) -> pure $ Interpreter.String s
    (Interpreter.Boolean b) -> pure $ Interpreter.String $ show b
eval (Expr.Var (WithPos _ _ _ (LoxTok (Identifier name) _)) e) = do
  case e of
    Just e' -> do
      v <- eval e'
      let newVar = (name, v)
      -- _ <- state (\env -> (v, newVar : env))
      modify (newVar :)
      pure v
    Nothing -> do
      let newVar = (name, Interpreter.Nil)
      _ <- state (\env -> (Interpreter.Nil, newVar : env))
      pure Interpreter.Nil
eval (Expr.Variable wp@(WithPos _ _ _ (LoxTok (Identifier name) _))) = do
  env <- get
  case lookup name env of
    Just v -> pure v
    Nothing -> throwError (wp, "undefined variable: " <> name <> show env)
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
