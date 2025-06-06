{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interpreter (eval, eval') where

import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (runStateT), modify)
import Data.Foldable (traverse_)
import Expr (Expr (Assign, Binary, Block, Expression, Grouping, If, Literal, Print, Unary, Var, Variable), LiteralValue (..))
import qualified Parser
import Text.Megaparsec (errorBundlePretty)
import Token (LoxTok (LoxTok, tokenType), WithPos (WithPos))
import TokenType (TokenType (Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Identifier, Less, LessEqual, Minus, Plus, Slash, Star))
import Prelude hiding (error)

data Value = Nil | Boolean Bool | Number Double | String String deriving (Show)

type Name = String

type Env = [(Name, Value)]

data Environment = Environment {values :: Env, parent :: Maybe Environment}

type Result = ExceptT (WithPos LoxTok, String) (StateT Environment IO)

eval' :: String -> IO ()
eval' src = do
  let action = case Parser.parse src of
        Right es -> mapM_ eval es `catchError` (liftIO . print)
        Left (Nothing, sErrors) -> traverse_ (liftIO . print) sErrors
        Left (Just e, Nothing) -> liftIO $ putStrLn $ errorBundlePretty e
        Left (Just _, Just _) -> liftIO $ putStrLn "no way!!!!"
  _ <- runStateT (runExceptT action) (Environment [] Nothing)
  pure ()

eval :: Expr -> Result Value
eval (Literal inner) = case inner of
  Expr.Number n -> pure $ Interpreter.Number n
  Expr.Boolean b -> pure $ Interpreter.Boolean b
  Expr.Nil -> pure Interpreter.Nil
  Expr.String s -> pure $ Interpreter.String s
eval (Grouping expr) = eval expr
eval (Unary wp@(WithPos _ _ _ (LoxTok tt _)) expr) = case (tt, expr) of
  (Minus, Literal (Expr.Number n)) -> pure $ Interpreter.Number $ -n
  (Bang, e) -> eval e >>= ((pure . Interpreter.Boolean) . not . isTruthy)
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
    (Interpreter.Number n) -> do
      liftIO $ print n
      pure $ Interpreter.Number n
    Interpreter.Nil -> do
      liftIO $ putStrLn "nil"
      pure Interpreter.Nil
    (Interpreter.String s) -> do
      liftIO $ print s
      pure $ Interpreter.String s
    (Interpreter.Boolean b) -> do
      liftIO $ print b
      pure $ Interpreter.Boolean b
eval (Expr.Var (WithPos _ _ _ (LoxTok (Identifier name) _)) e) = do
  case e of
    Just e' -> do
      v <- eval e'
      let newVar = (name, v)
      modify (\(Environment {..}) -> Environment {values = newVar : values, ..})
      pure v
    Nothing -> do
      let newVar = (name, Interpreter.Nil)
      modify (\(Environment {..}) -> Environment {values = newVar : values, ..})
      pure Interpreter.Nil
eval (Expr.Variable wp@(WithPos _ _ _ (LoxTok (Identifier name) _))) = do
  env <- get

  case searchValue name env of
    Just v -> pure v
    Nothing -> throwError (wp, "undefined variable: " <> name)
eval (Expr.Assign (Variable wp@(WithPos _ _ _ (LoxTok (Identifier name) _))) l) = do
  v' <- eval l
  env <- get

  case searchValue name env of
    Just _ -> do
      modify (\(Environment {..}) -> Environment {values = filter (\(n, _) -> n /= name) values, ..})
      modify (\Environment {..} -> Environment {values = (name, v') : values, ..})
      pure v'
    Nothing -> throwError (wp, "undefined variable: " <> name)
eval (Expr.Block es) = do
  oldEnv <- get
  modify (\(Environment {..}) -> Environment {parent = Just (Environment [] Nothing), ..})
  e <- last <$> traverse eval es
  put oldEnv
  pure e
eval (Expr.If c e e') = do
  c' <- eval c

  if isTruthy c'
    then eval e
    else case e' of
      Just e'' -> eval e''
      Nothing -> pure Interpreter.Nil
eval _ = undefined

searchValue :: String -> Environment -> Maybe Value
searchValue name env =
  case filter snd (search name env) of
    [] -> Nothing
    xs ->
      let (Environment vs _, _) = last xs
       in lookup name vs

search :: String -> Environment -> [(Environment, Bool)]
search name e@(Environment vs (Just e')) =
  case lookup name vs of
    Just _ -> (e, True) : search name e'
    Nothing -> (e, False) : search name e'
search name e@(Environment vs Nothing) =
  case lookup name vs of
    Just _ -> [(e, True)]
    Nothing -> [(e, False)]

isEqual :: Expr -> Expr -> Bool
isEqual (Literal Expr.Nil) (Literal Expr.Nil) = True
isEqual (Literal Expr.Nil) _ = False
isEqual (Literal (Expr.Boolean b1)) (Literal (Expr.Boolean b2)) = b1 == b2
isEqual (Literal (Expr.String s1)) (Literal (Expr.String s2)) = s1 == s2
isEqual (Literal (Expr.Number n1)) (Literal (Expr.Number n2)) = n1 == n2
isEqual _ _ = False

isTruthy :: Value -> Bool
isTruthy Interpreter.Nil = False
isTruthy (Interpreter.Boolean False) = False
isTruthy _ = True
