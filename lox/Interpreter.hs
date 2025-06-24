{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interpreter (eval, eval') where

import Control.Monad (MonadPlus (mplus), zipWithM)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), liftEither, runExceptT)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (runStateT), modify)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Expr (Expr (And, Assign, Binary, Block, Call, Expression, Function, Grouping, If, Literal, Or, Print, Return, Unary, Var, Variable, While), LiteralValue (..))
import GHC.Base (error)
import qualified Parser
import Text.Megaparsec (errorBundlePretty)
import Token (LoxTok (LoxTok, tokenType), WithPos (WithPos))
import TokenType (TokenType (Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Identifier, Less, LessEqual, Minus, Plus, Slash, Star))
import Prelude hiding (error)

data Value
  = Nil
  | Void
  | Boolean Bool
  | Number Double
  | String String
  | LoxFunction Expr
  deriving (Show)

data Exception = Error (WithPos LoxTok, String) | ReturnException Value deriving (Show)

type Env = M.Map String Value

data Environment = Environment {values :: Env, parent :: Maybe Environment, globals :: Env} deriving (Show)

type Result = ExceptT Exception (StateT Environment IO)

eval' :: String -> IO ()
eval' src = do
  let action = case Parser.parse src of
        Right es -> mapM_ eval es `catchError` (liftIO . print)
        Left (Nothing, sErrors) -> traverse_ (liftIO . print) sErrors
        Left (Just e, Nothing) -> liftIO $ putStrLn $ errorBundlePretty e
        Left (Just _, Just _) -> liftIO $ putStrLn "no way!!!!"
  _ <- runStateT (runExceptT action) (Environment M.empty Nothing M.empty)
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
  (_, _) -> throwError $ Error (wp, "wrong operands for " ++ show tt)
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
    _ -> throwError $ Error (wp, "wrong operands for " ++ show (tokenType t))
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
    Interpreter.Void -> do
      liftIO $ putStrLn "void"
      pure Interpreter.Void
    (Interpreter.String s) -> do
      liftIO $ print s
      pure $ Interpreter.String s
    (Interpreter.Boolean b) -> do
      liftIO $ print b
      pure $ Interpreter.Boolean b
    (Interpreter.LoxFunction f) -> do
      liftIO $ putStrLn "some function"
      pure $ Interpreter.LoxFunction f
eval (Expr.Var (WithPos _ _ _ (LoxTok (Identifier name) _)) e) = do
  env <- get
  case e of
    Just e' -> do
      v <- eval e'
      put $ setVar env (name, v)
      pure v
    Nothing -> do
      put $ setVar env (name, Interpreter.Nil)
      pure Interpreter.Nil
eval (Expr.Variable wp@(WithPos _ _ _ (LoxTok (Identifier name) _))) = do
  env <- get
  maybe (throwError $ Error (wp, "undefined variable: " <> name)) pure (getVar env name)
eval (Expr.Assign (Variable wp@(WithPos _ _ _ (LoxTok (Identifier name) _))) r) = do
  v' <- eval r
  env <- get

  case assignVar env (name, v') of
    Just e' -> do
      put e'
      pure v'
    Nothing -> throwError $ Error (wp, "undefined variable: " <> name)
eval (Expr.Block es) = do
  openNewClosure
  e <- last <$> traverse eval es
  closeNewClosure
  pure e
  where
    openNewClosure :: Result ()
    openNewClosure = get >>= put . enclosed
    closeNewClosure :: Result ()
    closeNewClosure = get >>= put . fromJust . parent
eval (Expr.If c e e') = do
  c' <- eval c

  if isTruthy c'
    then eval e
    else case e' of
      Just e'' -> eval e''
      Nothing -> pure Interpreter.Nil
eval (Expr.Or e1 e2) = do
  left <- eval e1
  if isTruthy left
    then pure left
    else eval e2
eval (Expr.And e1 e2) = do
  left <- eval e1
  if not $ isTruthy left
    then pure left
    else eval e2
eval (Expr.While c e) = do
  t <- eval c
  go $ isTruthy t
  where
    go False = pure Interpreter.Void
    go True = eval e >> (eval c >>= (go . isTruthy))
eval (Expr.Call callable parens as) = do
  env <- get -- current environment
  e <- case callable of
    Variable (WithPos _ _ _ (LoxTok (TokenType.Identifier n) _)) ->
      case lookupVar n env of
        Just v ->
          case v of
            -- when the callable is a function
            LoxFunction (Function _ ps (Expr.Block es)) -> do
              if length ps /= length as
                then error "ps and as don't match"
                else
                  ( do
                      oldEnv <- get
                      put $ enclosed oldEnv
                      populate ps as -- insert args/params values into the new env
                      e <- (last <$> traverse eval es) `catchError` checkReturnOrError
                      put oldEnv
                      pure e
                  )
            _ -> error "unexpected"
        Nothing -> error "function isn't defined"
    _ -> error "function isn't a variable"
  pure e
  where
    checkReturnOrError :: Exception -> Result Value
    checkReturnOrError e@(Error _) = throwError e
    checkReturnOrError (ReturnException v) = liftEither $ Right v
    -- args -> params
    populate :: [WithPos LoxTok] -> [Expr] -> Result ()
    populate as' ps =
      do
        vars <-
          zipWithM
            ( \a e ->
                case a of
                  (WithPos _ _ _ (LoxTok (TokenType.Identifier n) _)) ->
                    do
                      e' <- eval e
                      pure (n, e')
                  _ -> error "expected an identifier"
            )
            as'
            ps
        mapM_
          ( \p ->
              get >>= \e -> put $ setVar e p
          )
          vars
eval (Expr.Return e) = do
  v <- eval e
  throwError $ ReturnException v
eval f@(Expr.Function (WithPos _ _ _ (LoxTok (TokenType.Identifier n) _)) _ _) = do
  let fun = LoxFunction f
  env <- get
  put $ setVar env (n, fun)
  pure Interpreter.Nil
eval _ = undefined

assignVar :: Environment -> (String, Value) -> Maybe Environment
assignVar env (n, v) = maybeAssignToEnv `mplus` maybeAssignToParentEnv
  where
    maybeAssignToEnv = M.lookup n (values env) >> return (setVar env (n, v))
    maybeAssignToParentEnv = do
      parentEnv <- parent env
      parentEnv' <- assignVar parentEnv (n, v)
      return $ env {parent = Just parentEnv'}

lookupVar :: String -> Environment -> Maybe Value
lookupVar s env = lookupCur `mplus` lookupEnclosing
  where
    lookupCur = M.lookup s (values env)
    lookupEnclosing = do
      parentEnv <- parent env
      lookupVar s parentEnv

setVar :: Environment -> (String, Value) -> Environment
setVar env (n, v) = env {values = M.insert n v (values env)}

getVar :: Environment -> String -> Maybe Value
getVar env n = maybeValueFromEnv `mplus` maybeValueFromParentEnv
  where
    maybeValueFromEnv = M.lookup n (values env)
    maybeValueFromParentEnv = parent env >>= \e -> getVar e n

enclosed :: Environment -> Environment
enclosed env = emptyEnv {parent = Just env}

emptyEnv :: Environment
emptyEnv = Environment {values = M.empty, parent = Nothing, globals = M.empty}

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
