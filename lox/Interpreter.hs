{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interpreter where

import Control.Monad (zipWithM)
import Control.Monad.Error.Class
import Control.Monad.Except (MonadError (catchError, throwError), runExceptT)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT (runStateT), gets, modify)
import Data.Foldable (traverse_)
import Data.Maybe (fromJust, fromMaybe)
import Expr (Expr (And, Assign, Binary, Block, Call, Expression, Function, Grouping, If, Literal, Or, Print, Return, Unary, Var, Variable, While), LiteralValue (..))
import GHC.Base (error)
import qualified Parser
import Runtime
import Text.Megaparsec (errorBundlePretty)
import Token (LoxTok (LoxTok, tokenType), WithPos (WithPos))
import TokenType (TokenType (Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Identifier, Less, LessEqual, Minus, Plus, Slash, Star))
import Prelude hiding (error)

eval' :: String -> IO ()
eval' src = do
  let action = case Parser.parse src of
        Right es -> do
          -- mapM_ resolve es `catchError` (liftIO . print)
          mapM_ eval es `catchError` (liftIO . print)
        Left (Nothing, sErrors) -> traverse_ (liftIO . print) sErrors
        Left (Just e, Nothing) -> liftIO $ putStrLn $ errorBundlePretty e
        Left (Just _, Just _) -> liftIO $ putStrLn "no way!!!!"
  (_, st) <- runStateT (runExceptT action) emptyState
  pure ()

eval :: Expr -> Result Value
eval (Literal inner) = case inner of
  Expr.Number n -> pure $ Runtime.Number n
  Expr.Boolean b -> pure $ Runtime.Boolean b
  Expr.Nil -> pure Runtime.Nil
  Expr.String s -> pure $ Runtime.String s
eval (Grouping expr) = eval expr
eval (Unary wp@(WithPos _ _ _ (LoxTok tt _)) expr) = case (tt, expr) of
  (Minus, Literal (Expr.Number n)) -> pure $ Runtime.Number $ -n
  (Bang, e) -> eval e >>= ((pure . Runtime.Boolean) . not . isTruthy)
  (_, _) -> throwError $ ErrorTok (wp, "wrong operands for " ++ show tt)
eval (Binary e1 wp@(WithPos _ _ _ t) e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, tokenType t, v2) of
    (Runtime.Number v1', Minus, Runtime.Number v2') -> pure $ Runtime.Number $ v1' - v2'
    (Runtime.Number v1', Slash, Runtime.Number v2') -> pure $ Runtime.Number $ v1' / v2'
    (Runtime.Number v1', Star, Runtime.Number v2') -> pure $ Runtime.Number $ v1' * v2'
    (Runtime.Number v1', Plus, Runtime.Number v2') -> pure $ Runtime.Number $ v1' + v2'
    (Runtime.String v1', Plus, Runtime.String v2') -> pure $ Runtime.String $ v1' ++ v2'
    (Runtime.Number v1', Greater, Runtime.Number v2') -> pure $ Runtime.Boolean $ v1' > v2'
    (Runtime.Number v1', GreaterEqual, Runtime.Number v2') -> pure $ Runtime.Boolean $ v1' >= v2'
    (Runtime.Number v1', Less, Runtime.Number v2') -> pure $ Runtime.Boolean $ v1' < v2'
    (Runtime.Number v1', LessEqual, Runtime.Number v2') -> pure $ Runtime.Boolean $ v1' <= v2'
    (_, BangEqual, _) -> pure $ Runtime.Boolean $ v1 /= v2
    (_, EqualEqual, _) -> pure $ Runtime.Boolean $ v1 == v2
    _ -> throwError $ ErrorTok (wp, "wrong operands for " ++ show (tokenType t))
eval (Expression e) = eval e
eval (Expr.Print e) = do
  e' <- eval e

  case e' of
    (Runtime.Number n) -> do
      liftIO $ print n
      pure Runtime.Nil
    Runtime.Nil -> do
      liftIO $ putStrLn "nil"
      pure Runtime.Nil
    Runtime.Void -> do
      liftIO $ putStrLn "void"
      pure Runtime.Nil
    (Runtime.String s) -> do
      liftIO $ print s
      pure Runtime.Nil
    (Runtime.Boolean b) -> do
      liftIO $ print b
      pure Runtime.Nil
    (Runtime.LoxFunction _ _) -> do
      liftIO $ putStrLn "some function"
      pure Runtime.Nil
eval (Expr.Var (WithPos _ _ _ (LoxTok (Identifier name) _)) e) = do
  v <- maybe (pure Runtime.Nil) eval e
  defineVar name v
  pure Runtime.Void
eval e@(Expr.Variable wp@(WithPos _ _ _ (LoxTok (Identifier name) _))) = do
  env <- gets environment
  -- printEnv >> printStore
  addr <- lookupVar name env
  readAddr addr
eval e@(Expr.Assign (Variable wp@(WithPos _ _ _ (LoxTok (Identifier name) _))) r) = do
  env <- gets environment
  addr <- lookupVar name env
  v <- eval r
  writeAddr addr v
  pure v
eval (Expr.Block es) = do
  openNewClosure
  e <- last <$> traverse eval es
  closeNewClosure
  pure e
  where
    openNewClosure :: Result ()
    openNewClosure = modify $ \st -> st {environment = enclosed (environment st)}
    closeNewClosure :: Result ()
    closeNewClosure = modify $ \st -> st {environment = fromJust (parent $ environment st)}
eval (Expr.If c e e') = do
  c' <- eval c

  if isTruthy c'
    then eval e
    else case e' of
      Just e'' -> eval e''
      Nothing -> pure Runtime.Nil
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
    go False = pure Runtime.Void
    go True = eval e >> (eval c >>= (go . isTruthy))
eval (Expr.Call callable parens as) = do
  env <- gets environment
  e <- case callable of
    Variable (WithPos _ _ _ (LoxTok (TokenType.Identifier n) _)) -> do
      addr <- lookupVar n env
      v <- readAddr addr
      case v of
        -- when the callable is a function
        LoxFunction f@(Function _ ps (Expr.Block es)) closure -> do
          if length ps /= length as
            then error "ps and as don't match"
            else
              ( do
                  oldEnv <- gets environment
                  vars <- evalArgsParams ps as

                  modify $ \s -> s {environment = enclosed closure}

                  populate vars

                  result <- (last <$> traverse eval es) `catchError` checkReturnOrError

                  -- printEnv >> printStore

                  modify $ \st -> st {environment = oldEnv}

                  pure result
              )
        _ -> error "unexpected"
    _ -> error "function isn't a variable"
  pure e
  where
    checkReturnOrError :: Exception -> Result Value
    checkReturnOrError e@(ErrorTok _) = throwError e
    checkReturnOrError (ReturnException v) = liftEither $ Right v
    evalArgsParams :: [WithPos LoxTok] -> [Expr] -> Result [(String, Value)]

    populate :: [(String, Value)] -> Result ()
    populate = mapM_ (uncurry defineVar)
    -- point-free (gets called with ps as')
    evalArgsParams =
      zipWithM
        ( \p a ->
            case p of
              (WithPos _ _ _ (LoxTok (TokenType.Identifier n) _)) ->
                do
                  v <- eval a
                  pure (n, v)
              _ -> error "expected an identifier"
        )
eval (Expr.Return e) = do
  v <- eval e
  throwError $ ReturnException v
eval f@(Expr.Function (WithPos _ _ _ (LoxTok (TokenType.Identifier n) _)) _ _) = do
  env <- gets environment
  defineVar n (LoxFunction f env)
  -- just so the closure captures itself
  env' <- gets environment
  addr <- lookupVar n env'
  writeAddr addr (LoxFunction f env')

  pure Runtime.Nil
eval _ = undefined

--
-- assignVar :: Environment -> (String, Value) -> Maybe Environment
-- assignVar env (n, v) = maybeAssignToEnv `mplus` maybeAssignToParentEnv
--   where
--     maybeAssignToEnv = M.lookup n (values env) >> return (setVar env (n, v))
--     maybeAssignToParentEnv = do
--       parentEnv <- parent env
--       parentEnv' <- assignVar parentEnv (n, v)
--       return $ env {parent = Just parentEnv'}
--
-- lookupVar :: String -> Environment -> Maybe Value
-- lookupVar s env = lookupCur `mplus` lookupEnclosing
--   where
--     lookupCur = M.lookup s (values env)
--     lookupEnclosing = do
--       parentEnv <- parent env
--       lookupVar s parentEnv
--
-- setVar :: Environment -> (String, Value) -> Environment
-- setVar env (n, v) = env {values = M.insert n v (values env)}
--
-- getVar :: Environment -> Locals -> (String, Expr) -> Maybe Value
-- getVar env ls (name, e) = do
--   case M.lookup e ls of
--     Just distance -> Just $ getAt env distance name
--     Nothing -> error "not implemented: lookup globals"
--
-- getAt :: Environment -> Int -> String -> Value
-- getAt env distance n = do
--   let vs = values $ ancestor env distance
--   case M.lookup n vs of
--     Just v -> v
--     Nothing -> error $ "variable " <> n <> " isn't defined"
--
-- assignAt :: Environment -> Int -> (String, Value) -> Environment
-- assignAt env distance (n, v) = go env 0
--   where
--     go :: Environment -> Int -> Environment
--     go e@(Environment _ p _) d
--       | d == distance = e {values = M.insert n v (values e)}
--       | Just p <- (parent e) = e {parent = Just $ go p (d + 1)}
--       | otherwise = error "assignAt: environment chain too short"
--
-- ancestor :: Environment -> Int -> Environment
-- ancestor env d = go env 0
--   where
--     go :: Environment -> Int -> Environment
--     go e@(Environment _ p _) d'
--       | d == d' = e
--       | otherwise = go (fromJust p) (d' + 1)
--
-- -- (trace $ formatEnv' env)
--

--
isEqual :: Expr -> Expr -> Bool
isEqual (Literal Expr.Nil) (Literal Expr.Nil) = True
isEqual (Literal Expr.Nil) _ = False
isEqual (Literal (Expr.Boolean b1)) (Literal (Expr.Boolean b2)) = b1 == b2
isEqual (Literal (Expr.String s1)) (Literal (Expr.String s2)) = s1 == s2
isEqual (Literal (Expr.Number n1)) (Literal (Expr.Number n2)) = n1 == n2
isEqual _ _ = False

--
isTruthy :: Value -> Bool
isTruthy Runtime.Nil = False
isTruthy (Runtime.Boolean False) = False
isTruthy _ = True
