{-# LANGUAGE RecordWildCards #-}

module Resolver (resolve, resolve'') where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT (runStateT), gets, modify)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Expr (Expr (..))
import Interpreter (Locals, LoxState (LoxState, locals, scopes), Result, Scope, emptyState)
import qualified Parser
import Text.Megaparsec (errorBundlePretty)
import Token (LoxTok (LoxTok), WithPos (WithPos))
import TokenType (TokenType (Identifier))

resolve'' :: String -> IO ()
resolve'' src = do
  let action = case Parser.parse src of
        Right es -> mapM_ resolve es
        Left (Nothing, sErrors) -> traverse_ (liftIO . print) sErrors
        Left (Just e, Nothing) -> liftIO $ putStrLn $ errorBundlePretty e
        Left (Just _, Just _) -> liftIO $ putStrLn "no way!!!!"
  (_, st) <- runStateT (runExceptT action) emptyState
  print $ locals st
  putStrLn "---------"
  print $ scopes st
  pure ()

resolve :: Expr -> Result ()
resolve (Expr.Block es) = do
  beginScope
  traverse_ resolve es
  endScope
  pure ()
resolve (Expr.Var tok i) = do
  declare tok
  case i of
    Just i' -> resolve i' >> define tok
    Nothing -> define tok
resolve e@(Expr.Variable t@(WithPos _ _ _ (LoxTok (Identifier name) _))) = do
  s <- peek
  case M.lookup name s of
    Just k -> (if k then resolveLocal e t else error "can't read local variable in its own initializer")
    Nothing -> resolveLocal e t
resolve e@(Expr.Assign (Expr.Variable t) v) = do
  resolve v
  resolveLocal e t
  pure ()
resolve f@(Expr.Function t _ _) = do
  declare t
  define t
  resolveFunction f
resolve (Expr.Expression e) = resolve e
resolve (Expr.If c e e') = do
  resolve c
  resolve e
  maybe (pure ()) resolve e'
resolve (Expr.Print e) = resolve e
-- TODO: what if its nil?
resolve (Expr.Return e) = resolve e
resolve (Expr.While c e) = resolve c >> resolve e
resolve (Expr.Call e _ as) = do
  resolve e
  traverse_ resolve as
resolve (Expr.Grouping e) = resolve e
resolve (Expr.Literal _) = pure ()
resolve (Expr.And e e') = resolve e >> resolve e'
resolve (Expr.Or e e') = resolve e >> resolve e'
resolve (Expr.Unary _ e') = resolve e'
resolve _ = error "can't resolve that"

resolve' :: Expr -> Int -> Result ()
resolve' e depth = modify $ \s -> s {locals = M.insert e depth (locals s)}

resolveFunction :: Expr -> Result ()
resolveFunction (Expr.Function _ ps body) = do
  beginScope
  traverse_ (\p -> declare p >> define p) ps
  resolve body
  endScope
resolveFunction _ = error "can't resolve non-function"

resolveLocal :: Expr -> WithPos LoxTok -> Result ()
resolveLocal e (WithPos _ _ _ (LoxTok (Identifier name) _)) = do
  st <- get
  let ((i, s) : _) = filter (M.member name . snd) (zip [0 ..] (scopes st))
  resolve' e i

beginScope :: Result ()
beginScope = modify $ \s -> s {scopes = M.empty : scopes s}

endScope :: Result ()
endScope = modify $ \s -> s {scopes = tail (scopes s)}

declare :: WithPos LoxTok -> Result ()
declare (WithPos _ _ _ (LoxTok (Identifier name) _)) = push (name, False)
declare _ = error "can't declare non-variable"

define :: WithPos LoxTok -> Result ()
define (WithPos _ _ _ (LoxTok (Identifier name) _)) = push (name, True)
define _ = error "can't define non-variable"

peek :: Result Scope
peek = gets (head . scopes)

push :: (String, Bool) -> Result ()
push (name, k) = modify $ \s -> s {scopes = M.insert name k (head (scopes s)) : tail (scopes s)}

-- push (name, k) = modify (\(s : ss, ls) -> (M.insert name k s : ss, ls))
