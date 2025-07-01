{-# LANGUAGE RecordWildCards #-}

module Runtime where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT, gets, modify)
import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Expr
import Text.Printf (printf)
import Token

type Scope = M.Map String Bool

type Scopes = [Scope]

type Locals = M.Map Expr Int

data Exception = ErrorTok (WithPos LoxTok, String) | ReturnException Value | Error String deriving (Show)

type Addr = Int

-- the heap
type Store = Map Addr Value

type Env = Map String Addr

data Environment = Environment {values :: Env, parent :: Maybe Environment} deriving (Show, Eq)

data LoxState = LoxState
  { environment :: Environment,
    globals :: Env,
    store :: Store,
    nextAddr :: Addr,
    locals :: Locals
  }

data Value
  = Nil
  | Void
  | Boolean Bool
  | Number Double
  | String String
  | LoxFunction Expr Environment
  deriving (Eq)

instance Show Value where
  show Runtime.Nil = "nil"
  show Runtime.Void = "void"
  show (Runtime.Boolean b) = show b
  show (Runtime.Number n) = show n
  show (Runtime.String s) = s
  show (Runtime.LoxFunction _ _) = "<LoxFunction>"

type Result a = ExceptT Exception (StateT LoxState IO) a

emptyState :: LoxState
emptyState = LoxState emptyEnv M.empty M.empty 0 M.empty

enclosed :: Environment -> Environment
enclosed env = emptyEnv {parent = Just env}

emptyEnv :: Environment
emptyEnv = Environment M.empty Nothing

alloc :: Value -> Result Addr
alloc v = do
  st@LoxState {..} <- get
  let addr = nextAddr
  put $ st {store = M.insert addr v store}
  -- increment nextAddr
  modify $ \s -> s {nextAddr = nextAddr + 1}
  pure addr

readAddr :: Addr -> Result Value
readAddr a = gets (fromMaybe (error "invalid addr") . M.lookup a . store)

writeAddr :: Addr -> Value -> Result ()
writeAddr a v = modify $ \st -> st {store = M.insert a v (store st)}

lookupVar :: String -> Environment -> Result Addr
lookupVar name Environment {..} =
  case M.lookup name values of
    Just a -> pure a
    Nothing -> case parent of
      Just p -> lookupVar name p
      Nothing -> error $ "unbound variable: " <> name

defineVar :: String -> Value -> Result ()
defineVar n v = do
  addr <- alloc v
  modify $ \s@LoxState {..} ->
    s {environment = environment {values = M.insert n addr (values environment)}}

formatStore :: Store -> String
formatStore store =
  "Store:\n"
    ++ if M.null store
      then "  (empty)\n"
      else concatMap formatEntry (M.toList store)
  where
    formatEntry :: (Addr, Value) -> String
    formatEntry (addr, val) = printf "  [%d] -> %s\n" addr (show val)

formatEnvironment :: Environment -> String
formatEnvironment = go 0
  where
    go :: Int -> Environment -> String
    go depth (Environment vals parent) =
      indent depth
        ++ "Environment:\n"
        ++ indent (depth + 1)
        ++ "values: [\n"
        ++ formatBindings vals (depth + 2)
        ++ indent (depth + 1)
        ++ "]\n"
        ++ case parent of
          Nothing -> indent (depth + 1) ++ "parent: None\n"
          Just p -> indent (depth + 1) ++ "parent:\n" ++ go (depth + 2) p

    formatBindings :: Env -> Int -> String
    formatBindings env d
      | M.null env = indent d ++ "-- empty --\n"
      | otherwise = concatMap (formatEntry d) (M.toList env)

    formatEntry :: Int -> (String, Addr) -> String
    formatEntry d (k, addr) = indent d ++ printf "(%s -> %d)\n" k addr

    indent n = replicate (n * 2) ' '

printStore :: Result ()
printStore = gets (formatStore . store) >>= liftIO . putStrLn

printEnv :: Result ()
printEnv = gets (formatEnvironment . environment) >>= liftIO . putStrLn
