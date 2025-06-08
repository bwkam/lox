{-# LANGUAGE InstanceSigs #-}

module Expr where

import Data.List (intersperse)
import Token

data Expr
  = Binary Expr (WithPos LoxTok) Expr
  | Grouping Expr
  | Literal LiteralValue
  | Unary (WithPos LoxTok) Expr
  | Expression Expr
  | Print Expr
  | If Expr Expr (Maybe Expr)
  | Var (WithPos LoxTok) (Maybe Expr)
  | Variable (WithPos LoxTok)
  | Assign Expr Expr
  | Or Expr Expr
  | While Expr Expr
  | And Expr Expr
  | Call Expr (WithPos LoxTok) [Expr]
  | Block [Expr]
  | Function (WithPos LoxTok) [WithPos LoxTok] Expr
  deriving (Show)

data LiteralValue = Number Double | String String | Boolean Bool | Nil
  deriving (Eq)

instance Show LiteralValue where
  show :: LiteralValue -> String
  show (Number x) = show x
  show (String s) = s
  show (Boolean bool) = show bool
  show Nil = ""

-- instance Show Expr where
--   show (Binary e1 (WithPos _ _ _ (LoxTok tt _)) e2) = "(" <> show tt <> " " <> show e1 <> " " <> show e2 <> ")"
--   show (Literal v) = show v
--   show (Grouping e) = "(group " <> show e <> ")"
--   show (Unary t e) = "(" <> show t <> " " <> show e <> ")"
--   show (Expression e) = show e
--   show (Print e) = "(print " <> show e <> ")"
--   show (Var t e) = "(var " <> show t <> f <> ")"
--     where
--       f = case e of
--         Just e' -> " = " <> show e'
--         Nothing -> ""
--   show (Variable t) = "(var " <> show t
--   show (Assign t e) = "(" <> show t <> " = " <> show e <> ")"
--   show (Block es) = show es
--   show (If c e e') = "(if (" <> show c <> ") then (" <> show e <> ") " <> f
--     where
--       f = case e' of
--         Just x -> "(else (" <> show x <> "))"
--         Nothing -> ""
--   show (Or e1 e2) = "(or " <> show e1 <> " " <> show e2 <> ")"
--   show (And e1 e2) = "(and " <> show e1 <> " " <> show e2 <> ")"
--   show (While e1 e2) = "(while (" <> show e1 <> ") (" <> show e2 <> ")"

-- show (Call c _ es) = "(" <> show c <> "(" <> intersperse ',' (show es) <> ")"