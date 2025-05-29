{-# LANGUAGE InstanceSigs #-}

module Expr where

import Token

data Expr
  = Binary Expr LoxTok Expr
  | Grouping Expr
  | Literal LiteralValue
  | Unary LoxTok Expr

data LiteralValue = Number Double | String String | Boolean Bool | Nil
  deriving (Eq)

instance Show LiteralValue where
  show :: LiteralValue -> String
  show (Number x) = show x
  show (String s) = s
  show (Boolean bool) = show bool
  show Nil = ""

instance Show Expr where
  show (Binary e1 (LoxTok tt _ _) e2) = "(" <> show tt <> " " <> show e1 <> " " <> show e2 <> ")"
  show (Literal v) = show v
  show (Grouping e) = "(group " <> show e <> ")"
  show (Unary t e) = "(" <> show t <> " " <> show e <> ")"
