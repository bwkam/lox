{-# LANGUAGE InstanceSigs #-}

module Token where

import TokenType (TokenType)

data Token = Token
  { tokenType :: TokenType,
    lexeme :: Maybe String,
    line :: Int
  }

instance Show Token where
  show :: Token -> String
  show (Token tt _ _) = show tt