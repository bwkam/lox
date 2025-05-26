{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Token where

import Text.Megaparsec (Stream (Tokens), Token)
import TokenType (TokenType)

data LoxTok = LoxTok
  { tokenType :: TokenType,
    lexeme :: Maybe String,
    line :: Int
  }
  deriving (Ord, Eq)

instance Show LoxTok where
  show :: LoxTok -> String
  show (LoxTok tt _ _) = show tt

instance Stream LoxTok where
  type Token LoxTok = LoxTok
  type Tokens LoxTok = [LoxTok]