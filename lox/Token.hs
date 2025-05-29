{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Token where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.Proxy (Proxy)
import Text.Megaparsec (Stream (Tokens), Token, VisualStream (showTokens, tokensLength))
import TokenType (TokenType (Tab))

type LoxToks = [LoxTok]

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

instance VisualStream [LoxTok] where
  showTokens :: Proxy [LoxTok] -> NonEmpty (Token [LoxTok]) -> String
  showTokens _ ts = mconcat $ map show (toList ts)
  tokensLength :: Proxy [LoxTok] -> NonEmpty (Token [LoxTok]) -> Int
  tokensLength _ ts = length $ toList ts
