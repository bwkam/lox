module Token where

import TokenType (TokenType)

data Token = Token
  { tokenType :: TokenType,
    lexeme :: String,
    literal :: String, -- Object ?
    line :: Int
  }

instance Show Token where
  show t = show (tokenType t) <> " " <> lexeme t <> " " <> literal t