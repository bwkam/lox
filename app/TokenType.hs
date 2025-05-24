module TokenType (TokenType (..)) where

data TokenType
  = -- Single-character tokens
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | -- One or two character tokens
    Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | -- Literals
    Identifier
  | StringLit
  | Number
  | -- Keywords
    And
  | Class
  | Else
  | False_
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True_
  | Var
  | While
  | EOF
  deriving (Eq, Show)
