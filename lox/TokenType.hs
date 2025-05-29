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
  | Newline
  | Tab
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
    Identifier String
  | StringLit String
  | Number Double
  | -- Comments
    Comment String
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
  deriving (Eq, Ord)

instance Show TokenType where
  show token = case token of
    -- Single-character tokens
    LeftParen -> "("
    RightParen -> ")"
    LeftBrace -> "{"
    RightBrace -> "}"
    Comma -> ","
    Dot -> "."
    Minus -> "-"
    Plus -> "+"
    Semicolon -> ";"
    Slash -> "/"
    Star -> "*"
    -- One or two character tokens
    Bang -> "!"
    BangEqual -> "!="
    Equal -> "="
    EqualEqual -> "=="
    Greater -> ">"
    GreaterEqual -> ">="
    Less -> "<"
    LessEqual -> "<="
    -- Literals
    Identifier s -> s
    StringLit s -> "\"" ++ s ++ "\""
    Number n -> show n
    -- Comments
    Comment s -> "//" ++ s
    -- Keywords
    And -> "and"
    Class -> "class"
    Else -> "else"
    False_ -> "false"
    Fun -> "fun"
    For -> "for"
    If -> "if"
    Nil -> "nil"
    Or -> "or"
    Print -> "print"
    Return -> "return"
    Super -> "super"
    This -> "this"
    True_ -> "true"
    Newline -> "\n"
    Tab -> "tab"
    Var -> "var"
    While -> "while"
    EOF -> "EOF"
