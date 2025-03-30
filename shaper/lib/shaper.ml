module Token = Token
module Lexer = Lexer

type shape =
  [ `id of string
  | `int of int
  | `op of string
  | `parens of shape
  | `brackets of shape
  | `braces of shape
  | `prefix of shape * shape
  | `infix of shape * shape * shape
  | `postfix of shape * shape
  | `comma of shape list
  | `braces of shape list
  | `seq of shape list ]

