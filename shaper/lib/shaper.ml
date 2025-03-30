module Token = Token
module Lexer = Lexer

type shape =
  [ `id of string
  | `int of int
  | `sym of string
  | `block of block * shape
  | `prefix of string * shape
  | `infix of string * shape * shape
  | `postfix of string * shape
  | `seq of sep * shape list ]

and block = [ `parens | `brackets | `braces ]
and sep = [ `sp | `semi | `comma ]
