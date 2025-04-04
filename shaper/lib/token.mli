type t =
  | Id of string
  | Op of string
  | Int of int
  | Str of string
  | Char of char
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  | Comma
  | Semi
  | Eof

val pp : Format.formatter -> t -> unit
val cmp : t -> t -> int
val eq : t -> t -> bool
val is_eof : t -> bool
