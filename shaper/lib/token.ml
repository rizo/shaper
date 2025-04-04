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

let pp =
  let pf = Format.fprintf in
  fun f token ->
    match token with
    | Id x -> pf f "%s" x
    | Lparen -> pf f "("
    | Rparen -> pf f ")"
    | Lbrace -> pf f "{"
    | Rbrace -> pf f "}"
    | Lbracket -> pf f "["
    | Rbracket -> pf f "]"
    | Comma -> pf f ","
    | Semi -> pf f ";"
    | Str x -> pf f "%S" x
    | Char x -> pf f "%c" x
    | Int x -> pf f "%d" x
    | Op x -> pf f "%s" x
    | Eof -> pf f "(eof)"

let eq t1 t2 = Stdlib.( = ) t1 t2
let cmp = Stdlib.compare
let is_eof = function Eof -> true | _ -> false
