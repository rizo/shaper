module P = Pratt
module L = Lexer
module G = Pratt.Grammar

type shape =
  [ `id of string
  | `op of string
  | `int of int
  | `str of string
  | `parens of shape
  | `brackets of shape
  | `braces of shape
  | `prefix of shape * shape
  | `infix of shape * shape * shape
  | `postfix of shape * shape
  | `comma of shape list
  | `semi of shape list
  | `seq of shape list ]

let id x :shape = `id x 
let op x :shape = `op x 
let int x :shape = `int x 
let str x :shape = `str x 
let parens x :shape = `parens x 
let brackets x :shape = `brackets x 
let braces x :shape = `braces x 
let prefix x y :shape = `prefix (x, y)
let infix x y z :shape = `infix (x, y, z)
let postfix x :shape = `postfix x 
let comma x :shape = `comma x 
let semi x :shape = `semi x 
let seq x :shape = `seq x 

module Prec = struct
  let semi = 42
  let comma = 43
  let get op = 100
end

let default_prefix _g l =
  match (L.pick l : Token.t) with
  | Int x ->
    L.move l;
    Ok x
  | t -> Fmt.failwith "calc: not constant: %a" Token.pp t

let prefix (tok : Token.t) =
  match tok with
  | Int x -> Some (P.const (int x))
  | Op x -> Some (P.prefix_unary tok (fun shape -> prefix (op x) shape))
  | Lparen -> Some (P.between Lparen Rparen parens)
  | Lbrace -> Some (P.between Lbrace Rbrace braces)
  | Lbracket -> Some (P.between Lbracket Rbracket brackets)
  | _ -> None

let infix (tok : Token.t) =
  match tok with
  | Rparen | Rbrace | Rbracket -> Some P.infix_unbalanced
  | Semi -> Some (P.infix_seq_opt ~sep:(tok, Prec.semi) semi)
  | Comma -> Some (P.infix_seq ~sep:(tok, Prec.comma) comma)
  | Op x -> Some (P.infix_binary (Prec.get x) tok (fun a b -> infix (op x) a b))
  | Eof -> Some P.eof
  | _ -> None

let grammar = G.make ~prefix ~infix "calc"
