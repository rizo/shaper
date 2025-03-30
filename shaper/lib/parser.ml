module P = Pratt
module L = Lexer
module G = Pratt.Grammar

let default_prefix _g l =
  match (L.pick l : Token.t) with
  | Int x ->
    L.move l;
    Ok x
  | t -> Fmt.failwith "calc: not constant: %a" Token.pp t

let prefix (tok : Token.t) =
  match tok with
  | Int x -> Some (P.const x)
  | Op "+" -> Some (P.prefix_unary tok (fun x -> x))
  | Op "-" -> Some (P.prefix_unary tok (fun x -> -x))
  | Delim "(" -> Some (P.between Lparen Rparen (fun x -> x))
  | _ -> None

let infix (tok : L.token) =
  match tok with
  | Rparen -> Some P.infix_delimiter
  | Sym "+" -> Some (P.infix_binary 30 tok ( + ))
  | Sym "-" -> Some (P.infix_binary 30 tok ( - ))
  | Sym "*" -> Some (P.infix_binary 40 tok ( * ))
  | Sym "/" -> Some (P.infix_binary 40 tok ( / ))
  | Semi -> Some (P.infix_right_binary 50 tok (fun _ x -> x))
  | Sym "!" -> Some (P.postfix_unary 70 tok fac)
  | _ -> None

let grammar = G.make ~prefix ~infix "calc"
