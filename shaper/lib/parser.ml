module P = Pratt
module L = Lexer
module G = Pratt.Grammar

module Shape = struct
  type t =
    [ `id of string
    | `op of string
    | `int of int
    | `str of string
    | `char of char
    | `parens of t
    | `brackets of t
    | `braces of t
    | `prefix of string * t
    | `infix of string * t * t
    | `postfix of string * t
    | `colon of t * t
    | `label of string * t
    | `comma of t list
    | `semi of t list
    | `seq of t list ]

  let id x : t = `id x
  let op x : t = `op x
  let int x : t = `int x
  let str x : t = `str x
  let char x : t = `char x
  let parens x : t = `parens x
  let brackets x : t = `brackets x
  let braces x : t = `braces x
  let prefix x y : t = `prefix (x, y)
  let infix x y z : t = `infix (x, y, z)
  let postfix x y : t = `postfix (x, y)
  let colon x y : t = `colon (x, y)
  let label x y : t = `label (x, y)
  let comma x : t = `comma x
  let semi x : t = `semi x
  let seq x : t = `seq x

  let rec pp f (t : t) =
    match t with
    | `id x -> Fmt.pf f "%s" x
    | `op x -> Fmt.pf f "%s" x
    | `int x -> Fmt.pf f "%d" x
    | `str x -> Fmt.pf f "%S" x
    | `char x -> Fmt.pf f "%c" x
    | `parens x -> Fmt.pf f "@[<hv2>(parens@ %a@])" pp x
    | `brackets x -> Fmt.pf f "@[<hv2>(brackets@ %a@])" pp x
    | `braces x -> Fmt.pf f "@[<hv2>(braces@ %a@])" pp x
    | `prefix (fix, x) -> Fmt.pf f "@[<hv2>(prefix@ %s@ %a)@]" fix pp x
    | `infix (fix, x, y) ->
        Fmt.pf f "@[<hv2>(infix@ %s@ %a@ %a)@]" fix pp x pp y
    | `postfix (fix, x) -> Fmt.pf f "@[<hv2>(postfix@ %s@ %a)@]" fix pp x
    | `colon (x, y) -> Fmt.pf f "(: %a %a)" pp x pp y
    | `label (x, y) -> Fmt.pf f "(%s: %a)" x pp y
    | `comma xs -> Fmt.pf f "(, @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
    | `semi xs -> Fmt.pf f "(; @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
    | `seq xs -> Fmt.pf f "(_ @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
end

(*
  Operator precedence, also known as binding power, is defined as a number.

  Higher numbers represent higher binding power.

  The sign of the number controls associativity. Positive numbers have
  left-to-right associativity, while negative numbers represent right-to-left
  associativity. 

  SEE: https://ocaml.org/manual/5.3/api/Ocaml_operators.html
*)
module Power = struct
  let semi = 10
  let comma = 20
  let colon = 25

  let get str =
    match str with
    | "=" -> -30
    | "|" -> -40
    | ":" -> -50
    | "::" -> 55
    | "->" -> -60
    | "!" -> 60
    | ":=" -> -60
    | "<-" -> -60
    | "&" | "&&" -> -70
    | "||" -> -70
    | "**" -> -80
    | _ -> (
        match str.[0] with
        | '@' -> 100
        | '=' -> 101
        | '<' | '>' -> 102
        | '#' | '&' -> 102
        | '|' -> 102
        | '+' | '-' -> 103
        | '*' | '/' -> 104
        | _ -> 100)

  let juxt = 300
  let dot = 310
end

let ( let* ) = Result.bind

let prefix (tok : Token.t) =
  match tok with
  | Int x -> Some (P.const (Shape.int x))
  | Str x -> Some (P.const (Shape.str x))
  | Char x -> Some (P.const (Shape.char x))
  | Id x -> Some (P.const (Shape.id x))
  | Op x -> Some (P.prefix_unary tok (fun shape -> Shape.prefix x shape))
  | Lparen -> Some (P.between Lparen Rparen Shape.parens)
  | Lbrace -> Some (P.between Lbrace Rbrace Shape.braces)
  | Lbracket -> Some (P.between Lbracket Rbracket Shape.brackets)
  | _ -> None

let infix (tok : Token.t) =
  match tok with
  | Rparen | Rbrace | Rbracket -> Some P.infix_unbalanced
  | Semi -> Some (P.infix_seq_opt ~sep:(tok, Power.semi) Shape.semi)
  | Comma ->
      Some (P.infix_seq ~sep:(Token.eq tok) ~power:Power.comma Shape.comma)
  | Op x ->
      let power = Power.get x in
      Some
        (P.infix_or_postfix power tok
           ~infix:(fun a b -> Shape.infix x a b)
           ~postfix:(fun a -> Shape.postfix x a))
  | Eof -> Some P.eof
  | _ -> None

let default_infix = (P.parse_infix_juxt ~power:Power.juxt Shape.seq, Power.juxt)
let grammar = G.make ~default_infix ~prefix ~infix "shaper"
let parse lexer = P.run grammar lexer
