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
  let comma x : t = `comma x
  let semi x : t = `semi x
  let seq x : t = `seq x

  let rec pp_sexp f (t : t) =
    match t with
    | `id x -> Fmt.pf f "%s" x
    | `op x -> Fmt.pf f "%s" x
    | `int x -> Fmt.pf f "%d" x
    | `str x -> Fmt.pf f "%S" x
    | `char x -> Fmt.pf f "%C" x
    | `parens x -> Fmt.pf f "@[<hv2>((_)@ %a@])" pp_sexp x
    | `brackets x -> Fmt.pf f "@[<hv2>([_]@ %a@])" pp_sexp x
    | `braces x -> Fmt.pf f "@[<hv2>({_}@ %a@])" pp_sexp x
    | `prefix (fix, x) -> Fmt.pf f "@[<hv2>(%s_@ %a)@]" fix pp_sexp x
    | `infix (fix, x, y) ->
        Fmt.pf f "@[<hv2>(_%s_@ %a@ %a)@]" fix pp_sexp x pp_sexp y
    | `postfix (fix, x) -> Fmt.pf f "@[<hv2>(_%s@ %a)@]" fix pp_sexp x
    | `comma xs -> Fmt.pf f "(, @[%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs
    | `semi xs -> Fmt.pf f "(; @[%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs
    | `seq [] -> Fmt.pf f "()"
    | `seq xs -> Fmt.pf f "(_ @[%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs

  let rec pp f (t : t) =
    match t with
    | `id x -> Fmt.pf f "%s" x
    | `op x -> Fmt.pf f "%s" x
    | `int x -> Fmt.pf f "%d" x
    | `str x -> Fmt.pf f "%S" x
    | `char x -> Fmt.pf f "%C" x
    | `parens x -> Fmt.pf f "@[<hv2>(%a@])" pp x
    | `brackets x -> Fmt.pf f "@[<hv2>[%a@]]" pp x
    | `braces x -> Fmt.pf f "@[<hv2>{%a@]}" pp x
    | `prefix (op, x) -> Fmt.pf f "@[<hv2>%s%a@]" op pp x
    | `infix (op, x, y) -> Fmt.pf f "@[<hv2>%a@ %s@ %a@]" pp x op pp y
    | `postfix (op, x) -> Fmt.pf f "@[<hv2>%a%s@]" pp x op
    | `comma xs -> Fmt.pf f "@[%a@]" (Fmt.list ~sep:Fmt.comma pp) xs
    | `semi xs -> Fmt.pf f "@[%a@]" (Fmt.list ~sep:Fmt.semi pp) xs
    | `seq xs -> Fmt.pf f "@[%a@]" (Fmt.list ~sep:Fmt.sp pp) xs
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
  (* FIXME *)
  | Op ".." -> Some (P.const (Shape.op ".."))
  | Op x -> Some (P.prefix_unary tok (fun shape -> Shape.prefix x shape))
  | Lparen ->
      P.prefix_scope Lparen Rparen (function
        | None -> Shape.parens (Shape.seq [])
        | Some x -> Shape.parens x)
      |> Option.some
  | Lbrace ->
      P.prefix_scope Lbrace Rbrace (function
        | None -> Shape.braces (Shape.seq [])
        | Some x -> Shape.braces x)
      |> Option.some
  | Lbracket ->
      P.prefix_scope Lbracket Rbracket (function
        | None -> Shape.brackets (Shape.seq [])
        | Some x -> Shape.brackets x)
      |> Option.some
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
