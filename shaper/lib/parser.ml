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
    | `comma [] -> Fmt.pf f "(,)"
    | `comma xs -> Fmt.pf f "(, @[%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) xs
    | `semi [] -> Fmt.pf f "(;)"
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
    | "." -> 310
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
end

let ( let* ) = Result.bind

let parse_prefix_op op g l =
  Lexer.move l;
  let tok_right = Lexer.pick l in
  if G.has_infix tok_right g then Ok (Shape.op op)
  else
    let* x = P.parse g l in
    Ok (Shape.prefix op x)

let parse_infix_op op =
  let power = Power.get op in
  let lbp = abs power in
  let rbp = if power < 0 then lbp - 1 else lbp in
  let rule left g l =
    Lexer.move l;
    let tok_right = Lexer.pick l in
    if G.has_infix tok_right g then Ok (Shape.postfix op left)
    else
      let* right = P.parse ~power:rbp g l in
      Ok (Shape.infix op left right)
  in
  (rule, lbp)

let parse_block tok1 tok2 mk_block =
  let rule g l =
    let* () = P.consume tok1 l in
    if Lexer.pick l = tok2 then
      let* () = P.consume tok2 l in
      Ok (mk_block (Shape.seq []))
    else
      let* x = P.parse g l in
      let* () = P.consume tok2 l in
      Ok (mk_block x)
  in
  rule

let prefix (tok : Token.t) =
  match tok with
  | Int x -> Some (P.const (Shape.int x))
  | Str x -> Some (P.const (Shape.str x))
  | Char x -> Some (P.const (Shape.char x))
  | Id x -> Some (P.const (Shape.id x))
  | Op op -> Some (parse_prefix_op op)
  | Lparen -> Some (parse_block Lparen Rparen Shape.parens)
  | Lbrace -> Some (parse_block Lbrace Rbrace Shape.braces)
  | Lbracket -> Some (parse_block Lbracket Rbracket Shape.brackets)
  | Semi -> Some (P.const (Shape.semi []))
  | Comma -> Some (P.const (Shape.comma []))
  | _ -> None

let infix (tok : Token.t) =
  match tok with
  | Rparen | Rbrace | Rbracket -> Some P.infix_unbalanced
  | Semi -> Some (P.infix_seq_opt ~sep:(tok, Power.semi) Shape.semi)
  | Comma -> Some (P.infix_seq_opt ~sep:(tok, Power.comma) Shape.comma)
  | Op op -> Some (parse_infix_op op)
  | Eof -> Some P.eof
  | _ -> None

let default_infix = (P.parse_infix_juxt ~power:Power.juxt Shape.seq, Power.juxt)
let grammar = G.make ~default_infix ~prefix ~infix "shaper"
let parse lexer = P.run grammar lexer
