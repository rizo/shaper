module P = Pratt
module L = Lexer
module G = Pratt.Grammar

module Shape2 = struct
  type t =
    [ `id of string
    | `op of string
    | `int of int
    | `str of string
    | `parens of t
    | `brackets of t
    | `braces of t
    | `prefix of [ `op of string | `colon of t ] * t
    | `infix of [ `op of string | `colon of t ] * t * t
    | `postfix of t * t
    | `comma of t list
    | `semi of t list
    | `seq of t list ]
end

module Shape = struct
  type t =
    [ `id of string
    | `op of string
    | `int of int
    | `str of string
    | `parens of t
    | `brackets of t
    | `braces of t
    | `prefix of t * t
    | `infix of t * t * t
    | `postfix of t * t
    | `comma of t list
    | `semi of t list
    | `seq of t list ]

  let id x : t = `id x
  let op x : t = `op x
  let int x : t = `int x
  let str x : t = `str x
  let parens x : t = `parens x
  let brackets x : t = `brackets x
  let braces x : t = `braces x
  let prefix x y : t = `prefix (x, y)
  let infix x y z : t = `infix (x, y, z)
  let postfix x : t = `postfix x
  let comma x : t = `comma x
  let semi x : t = `semi x
  let seq x : t = `seq x

  let rec pp f t =
    match t with
    | `id x -> Fmt.pf f "%s" x
    | `op x -> Fmt.pf f "%s" x
    | `int x -> Fmt.pf f "%d" x
    | `str x -> Fmt.pf f "%S" x
    | `parens x -> Fmt.pf f "(@[<hv1>%a@])" pp x
    | `brackets x -> Fmt.pf f "[@[<hv1>%a@]]" pp x
    | `braces x -> Fmt.pf f "{@[<hv1>%a@]}" pp x
    | `prefix (fix, x) -> Fmt.pf f "(prefix %a %a)" pp fix pp x
    | `infix (fix, x, y) -> Fmt.pf f "(infix %a %a %a)" pp fix pp x pp y
    | `postfix (fix, x) -> Fmt.pf f "(post %a %a)" pp fix pp x
    | `comma xs -> Fmt.pf f "(, @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
    | `semi xs -> Fmt.pf f "(; @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
    | `seq xs -> Fmt.pf f "(_ @[%a@])" (Fmt.list ~sep:Fmt.sp pp) xs
end

module Prec = struct
  let semi = 10
  let item = 11
  let ampr = 19
  let comma = 20
  let equal = 30
  let pipe = 40
  let on = 40
  let colon = 50
  let colon_colon = 55
  let arrow = 60
  let or' = 65
  let as' = 70
  let excl = 210
  let juxt = 300
  let dot = 310

  (* TODO left/right *)
  let get str =
    match str.[0] with
    | '=' -> 101
    | '<' | '>' -> 102
    | '#' | '&' -> 102
    | '|' -> 102
    | '+' | '-' -> 103
    | '*' | '/' -> 104
    | _ -> 100
end

let default_prefix _g l =
  match (L.pick l : Token.t) with
  | Int x ->
      L.move l;
      Ok x
  | t -> Fmt.failwith "calc: not constant: %a" Token.pp t

let prefix (tok : Token.t) =
  match tok with
  | Int x -> Some (P.const (Shape.int x))
  | Op x ->
      Some (P.prefix_unary tok (fun shape -> Shape.prefix (Shape.op x) shape))
  | Lparen -> Some (P.between Lparen Rparen Shape.parens)
  | Lbrace -> Some (P.between Lbrace Rbrace Shape.braces)
  | Lbracket -> Some (P.between Lbracket Rbracket Shape.brackets)
  | _ -> None

let infix (tok : Token.t) =
  match tok with
  | Rparen | Rbrace | Rbracket -> Some P.infix_unbalanced
  | Semi -> Some (P.infix_seq_opt ~sep:(tok, Prec.semi) Shape.semi)
  | Comma -> Some (P.infix_seq ~sep:(tok, Prec.comma) Shape.comma)
  | Op x ->
      let prec = Prec.get x in
      Some (P.infix_binary prec tok (fun a b -> Shape.infix (Shape.op x) a b))
  | Eof -> Some P.eof
  | _ -> None

let grammar = G.make ~prefix ~infix "calc"
