module Token = Token
module Lexer = Lexer

module M1 = struct
  type ident = Upper of string | Lower of string
  type const = Int of int | Char of char | String of string | Float of float

  type syntax =
    | Ident of string
    | Const of const
    | Sym of string
    | Block of string * syntax * string
    | Seq of string option * syntax list
    | Infix of syntax * string * syntax
    | Prefix of string * syntax
    | Postfix of syntax * string
end

module M2 = struct
  type term = Id of string | Int of int | Sym of string | Shape of term list

  (* 2 + x *)
  let t1 = Shape [ Int 2; Sym "+"; Id "x" ]

  (* `2 + x` *)
  let t1 = Shape [ Sym "`"; Shape [ Int 2; Sym "+"; Id "x" ]; Sym "`" ]

  (* `2 + $x` *)
  let t1 =
    Shape
      [ Sym "`"; Shape [ Int 2; Sym "+"; Shape [ Sym "$"; Id "x" ] ]; Sym "`" ]
end

module M3 = struct
  type block = Parens | Brackets | Braces
  type sep = Sp | Semi | Comma

  type syntax =
    (* a foo Bar *)
    | Id of string
    (* 0 42 1_000_000 *)
    | Int of int
    (* @ -> >>= <+> *)
    | Sym of string
    (* (a) [b] {c}  *)
    | Block of block * syntax (* a + b *)
    (* -a *)
    | Prefix of string * syntax
    (* a! *)
    | Postfix of string * syntax
    (* a + b *)
    | Infix of string * syntax * syntax (* a, b => Infix ("," Id "a", Id "b") *)
    (* a b c *)
    (* a, b, c *)
    (* a; b; c; *)
    | Seq of sep * syntax list
    | Scope of syntax * syntax * syntax option
    (* `a + (2 - $x)` *)
    | Quote of syntax

  (* [a] *)
  (* Shape [Id "["; Id "a"; Id "]"] *)

  (* Shape [Id "a"; Sym "+"; Id "b"] *)
  (* Shape [Id "a"; Sym "!"] *)
  (* Shape [Sym "-"; Id "a"] *)

  (* 2 + x *)
  let t1 = Infix ("+", Int 2, Id "x")

  (* `2 + x` *)
  let t1 = Quote (Infix ("+", Int 2, Id "x"))

  (* `2 + $x` *)
  let t1 = Quote (Infix ("+", Int 2, Prefix ("$", Id "x")))

  (* select: (name, age) from: users where: age > 18 *)
  (*let t1 =*)
  (*  Seq*)
  (*    ( Colon,*)
  (*      [*)
  (*        Id "select";*)
  (*        Block (Parens, Seq (Comma, [ Id "name"; Id "age" ]));*)
  (*        Id "from";*)
  (*        Id "users";*)
  (*        Id "where";*)
  (*        Infix (">", Id "age", Int 18);*)
  (*      ] )*)

  (* select: (name, age) from: users where: age > 18 *)
  let t1 =
    Scope
      ( Id "select",
        Block (Parens, Seq (Comma, [ Id "name"; Id "age" ])),
        Some
          (Scope
             ( Id "from",
               Id "users",
               Some (Scope (Id "where", Infix (">", Id "age", Int 18), None)) ))
      )

  (* 5 + {a} / 3 *)
  let t1 = Infix ("+", Int 5, Infix ("/", Block (Braces, Id "a"), Int 3))

  (* if f a b then c d else e f *)
  (* if: f a b then: c d else: e f *)
end
