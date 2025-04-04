(* Single-letter variables in this module:
    - p for parser
    - g for grammar
    - l for lexer
    - t for token


   power:
   - delimiter = 1, to allow lbp(1) > rbp(0) and trigger invalid infix
*)

let ( let* ) = Result.bind

type reason =
  | Unexpected of { expected : Token.t option; actual : Token.t option }
  | Invalid_infix of Token.t
  | Invalid_prefix of Token.t
  | Unbalanced of Token.t
  | Zero
  | Halted of Token.t

type error = { reason : reason; ctx : string; loc : Lexer.loc option }

let unexpected_token ~ctx ?expected actual =
  Error
    { reason = Unexpected { expected; actual = Some actual }; ctx; loc = None }

let unexpected_end ~ctx ?expected () =
  Error { reason = Unexpected { expected; actual = None }; ctx; loc = None }

let invalid_prefix ?loc ~ctx t = Error { reason = Invalid_prefix t; ctx; loc }
let invalid_infix ?loc ~ctx t = Error { reason = Invalid_infix t; ctx; loc }
let unbalanced ~ctx t = Error { reason = Unbalanced t; ctx; loc = None }

let error_to_string err =
  let pre =
    match err.loc with
    | None -> err.ctx
    | Some loc -> Fmt.str "%a: %s" Lexer.pp_loc loc err.ctx
  in
  match err.reason with
  | Unexpected { expected = Some t1; actual = Some t2 } ->
      Fmt.str "%s: invalid syntax: expected '%a' but got '%a'" pre Token.pp t1
        Token.pp t2
  | Unexpected { expected = Some t; actual = None } ->
      Fmt.str "%s: invalid syntax: end of file while expecting '%a'" pre
        Token.pp t
  | Unexpected { expected = None; actual = None } ->
      Fmt.str "%s: invalid syntax: unexpected end of file" pre
  | Unexpected { expected = None; actual = Some t } ->
      Fmt.str "%s: invalid syntax: unexpected token '%a'" pre Token.pp t
  | Invalid_infix token ->
      Fmt.str "%s: invalid syntax: '%a' cannot be used in infix postion" pre
        Token.pp token
  | Invalid_prefix token ->
      Fmt.str "%s: invalid syntax: '%a' cannot be used in prefix position" pre
        Token.pp token
  | Unbalanced token ->
      Fmt.str "%s: invalid syntax: unbalanced '%a'" pre Token.pp token
  | Zero -> Fmt.str "%s: invalid syntax: empty parser result" pre
  | Halted tok ->
      Fmt.str "%s: invalid syntax: parser halted at %a" pre Token.pp tok

(* Parser type *)

type 'a parser = Lexer.t -> ('a, error) result

type 'a grammar = {
  name : string;
  prefix : Token.t -> ('a grammar -> 'a parser) option;
  infix : Token.t -> (('a -> 'a grammar -> 'a parser) * int) option;
  default_prefix : 'a grammar -> 'a parser;
  default_infix : ('a -> 'a grammar -> 'a parser) * int;
}

module Grammar = struct
  let default_prefix_err g l =
    let tok = Lexer.pick l in
    let loc = Lexer.loc l in
    invalid_prefix ~loc ~ctx:g.name tok

  let default_infix_err =
    let rule _left g : 'a parser =
     fun l ->
      let tok = Lexer.pick l in
      let loc = Lexer.loc l in
      invalid_infix ~loc ~ctx:g.name tok
    in
    (rule, 0)

  let make ?(default_prefix = default_prefix_err)
      ?(default_infix = default_infix_err) ~prefix ~infix name =
    { name; prefix; infix; default_prefix; default_infix }

  let extend ~prefix ~infix g =
    let prefix tok =
      match prefix tok with None -> g.prefix tok | some -> some
    in
    let infix tok = match infix tok with None -> g.infix tok | some -> some in
    { g with prefix; infix }

  let get_prefix tok g = g.prefix tok
  let get_infix tok g = g.infix tok
  let has_prefix tok g = Option.is_some (get_prefix tok g)
  let has_infix tok g = Option.is_some (get_infix tok g)
end

(* Parser combinators *)

let consume expected : 'a parser =
 fun l ->
  match Lexer.pick l with
  | Eof -> unexpected_end ~ctx:"consume" ~expected ()
  | actual when actual = expected ->
      Lexer.move l;
      Ok ()
  | actual -> unexpected_token ~ctx:"consume" ~expected actual

let rec until pred (p : 'a parser) : 'a list parser =
 fun l ->
  let t = Lexer.pick l in
  if pred t then
    let* x = p l in
    let* xs = until pred p l in
    Ok (x :: xs)
  else Ok []

let parse_prefix g : 'a parser =
 fun l ->
  let tok = Lexer.pick l in
  if Token.is_eof tok then unexpected_end ~ctx:g.name ()
  else
    match Grammar.get_prefix tok g with
    | None -> g.default_prefix g l
    | Some rule -> rule g l

let rec parse_infix rbp left g : 'a parser =
 fun l ->
  let tok = Lexer.pick l in
  let rule, lbp =
    match Grammar.get_infix tok g with
    | Some infix -> infix
    | None -> g.default_infix
  in
  (* Fmt.epr "YYY: tok=%a   \tL=%03d\tR=%03d\t -> %s won!@." Token.pp tok lbp rbp *)
  (*   (if lbp > rbp then "L" else "R"); *)
  if lbp > rbp then
    let* left' = rule left g l in
    parse_infix rbp left' g l
  else Ok left

let parse ?power:(rbp = 0) g : 'a parser =
 fun l ->
  (* Fmt.epr "XXX: tok=%a   \tR=%d@." Token.pp (Lexer.pick l) rbp; *)
  let* left = parse_prefix g l in
  parse_infix rbp left g l

let run g l =
  match parse g l with
  | Ok x ->
      let tok = Lexer.pick l in
      if Token.is_eof tok then x
      else
        let loc = Lexer.loc l in
        failwith
          (error_to_string { reason = Halted tok; ctx = "run"; loc = Some loc })
  | Error err -> failwith (error_to_string err)

(* Rules *)

let parse_infix_juxt ~power f left g l =
  let* xs =
    until (fun tok -> not (Grammar.has_infix tok g)) (parse ~power g) l
  in
  Ok (f (left :: xs))

(* let parse_infix_juxt f left g l =
   let* xs =
     until
       (fun tok -> not (Grammar.has_infix tok g || Lexer.is_eof tok))
       (parse_prefix g) l
   in
   Ok (f (left :: xs)) *)

(* let parse_infix_juxt' ?power f left g l =
   let tok = Lexer.pick l in
   if not (Grammar.has_infix tok g || Lexer.is_eof tok) then
     let* right = parse ?power g l in
     let* xs =
       until
         (fun tok -> not (Grammar.has_infix tok g || Lexer.is_eof tok))
         (parse_prefix g) l
     in
     Ok (f (left :: xs))
   else Ok (f [ left ]) *)

let parse_prefix_juxt =
  let rule g l =
    until (fun tok -> not (Grammar.has_infix tok g)) (parse_prefix g) l
  in
  rule

let infix_delimiter =
  let rule _left g l =
    let tok = Lexer.pick l in
    invalid_infix ~ctx:g.name tok
  in
  (rule, 0)

let infix_unbalanced =
  let rule _left g l =
    let tok = Lexer.pick l in
    unbalanced ~ctx:g.name tok
  in
  (rule, 0)

let infix_binary power tok f =
  let lbp = abs power in
  let rbp = if power < 0 then lbp - 1 else lbp in
  let rule left g l =
    let* () = consume tok l in
    let* right = parse ~power:rbp g l in
    Ok (f left right)
  in
  (rule, lbp)

let infix_or_postfix power tok ~infix:mk_infix ~postfix:mk_postfix =
  let lbp = abs power in
  let rbp = if power < 0 then lbp - 1 else lbp in
  let rule left g l =
    let* () = consume tok l in
    let tok_right = Lexer.pick l in
    if Grammar.has_infix tok_right g then Ok (mk_postfix left)
    else
      let* right = parse ~power:rbp g l in
      Ok (mk_infix left right)
  in
  (rule, lbp)

let prefix_unary ?power tok f =
  let rule g l =
    let* () = consume tok l in
    let* x = parse ?power g l in
    Ok (f x)
  in
  rule

let const x =
  let rule _g l =
    let tok = Lexer.pick l in
    let* () = consume tok l in
    Ok x
  in
  rule

let postfix_unary power tok f =
  let rule left _g l =
    let* () = consume tok l in
    Ok (f left)
  in
  (rule, power)

let prefix_invalid =
  let rule tok g _l = invalid_prefix ~ctx:g.name tok in
  rule

let infix_invalid tok =
  let rule _left g _l = invalid_infix ~ctx:g.name tok in
  (rule, 0)

let between tok1 tok2 f =
  let prefix g l =
    let* () = consume tok1 l in
    let* x = parse g l in
    let* () = consume tok2 l in
    Ok (f x)
  in
  prefix

let prefix_scope tok1 tok2 f =
  let rule g l =
    let* () = consume tok1 l in
    if Lexer.pick l = tok2 then
      let* () = consume tok2 l in
      Ok (f None)
    else
      let* x = parse g l in
      let* () = consume tok2 l in
      Ok (f (Some x))
  in
  rule

let parse_infix_seq ~sep:is_sep ~power f left g l =
  let* xs =
    until is_sep
      (fun l ->
        Lexer.move l;
        parse ~power g l)
      l
  in
  Ok (f (left :: xs))

let infix_seq ~sep:is_sep ~power f =
  let rule left g l =
    let* xs =
      until is_sep
        (fun l ->
          Lexer.move l;
          parse ~power g l)
        l
    in
    Ok (f (left :: xs))
  in
  (rule, power)

let postfix_seq ~sep:(sep, power) f =
  let rule left g l =
    let* () = consume sep l in
    let* xs =
      until
        (fun tok -> Grammar.has_prefix tok g)
        (fun l ->
          let* x = parse ~power g l in
          let* () = consume sep l in
          Ok x)
        l
    in
    Ok (f (left :: xs))
  in
  (rule, power)

(* Infix seq with optional sep terminator *)
let infix_seq_opt ~sep:(sep, power) f =
  let rule left g l =
    let* () = consume sep l in
    let rec p l =
      let tok = Lexer.pick l in
      if Grammar.has_prefix tok g then
        let* x = parse ~power g l in
        let tok = Lexer.pick l in
        if Token.eq tok sep then
          let* () = consume sep l in
          let* xs = p l in
          Ok (x :: xs)
        else Ok [ x ]
      else Ok []
    in
    let* xs = p l in
    Ok (f (left :: xs))
  in
  (rule, power)

let prefix_seq ~sep ~power f g l =
  Lexer.move l;
  let* left = parse ~power g l in
  parse_infix_seq ~sep ~power f left g l

let eof = ((fun left _g _l -> Ok left), 0)
