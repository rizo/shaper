type space = Left | Right | Same

type t =
  | Int of int
  | String of string
  | Id of string
  | Delim of string
  | Op of string
  | Eof

let pp =
  let pf = Format.fprintf in
  fun f token ->
    match token with
    | Id x -> pf f "%s" x
    | Delim x -> pf f "%s" x
    | String x -> pf f "%S" x
    | Int x -> pf f "%d" x
    | Op x -> pf f "%s" x
    | Eof -> pf f "(eof)"

let eq t1 t2 = Stdlib.( = ) t1 t2
let cmp = Stdlib.compare

let is_eof = function
  | Eof -> true
  | _ -> false
