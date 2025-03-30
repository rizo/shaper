type space = Left | Right | Same

type t =
  | Int of int
  | String of string
  | Id of string
  | Delim of string
  | Op of string
  | Eof

val pp : Format.formatter -> t -> unit
val cmp : t -> t -> int
val eq : t -> t -> bool
val is_eof : t -> bool
