type t
(* A stateful lexer of {!type:Token.t} values. *)

val read_string : string -> t
(* Read tokens from a string. *)

val read_channel : ?file_name:string -> in_channel -> t
(* Read tokens from a channel. *)

val line_number : t -> int
(* Current line number. *)

val pick : t -> Token.t
(* Pick at the next token without consuming it. *)

val next : t -> Token.t
(* Get the next token and consume it. *)

val move : t -> unit
(* Consume the current token. *)

type loc = { l_start : int; c_start : int; l_end : int; c_end : int }
(* Location within the file as line/column range. *)

val loc : t -> loc
(* The location of the current token. *)

val pp_loc : Format.formatter -> loc -> unit
(* Print the loc value. *)
