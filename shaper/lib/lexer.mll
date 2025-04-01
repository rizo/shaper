{
  type t = {
    lexbuf : Lexing.lexbuf;
    strbuf : Buffer.t;
    mutable token : Token.t;
    mutable line_start : int;
    mutable line_count : int;
  }

  let update_loc_ lexer =
    lexer.line_count <- lexer.line_count + 1;
    lexer.line_start <- lexer.lexbuf.lex_abs_pos + lexer.lexbuf.lex_curr_pos

  let update_loc lexbuf file line absolute chars =
    let pos = lexbuf.Lexing.lex_curr_p in
    let new_file = match file with
                   | None -> pos.pos_fname
                   | Some s -> s
    in
    lexbuf.Lexing.lex_curr_p <- { pos with
      pos_fname = new_file;
      pos_lnum = if absolute then line else pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - chars;
    }
}

(* Ident *)
let ident_lower_char = ['a'-'z' '_']
let ident_upper_char = ['A'-'Z']
let ident_inner_char = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let ident_upper = (ident_upper_char) ident_inner_char*
let ident_lower = (ident_lower_char) ident_inner_char*

let op_char =
  ['!' '$' '%' '#' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let op = op_char+

let space = [' ' '\t' '\r']+
let digit = ['0'-'9']
let nonzero = ['1'-'9']
let int = (digit | nonzero digit+)

rule read lexer = parse
  | "//"[^'\n']* { read lexer lexbuf }
  | '\\' { Token.Op ("\\") }
  | '{' { Token.Lbrace }
  | '}' { Token.Rbrace }
  | '(' { Token.Lparen }
  | ')' { Token.Rparen }
  | '[' { Token.Lbracket }
  | ']' { Token.Rbracket }
  | ',' { Token.Comma }
  | ';' { Token.Semi }
  (*| '`' { Delim "`" }*)
  (*| '\'' { Delim "'" }*)
  | '"' {
    Buffer.clear lexer.strbuf;
    String (finish_string lexer lexbuf)
  }
  | "\n" {
    update_loc lexbuf None 1 false 0;
    read lexer lexbuf
  }
  | (ident_lower as x) ':' { Token.Label x }
  | ident_lower { Token.Id (Lexing.lexeme lexbuf) }
  | ident_upper { Token.Id (Lexing.lexeme lexbuf) }
  | op { Token.Op (Lexing.lexeme lexbuf) }
  | int { Token.Int (int_of_string (Lexing.lexeme lexbuf)) }
  | space { read lexer lexbuf }
  | eof { Eof }
  | _ { Fmt.failwith "Invalid token: %s" (Lexing.lexeme lexbuf) }

and finish_string lexer = parse
  | '"'  { Buffer.contents lexer.strbuf }
  | [^ '"' '\\']+ {
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Buffer.add_subbytes lexer.strbuf lexbuf.lex_buffer lexbuf.lex_start_pos len;
    finish_string lexer lexbuf
  }
    | eof { failwith "Unexpected end of input when reading a string" }

{
  let read_lexbuf lexbuf =
    let lexer = {
      lexbuf;
      strbuf = Buffer.create 64;
      token = Eof;
      line_count = 1;
      line_start = 0;
    } in
    lexer.token <- read lexer lexbuf;
    lexer

  let read_string s =
    let lexbuf = Lexing.from_string s in
    read_lexbuf lexbuf

  let read_channel ?file_name ic =
    let lexbuf = Lexing.from_channel ic in
    (match file_name with Some f -> Lexing.set_filename lexbuf f | _ -> ());
    read_lexbuf lexbuf

  let next lexer =
    lexer.token <- read lexer lexer.lexbuf;
    lexer.token

  let move lexer =
    lexer.token <- read lexer lexer.lexbuf

  let pick lexer =
    lexer.token

  let line_number lexer =
    lexer.line_count
  
  type loc = { l_start : int; c_start : int; l_end : int; c_end : int }

  let loc {lexbuf;_} = {
    l_start = lexbuf.Lexing.lex_start_p.pos_lnum;
    c_start = lexbuf.Lexing.lex_start_p.pos_cnum;
    l_end = lexbuf.Lexing.lex_curr_p.pos_lnum;
    c_end = lexbuf.Lexing.lex_curr_p.pos_cnum;
  }
}
