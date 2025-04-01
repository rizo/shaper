module Token = Token
module Lexer = Lexer
module Parser = Parser
module Shape = Parser.Shape

let parse_channel ?file_name ic =
  let lexer = Lexer.read_channel ?file_name ic in
  Parser.parse lexer
