type error = [
  | `LexerError of Lexing.position * string
  | `ParserError of Lexing.position * string
]

let parse lexbuf = 
  try Ok (Parser.program Lexer.read lexbuf) with
  | Lexer.Error msg -> 
      Error (`LexerError (lexbuf.lex_curr_p, msg))
  | Parser.Error -> 
      Error (`ParserError (lexbuf.lex_curr_p, "TODO"))

