type error = [`LexerError of string | `ParserError of string]

let parse lexbuf =
  try Ok (Parser.program Lexer.read lexbuf) with
  | Lexer.Error msg ->
      let loc = (lexbuf.lex_curr_p, lexbuf.lex_curr_p) in
      Error Located.{loc; value= `LexerError msg}
  | Parser.Error ->
      let loc = (lexbuf.lex_curr_p, lexbuf.lex_curr_p) in
      Error Located.{loc; value= `ParserError "parsing error"}
