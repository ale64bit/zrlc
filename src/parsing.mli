type error =
  [ `LexerError of Lexing.position * string
  | `ParserError of Lexing.position * string ]

val parse : Lexing.lexbuf -> (Ast.root, [> error]) result
