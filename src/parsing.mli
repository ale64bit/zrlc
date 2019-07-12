type error = [ `LexerError of string | `ParserError of string ]

val parse : Lexing.lexbuf -> (Ast.root, [> error ] Located.t) result
