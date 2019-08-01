{
  open Lexing
  open Parser

  exception Error of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

let sign = ['-' '+']
let int = ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] sign? digit+
let float = digit+ frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white      { read lexbuf }
  | "//" [^ '\r' '\n']* newline
  | newline    { next_line lexbuf; read lexbuf }
  | "cast"     { CAST }
  | "const"    { CONST }
  | "def"      { DEF }
  | "discard"  { DISCARD }
  | "else"     { ELSE }
  | "false"    { BOOL false }
  | "for"      { FOR }
  | "if"       { IF }
  | "in"       { IN }
  | "module"   { MODULE }
  | "pipeline" { PIPELINE }
  | "renderer" { RENDERER }
  | "return"   { RETURN }
  | "to"       { TO }
  | "true"     { BOOL true }
  | "type"     { TYPE }
  | "val"      { VAL }
  | "var"      { VAR }
  | int        { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float      { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | id         { ID (Lexing.lexeme lexbuf) }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | "["        { LBRACKET }
  | "]"        { RBRACKET }
  | "{"        { LBRACE }
  | "}"        { RBRACE }
  | ","        { COMMA }
  | "."        { DOT }
  | ":"        { COLON }
  | "!"        { LOGICAL_NOT }
  | "~"        { BITWISE_COMPLEMENT }
  | "*"        { MULT }
  | "/"        { DIV }
  | "%"        { MOD }
  | "+"        { PLUS }
  | "-"        { MINUS }
  | "<<"       { SHIFTLEFT }
  | ">>"       { SHIFTRIGHT}
  | "<"        { LT }
  | ">"        { GT }
  | "<="       { LE }
  | ">="       { GE }
  | "=="       { EQ }
  | "!="       { NEQ }
  | "&"        { BITWISE_AND }
  | "^"        { BITWISE_XOR }
  | "|"        { BITWISE_OR }
  | "&&"       { LOGICAL_AND }
  | "^^"       { LOGICAL_XOR }
  | "||"       { LOGICAL_OR }
  | "="        { ASSIGN }
  | "+="       { ASSIGN_PLUS }
  | "-="       { ASSIGN_MINUS }
  | "*="       { ASSIGN_MULT }
  | "/="       { ASSIGN_DIV }
  | "%="       { ASSIGN_MOD }
  | eof        { EOF }
  | _          { raise (Error ("unexpected char: " ^ Lexing.lexeme lexbuf)) }

and comment = 
  parse
  | newline    { next_line lexbuf; read lexbuf }
  | _          { comment lexbuf }
