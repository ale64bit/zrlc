module Keywords = Set.Make(String)

let keywords = Keywords.of_list [
  "array";
  "builtin";
  "const";
  "def";
  "else";
  "for";
  "if";
  "in";
  "pipeline";
  "renderer";
  "return";
  "to";
  "type";
  "var";
] 

let rec lex = parser
  | [< ' (' ' | '\n' | '\r' | '\t'); stream >] -> lex stream
  | [< ' ('a' .. 'z' | 'A' .. 'Z' as c); stream >] -> 
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c;
      lex_id buffer stream
  | [< ' ('0' .. '9' as c); stream >] -> 
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c;
      lex_number buffer stream
  | [< 'c when (Parser.is_operator (Printf.sprintf "%c" c)); stream >] ->
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c ;
      lex_op buffer stream
  | [< >] -> [< >]

and lex_id buffer = parser
  | [< ' ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' as c); stream >] -> 
      Buffer.add_char buffer c;
      lex_id buffer stream
  | [< stream=lex >] ->
      let t = match Buffer.contents buffer with
        | kw when Keywords.exists ( (=) kw ) keywords -> Token.Keyword kw
        | "true" -> Token.Bool true
        | "false" -> Token.Bool false
        | id -> Token.Id id in
      [< 't; stream >]

and lex_number buffer ?(is_float=false) = parser
  | [< ' ('.' as c) when not is_float; stream >] ->
      Buffer.add_char buffer c;
      lex_number buffer ~is_float:true stream
  | [< ' ('0' .. '9' as c); stream >] ->
      Buffer.add_char buffer c;
      lex_number buffer ~is_float stream
  | [< stream=lex >] ->
      if is_float then
        [< 'Token.Float (float_of_string (Buffer.contents buffer)); stream >]
      else
        [< 'Token.Int (int_of_string (Buffer.contents buffer)); stream >]

and lex_op buffer = parser
  | [< 'c when Parser.is_operator (Printf.sprintf "%s%c" (Buffer.contents buffer) c); stream >] ->
      Buffer.add_char buffer c ;
      lex_op buffer stream
  | [< stream=lex >] -> [< 'Token.Operator (Buffer.contents buffer); stream >]

