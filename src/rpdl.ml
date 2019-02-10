open Lexing

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let pass_parse lexbuf = 
  print_endline "Pass: parsing" ;
  try Ok (Parser.program Lexer.read lexbuf) with
  | Lexer.SyntaxError msg ->
      let msg = Printf.sprintf "%s: syntax error: %s\n" (print_position lexbuf) msg in
      Error msg
  | Parser.Error ->
      let msg = Printf.sprintf "%s: parser error\n" (print_position lexbuf) in
      Error msg

let pass_analysis ast = 
  print_endline "Pass: analysis" ;
  match ast with
  | Ok ast -> Analysis.pass ast
  | Error e -> Error e

let pass_gen_dot ast file =
  if file = "" then
    ast
  else
    match ast with
    | Ok ast -> 
      print_endline ("Pass: generating dot file: " ^ file) ;
      let out = open_out_bin file in
      Gendot.dot ast out ;
      Ok ast
    | Error e -> Error e

let compile file ~ast_file =
  if file = "" then Error "error: no input file"
  else
    let ch = open_in file in
    let lexbuf = Lexing.from_channel ch in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file } ;
    let res = pass_parse lexbuf in
    let res = pass_analysis res in
    let res = pass_gen_dot res ast_file in
    res

