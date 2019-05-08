open Lexing
open Monad.Result

type config = {source_file: string; verbose: bool; output_ast_file: string}

let gen_dot cfg ast =
  let _ =
    if cfg.output_ast_file <> "" then
      let ch = open_out_bin cfg.output_ast_file in
      Gendot.dot ch ast
  in
  Ok ast

let process cfg =
  let ch = open_in cfg.source_file in
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= cfg.source_file} ;
  Parsing.parse lexbuf >>= fun ast ->
  gen_dot cfg ast >>= fun ast ->
  Analysis.check ast >>= fun {root_env; root_module; root_elems} ->
  Ok (root_env, root_module, root_elems)
