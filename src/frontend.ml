open Lexing
open Monad.Result

let gen_dot ast = function
  | Some output_file ->
      let _ =
        let ch = open_out_bin output_file in
        Gendot.dot ch ast
      in
      ()
  | _ ->
      ()

let process Config.{cfg_source_file; cfg_output_ast_file; _} =
  let ch = open_in cfg_source_file in
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= cfg_source_file} ;
  Parsing.parse lexbuf >>= fun ast ->
  let _ = gen_dot ast cfg_output_ast_file in
  Analysis.check ast
