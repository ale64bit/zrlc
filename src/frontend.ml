(**
   Copyright 2019 Google LLC

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

open Lexing
open Monad.Result

let gen_dot ast = function
  | Some output_file ->
      let _ =
        let ch = open_out_bin output_file in
        Gendot.dot ch ast
      in
      ()
  | _ -> ()

let process Config.{ cfg_source_file; cfg_output_ast_file; _ } =
  let ch = open_in cfg_source_file in
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = cfg_source_file };
  Parsing.parse lexbuf >>= fun ast ->
  let _ = gen_dot ast cfg_output_ast_file in
  Analysis.check ast
