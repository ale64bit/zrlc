open OUnit2
open Rpdl
open Rpdl.Monad

let typed_ast_printer = function
  | Ok ast ->
      "Ok (" ^ TypedAst.string_of_ast ast ^ ")"
  | Error e ->
      "Error (" ^ Error.debug_string_of_error e ^ ")"

let analysis_test src want =
  let lexbuf = Lexing.from_string src in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= "test"} ;
  let got = Parsing.parse lexbuf >>= fun ast -> Analysis.check ast in
  assert_equal ~printer:typed_ast_printer want got

let analysis_ok_test src want _ = analysis_test src (Ok want)

let analysis_error_test src want _ = analysis_test src (Error want)

let loc lnum bol cnum =
  {Lexing.pos_fname= "test"; pos_lnum= lnum; pos_bol= bol; pos_cnum= cnum}

let test_empty =
  let src = "" in
  let want_env = Env.global in
  let want_ast = [] in
  "test_empty" >:: analysis_ok_test src (want_env, want_ast)

let test_const_bool_true =
  let src = "const b = true" in
  let bloc = (loc 1 0 10, loc 1 0 14) in
  let want_env =
    Env.add_constant "b" {loc= bloc; value= Env.Bool true} Env.global
  in
  let want_ast = [] in
  "test_const_bool_true" >:: analysis_ok_test src (want_env, want_ast)

let test_const_bool_false =
  let src = "const b = false" in
  let bloc = (loc 1 0 10, loc 1 0 15) in
  let want_env =
    Env.add_constant "b" {loc= bloc; value= Env.Bool false} Env.global
  in
  let want_ast = [] in
  "test_const_bool_false" >:: analysis_ok_test src (want_env, want_ast)

let test_const_int =
  let src = "const i = 42" in
  let iloc = (loc 1 0 10, loc 1 0 12) in
  let want_env =
    Env.add_constant "i" {loc= iloc; value= Env.Int 42} Env.global
  in
  let want_ast = [] in
  "test_const_int" >:: analysis_ok_test src (want_env, want_ast)

let test_const_float =
  let src = "const pi = 3.14" in
  let floc = (loc 1 0 11, loc 1 0 15) in
  let want_env =
    Env.add_constant "pi" {loc= floc; value= Env.Float 3.14} Env.global
  in
  let want_ast = [] in
  "test_const_float" >:: analysis_ok_test src (want_env, want_ast)

let test_empty_pipeline =
  let src = "pipeline P(x: int): float {}" in
  (* TODO: implement *)
  let want_env = Env.global in
  let want_ast = [] in
  "test_empty_pipeline" >:: analysis_ok_test src (want_env, want_ast)

let test_const_redefined =
  let src = "const i = 1\nconst i = 2" in
  let want_err = `Redefinition "i" in
  "test_const_redefined" >:: analysis_error_test src want_err

let test_duplicate_member =
  let src = "type T {\n  f: float\n  f: int\n}" in
  let want_err = `DuplicateMember ("T", "f") in
  "test_duplicate_member" >:: analysis_error_test src want_err

let test_unknown_type_name =
  let src = "type T {\n  x: X\n}" in
  let want_err = `UnknownTypeName "X" in
  "test_unknown_type_name" >:: analysis_error_test src want_err

let test_pipeline_redefined =
  let src = "pipeline P() {}\n    pipeline P() {}" in
  let want_err = `Redefinition "P" in
  "test_pipeline_redefined" >:: analysis_error_test src want_err

let test_pipeline_param_redefined =
  let src = "pipeline P(x: int, x: float) {}" in
  let want_err = `DuplicateParameter "x" in
  "test_pipeline_param_redefined" >:: analysis_error_test src want_err

let test_pipeline_function_redefined =
  let src = "pipeline P() {\n  def f() {}\n  def f() {}\n}" in
  let want_err = `Redefinition "f" in
  "test_pipeline_function_redefined" >:: analysis_error_test src want_err

let tests =
  "analysis_suite"
  >::: [ test_empty
       ; test_const_bool_true
       ; test_const_bool_false
       ; test_const_int
       ; test_const_float
       ; test_empty_pipeline
       ; test_const_redefined
       ; test_duplicate_member
       ; test_unknown_type_name
       ; test_pipeline_redefined
       ; test_pipeline_param_redefined
       ; test_pipeline_function_redefined ]

let _ = run_test_tt_main tests
