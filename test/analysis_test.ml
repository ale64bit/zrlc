open OUnit2
open Rpdl
open Rpdl.Monad

(* Helpers *)

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

let analysis_ok_test src root_env root_elems _ =
  analysis_test src (Ok {root_env; root_elems})

let analysis_error_test src want _ = analysis_test src (Error want)

let loc lnum bol cnum =
  Lexing.{pos_fname= "test"; pos_lnum= lnum; pos_bol= bol; pos_cnum= cnum}

(* Tests *)

let test_empty =
  let src = "" in
  let want_env = Env.global in
  let want_ast = [] in
  "test_empty" >:: analysis_ok_test src want_env want_ast

let test_const_bool_true =
  let src = "const b = true" in
  let stmt_loc = (loc 1 0 0, loc 1 0 14) in
  let val_loc = (loc 1 0 10, loc 1 0 14) in
  let const_expr_true = Located.{loc= val_loc; value= Ast.BoolLiteral true} in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "b"; cd_value= (Type.Primitive Bool, const_expr_true)}
  in
  let want_env =
    Env.(global |> add_constant "b" {loc= stmt_loc; value= Env.Bool true})
  in
  let want_ast = [cd] in
  "test_const_bool_true" >:: analysis_ok_test src want_env want_ast

let test_const_bool_false =
  let src = "const b = false" in
  let stmt_loc = (loc 1 0 0, loc 1 0 15) in
  let val_loc = (loc 1 0 10, loc 1 0 15) in
  let const_expr_false =
    Located.{loc= val_loc; value= Ast.BoolLiteral false}
  in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "b"; cd_value= (Type.Primitive Bool, const_expr_false)}
  in
  let want_env =
    Env.(global |> add_constant "b" {loc= stmt_loc; value= Env.Bool false})
  in
  let want_ast = [cd] in
  "test_const_bool_false" >:: analysis_ok_test src want_env want_ast

let test_const_int =
  let src = "const i = 42" in
  let stmt_loc = (loc 1 0 0, loc 1 0 12) in
  let val_loc = (loc 1 0 10, loc 1 0 12) in
  let const_expr_42 = Located.{loc= val_loc; value= Ast.IntLiteral 42} in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "i"; cd_value= (Type.Primitive Int, const_expr_42)}
  in
  let want_env =
    Env.(global |> add_constant "i" {loc= stmt_loc; value= Env.Int 42})
  in
  let want_ast = [cd] in
  "test_const_int" >:: analysis_ok_test src want_env want_ast

let test_const_float =
  let src = "const pi = 3.14" in
  let stmt_loc = (loc 1 0 0, loc 1 0 15) in
  let val_loc = (loc 1 0 11, loc 1 0 15) in
  let const_expr_pi = Located.{loc= val_loc; value= Ast.FloatLiteral 3.14} in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "pi"; cd_value= (Type.Primitive Float, const_expr_pi)}
  in
  let want_env =
    Env.(global |> add_constant "pi" {loc= stmt_loc; value= Env.Float 3.14})
  in
  let want_ast = [cd] in
  "test_const_float" >:: analysis_ok_test src want_env want_ast

let test_empty_pipeline =
  let src = "pipeline P(x: int): float {}" in
  let stmt_loc = (loc 1 0 0, loc 1 0 28) in
  let local_env =
    Env.(
      global
      |> add_var "x" {loc= stmt_loc; value= Type.TypeRef "int"}
      |> enter_pipeline_scope "P")
  in
  let args = [Type.{name= "x"; t= Type.TypeRef "int"}] in
  let rets = [Type.TypeRef "float"] in
  let pipeline_type = Type.Function (args, rets) in
  let pd =
    TypedAst.PipelineDecl
      { pd_env= local_env
      ; pd_name= "P"
      ; pd_type= pipeline_type
      ; pd_functions= [] }
  in
  let want_env =
    Env.(global |> add_pipeline "P" {loc= stmt_loc; value= pipeline_type})
  in
  let want_ast = [pd] in
  "test_empty_pipeline" >:: analysis_ok_test src want_env want_ast

let test_const_redefined =
  let src = "const i = 1\nconst i = 2" in
  let want_loc = (loc 2 12 12, loc 2 12 23) in
  let want_err = Located.{loc= want_loc; value= `Redefinition "i"} in
  "test_const_redefined" >:: analysis_error_test src want_err

let test_duplicate_member =
  let src = "type T {\n  f: float\n  f: int\n}" in
  let want_loc = (loc 1 0 0, loc 4 29 30) in
  let want_err = Located.{loc= want_loc; value= `DuplicateMember "f"} in
  "test_duplicate_member" >:: analysis_error_test src want_err

let test_unknown_type_name =
  let src = "type T {\n  x: X\n}" in
  let want_loc = (loc 1 0 0, loc 3 16 17) in
  let want_err = Located.{loc= want_loc; value= `UnknownTypeName "X"} in
  "test_unknown_type_name" >:: analysis_error_test src want_err

let test_pipeline_redefined =
  let src = "pipeline P() {}\npipeline P() {}" in
  let want_loc = (loc 2 16 16, loc 2 16 31) in
  let want_err = Located.{loc= want_loc; value= `Redefinition "P"} in
  "test_pipeline_redefined" >:: analysis_error_test src want_err

let test_pipeline_param_redefined =
  let src = "pipeline P(x: int, x: float) {}" in
  let want_loc = (loc 1 0 0, loc 1 0 31) in
  let want_err = Located.{loc= want_loc; value= `DuplicateParameter "x"} in
  "test_pipeline_param_redefined" >:: analysis_error_test src want_err

let test_pipeline_function_redefined =
  let src = "pipeline P() {\n  def f() {}\n  def f() {}\n}" in
  let want_loc = (loc 3 28 30, loc 3 28 40) in
  let want_err = Located.{loc= want_loc; value= `Redefinition "f"} in
  "test_pipeline_function_redefined" >:: analysis_error_test src want_err

(*   TODO: add test for AssignmentMismatch *)

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
