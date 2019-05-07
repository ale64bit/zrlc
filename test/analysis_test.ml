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
  analysis_test src (Ok {root_env; root_module= "test"; root_elems})

let analysis_error_test src want _ = analysis_test src (Error want)

let loc lnum bol cnum =
  Lexing.{pos_fname= "test"; pos_lnum= lnum; pos_bol= bol; pos_cnum= cnum}

let search s p n =
  let re = Str.regexp_string p in
  let rec aux start k =
    let index = Str.search_forward re s start in
    if k = 0 then index else aux (index + 1) (k - 1)
  in
  aux 0 n

let loc_of ?(n = 0) s p =
  let rec aux s from last (lnum, bol) =
    match String.index_from_opt s from '\n' with
    | Some i ->
        if i >= last then (lnum, bol)
        else aux s (i + 1) last (lnum + 1, bol + i - from + 1)
    | None ->
        (lnum, bol)
  in
  let first_cnum = search s p n in
  let last_cnum = first_cnum + String.length p in
  let first_lnum, first_bol = aux s 0 first_cnum (1, 0) in
  let last_lnum, last_bol = aux s 0 last_cnum (1, 0) in
  (loc first_lnum first_bol first_cnum, loc last_lnum last_bol last_cnum)

(* Tests *)

let test_empty =
  let src = "module test" in
  let want_env = Env.global in
  let want_ast = [] in
  "test_empty" >:: analysis_ok_test src want_env want_ast

let test_const_bool_true =
  let src = "module test\nconst b = true" in
  let stmt_loc = loc_of src "const b = true" in
  let val_loc = loc_of src "true" in
  let const_expr_true = Located.{loc= val_loc; value= Ast.BoolLiteral true} in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "b"; cd_value= (Type.TypeRef "bool", const_expr_true)}
  in
  let want_env =
    Env.(global |> add_constant "b" {loc= stmt_loc; value= Env.Bool true})
  in
  let want_ast = [cd] in
  "test_const_bool_true" >:: analysis_ok_test src want_env want_ast

let test_const_bool_false =
  let src = "module test\nconst b = false" in
  let stmt_loc = loc_of src "const b = false" in
  let val_loc = loc_of src "false" in
  let const_expr_false =
    Located.{loc= val_loc; value= Ast.BoolLiteral false}
  in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "b"; cd_value= (Type.TypeRef "bool", const_expr_false)}
  in
  let want_env =
    Env.(global |> add_constant "b" {loc= stmt_loc; value= Env.Bool false})
  in
  let want_ast = [cd] in
  "test_const_bool_false" >:: analysis_ok_test src want_env want_ast

let test_const_int =
  let src = "module test\nconst i = 42" in
  let stmt_loc = loc_of src "const i = 42" in
  let val_loc = loc_of src "42" in
  let const_expr_42 = Located.{loc= val_loc; value= Ast.IntLiteral 42} in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "i"; cd_value= (Type.TypeRef "int", const_expr_42)}
  in
  let want_env =
    Env.(global |> add_constant "i" {loc= stmt_loc; value= Env.Int 42})
  in
  let want_ast = [cd] in
  "test_const_int" >:: analysis_ok_test src want_env want_ast

let test_const_float =
  let src = "module test\nconst pi = 3.14" in
  let stmt_loc = loc_of src "const pi = 3.14" in
  let val_loc = loc_of src "3.14" in
  let const_expr_pi = Located.{loc= val_loc; value= Ast.FloatLiteral 3.14} in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "pi"; cd_value= (Type.TypeRef "float", const_expr_pi)}
  in
  let want_env =
    Env.(global |> add_constant "pi" {loc= stmt_loc; value= Env.Float 3.14})
  in
  let want_ast = [cd] in
  "test_const_float" >:: analysis_ok_test src want_env want_ast

let test_empty_pipeline =
  let src = "module test\npipeline P(x: int): float {}" in
  let stmt_loc = loc_of src "pipeline P(x: int): float {}" in
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
  let src = "module test\nconst i = 1\nconst i = 2" in
  let want_loc = loc_of src "const i = 2" in
  let prev_loc = loc_of src "const i = 1" in
  let want_err =
    Located.{loc= want_loc; value= `Redefinition ("i", prev_loc)}
  in
  "test_const_redefined" >:: analysis_error_test src want_err

let test_duplicate_member =
  let src = "module test\ntype T {\n  f: float\n  f: int\n}" in
  let want_loc = loc_of src "type T {\n  f: float\n  f: int\n}" in
  let want_err = Located.{loc= want_loc; value= `DuplicateMember "f"} in
  "test_duplicate_member" >:: analysis_error_test src want_err

let test_unknown_type_name =
  let src = "module test\ntype T {\n  x: X\n}" in
  let want_loc = loc_of src "type T {\n  x: X\n}" in
  let want_err = Located.{loc= want_loc; value= `UnknownTypeName "X"} in
  "test_unknown_type_name" >:: analysis_error_test src want_err

let test_pipeline_redefined =
  let src = "module test\npipeline P() {}\npipeline P() {}" in
  let want_loc = loc_of ~n:1 src "pipeline P() {}" in
  let prev_loc = loc_of ~n:0 src "pipeline P() {}" in
  let want_err =
    Located.{loc= want_loc; value= `Redefinition ("P", prev_loc)}
  in
  "test_pipeline_redefined" >:: analysis_error_test src want_err

let test_pipeline_param_redefined =
  let src = "module test\npipeline P(x: int, x: float) {}" in
  let want_loc = loc_of src "pipeline P(x: int, x: float) {}" in
  let want_err = Located.{loc= want_loc; value= `DuplicateParameter "x"} in
  "test_pipeline_param_redefined" >:: analysis_error_test src want_err

let test_pipeline_function_redefined =
  let src = "module test\npipeline P() {\n  def f() {}\n  def f() {}\n}" in
  let want_loc = loc_of src ~n:1 "def f() {}" in
  let prev_loc = loc_of src ~n:0 "def f() {}" in
  let want_err =
    Located.{loc= want_loc; value= `Redefinition ("f", prev_loc)}
  in
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
