open OUnit2
open Rpdl
open Rpdl.Monad

let typed_ast_printer = function
  | Ok ast -> "Ok (" ^ (TypedAst.string_of_ast ast) ^ ")"
  | Error e -> "Error (" ^ (Error.debug_string_of_error e) ^ ")"

let analysis_test src want =
  let lexbuf = Lexing.from_string src in
  let got = Parsing.parse lexbuf >>= fun ast -> Analysis.check ast in
  assert_equal ~printer:typed_ast_printer want got

let analysis_ok_test src want _ = analysis_test src (Ok want)

let analysis_error_test src want _ = analysis_test src (Error want)

let suite = "Test Analysis" >::: [
  "test_empty" >:: analysis_ok_test 
    "" 
    (Env.global, []);

  "test_const_bool_true" >:: analysis_ok_test 
    "const b = true" 
    (Env.add_constant "b" (Env.Bool true) (Env.global), []);

  "test_const_bool_false" >:: analysis_ok_test 
    "const b = false" 
    (Env.add_constant "b" (Env.Bool false) (Env.global), []);

  "test_const_int" >:: analysis_ok_test 
    "const i = 42" 
    (Env.add_constant "i" (Env.Int 42) (Env.global), []);

  "test_const_float" >:: analysis_ok_test 
    "const pi = 3.14" 
    (Env.add_constant "pi" (Env.Float 3.14) (Env.global), []);

  "test_empty_pipeline" >:: analysis_ok_test
    "pipeline P(x: int): float {}"
    (Env.global, [
      TypedAst.PipelineDecl (
        Env.global,
        {
          pd_name = "P";
          pd_type = Type.Function (
            [{name = "x"; t = Type.TypeRef "int"}], 
            [Type.TypeRef "float"]);
          pd_functions = [];
        }
      );
    ]);

  "test_const_redefined" >:: analysis_error_test 
    "const i = 1
    const i = 2" 
    (`Redefinition "i");

  "test_duplicate_member" >:: analysis_error_test 
    "type T {
      f: float
      f: int
    }"
    (`DuplicateMember ("T", "f"));

  "test_unknown_type_name" >:: analysis_error_test 
    "type T {
      x: X
    }"
    (`UnknownTypeName "X");

  "test_pipeline_redefined" >:: analysis_error_test
    "pipeline P() {}
    pipeline P() {}"
    (`Redefinition "P");

  "test_pipeline_param_redefined" >:: analysis_error_test
    "pipeline P(x: int, x: float) {}"
    (`DuplicateParameter "x");

  "test_pipeline_function_redefined" >:: analysis_error_test
    "pipeline P() {
      def f() {} 
      def f() {} 
    }"
    (`Redefinition "f");

]

let _ = run_test_tt_main suite

