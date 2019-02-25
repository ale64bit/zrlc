open OUnit2
open Rpdl

let ast_printer = function
  | Ok ast -> Ast.string_of_ast ast
  | Error _ -> "Error(TODO)"

let parsing_test src want _ =
  let lexbuf = Lexing.from_string src in
  let got = Parsing.parse lexbuf in
  assert_equal ~printer:ast_printer got (Ok want)

let suite = "Test Parsing" >::: [

  "test_empty" >:: parsing_test 
    ""
    [];

  "test_const_decl_bool_true" >:: parsing_test 
    "const b = true" 
    [Ast.ConstDecl{cd_name="b"; cd_value=Ast.BoolLiteral true}];

  "test_const_decl_bool_false" >:: parsing_test 
    "const b = false" 
    [Ast.ConstDecl{cd_name="b"; cd_value=Ast.BoolLiteral false}];

  "test_const_decl_int" >:: parsing_test 
    "const i = 1" 
    [Ast.ConstDecl{cd_name="i"; cd_value=Ast.IntLiteral 1}];

  "test_const_decl_float" >:: parsing_test 
    "const f = 1.2" 
    [Ast.ConstDecl{cd_name="f"; cd_value=Ast.FloatLiteral 1.2}];

  "test_type_decl" >:: parsing_test
    "type Foo { x:int y:[float,32,kNum] }"
    [Ast.TypeDecl {
      td_name="Foo";
      td_type=Type.Record [
        {name="x"; t=Type.TypeRef "int"};
        {name="y"; t=Type.Array (Type.TypeRef "float", [
          Type.OfInt 32;
          Type.OfName "kNum";
        ])};
      ];
    }];

  "test_pipeline_decl_empty" >:: parsing_test
    "pipeline MyPipeline() {}"
    [Ast.PipelineDecl {
      pd_name = "MyPipeline";
      pd_type = Type.Function ([], []);
      pd_functions = [];
    }];

] 

let _ = run_test_tt_main suite
