open OUnit2;;
open Rpdl;;

let parser_test src want _ = 
  let input = Lexer.lex (Stream.of_string src) in
  let got = Parser.parse input in
  assert_equal want got

let suite =
  "parser_suite">:::
    ["const_int" >:: (parser_test 
       "const x = 1" 
       (Ast.Root {pipelines=[]; renderers=[]}));
     "const_float" >:: (parser_test
       "const y = 2.3" 
       (Ast.Root {pipelines=[]; renderers=[]}));
     "const_bool" >:: (parser_test 
       "const x = true" 
       (Ast.Root {pipelines=[]; renderers=[]}));
     "id_list" >:: (parser_test 
       "type Foo {
         x, y, z: int
         foo, bar: vec3
         a: array[vec4,i,8]
       }"
       (Ast.Root {pipelines=[]; renderers=[]}))]

let () = 
  run_test_tt_main suite
