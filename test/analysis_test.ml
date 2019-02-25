open OUnit2
open Rpdl
open Rpdl.Monad

let analysis_test src want _ =
  let lexbuf = Lexing.from_string src in
  let got = Parsing.parse lexbuf >>= fun ast -> Analysis.check ast in
  assert_equal got (Ok want)

let suite = "Test Analysis" >::: [
  "test_empty" >:: analysis_test 
    "" 
    (Env.empty "global", []);

  "test_const_bool_true" >:: analysis_test 
    "const b = true" 
    (Env.add_constant "b" (Env.Bool true) (Env.empty "global"), []);

  "test_const_bool_false" >:: analysis_test 
    "const b = false" 
    (Env.add_constant "b" (Env.Bool false) (Env.empty "global"), []);

  "test_const_int" >:: analysis_test 
    "const i = 42" 
    (Env.add_constant "i" (Env.Int 42) (Env.empty "global"), []);

  "test_const_float" >:: analysis_test 
    "const pi = 3.14" 
    (Env.add_constant "pi" (Env.Float 3.14) (Env.empty "global"), []);
]

let _ = run_test_tt_main suite

