open OUnit2;;
open Rpdl;;

let lex src =
  Util.drain (Lexer.lex (Stream.of_string src))

let lex_test src want _ = 
  assert_equal ~printer:Util.string_of_token_stream want (lex src)

let token_tests = [
  "empty" >:: (lex_test "" []);
  "id" >:: (lex_test "ab" [Token.Id "ab"]);
  "int" >:: (lex_test "1234" [Token.Int 1234]);
  "float" >:: (lex_test "12.34" [Token.Float 12.34]);
  "true" >:: (lex_test "true" [Token.Bool true]);
  "false" >:: (lex_test "false" [Token.Bool false]);
  "const_def" >:: (lex_test "const kLightCount = 128"
    [Token.Keyword "const"; 
     Token.Id "kLightCount"; 
     Token.Operator "="; 
     Token.Int 128]);
]

let keyword_tests = List.map 
  (fun kw -> ("keyword_" ^ kw) >:: (lex_test kw [Token.Keyword kw]))
  (Lexer.Keywords.elements Lexer.keywords)

let operator_tests = List.map 
  (fun op -> ("operator_" ^ op) >:: (lex_test op [Token.Operator op]))
  (Parser.Operators.fold (fun op _ acc -> op :: acc) Parser.operators [])

let suite = 
  "lexer_suite">:::
    (List.concat [token_tests; keyword_tests; operator_tests])

let () = 
  run_test_tt_main suite
