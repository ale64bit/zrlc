open Rpdl

let verbose = ref false
let input = ref ""
let ast_file = ref ""

let die code msg = 
  Printf.fprintf stderr msg ;
  exit code 

let check_file file =
  if not (Sys.file_exists file)
  then (
    Printf.fprintf stderr "error: no such file: %s\n" file ;
    exit 1
  )

let string_of_error = function
  | `LexerError (pos, msg) ->
      Printf.sprintf "%s: lexer error: %s" (Lexer.string_of_position pos) msg
  | `ParserError (pos, msg) ->
      Printf.sprintf "%s: parser error: %s" (Lexer.string_of_position pos) msg
  | `SemanticError msg ->
      Printf.sprintf "???: semantic error: %s" msg
  | `TypeError msg ->
      Printf.sprintf "???: type error: %s" msg

let () =
  let spec = [
    ("-v", Arg.Set verbose, "Enable verbose mode");
    ("-i", Arg.Set_string input, "Input file name");
    ("-ast_file", Arg.Set_string ast_file, "Output AST in dot format");
  ] in
  let usage_msg = "RPDL compiler" in
  let input_files = ref [] in
  Arg.parse spec (fun arg -> input_files := arg::(!input_files)) usage_msg ;
  input_files := List.rev (!input_files) ;
  List.iter check_file (!input_files) ;

  print_endline "------ build started ------" ;
  print_endline ("Input file: " ^ !input) ;

  let cfg = {
    Frontend.source_file = !input;
    verbose = !verbose;
    output_ast_file = !ast_file;
  } in
  match Frontend.process cfg with
    | Ok _ ->
        print_endline "====== build succeeded ======" ;
    | Error err -> 
        print_endline (string_of_error err) ;
        print_endline "====== build failed ======" ;
        exit 1
