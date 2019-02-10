let verbose = ref false
let input = ref ""
let ast_file = ref ""

let die code msg = 
  Printf.fprintf stderr msg ;
  exit code 

let () =
  let spec = [
    ("-v", Arg.Set verbose, "Enable verbose mode");
    ("-i", Arg.Set_string input, "Input file name");
    ("-ast_file", Arg.Set_string ast_file, "Output AST in dot format");
  ] in
  let usage_msg = "RPDL compiler" in
  Arg.parse spec print_endline usage_msg ;

  print_endline "------ build started ------" ;
  print_endline ("Input file: " ^ !input) ;

  match Rpdl.compile !input ~ast_file:!ast_file with
    | Ok _ -> 
        print_endline "====== build succeeded ======" ;
    | Error msg -> 
        print_endline msg ;
        print_endline "====== build failed ======" ;
        exit 1
