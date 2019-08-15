open Zrl

let verbose = ref false

let input = ref ""

let ast_file = ref ""

let output_dir = ref ""

let die code msg =
  Printf.fprintf stderr msg;
  exit code

let check_file file =
  if not (Sys.file_exists file) then (
    Printf.fprintf stderr "error: no such file: %s\n" file;
    exit 1 )

let () =
  let spec =
    [
      ("-v", Arg.Set verbose, "Enable verbose mode");
      ("-i", Arg.Set_string input, "Input file name");
      ("-ast_file", Arg.Set_string ast_file, "Output AST in dot format");
      ("-o", Arg.Set_string output_dir, "Output directory name");
    ]
  in
  let usage_msg = "ZRL compiler" in
  let input_files = ref [] in
  Arg.parse spec (fun arg -> input_files := arg :: !input_files) usage_msg;
  input_files := List.rev !input_files;
  List.iter check_file !input_files;
  print_endline "------ build started ------";
  print_endline ("Input file: " ^ !input);
  let cfg =
    Config.
      {
        cfg_source_file = !input;
        cfg_verbose = !verbose;
        cfg_output_ast_file = (if !ast_file <> "" then Some !ast_file else None);
        cfg_output_directory = !output_dir;
      }
  in
  match Frontend.process cfg with
  | Ok ast -> (
      let open Backend in
      match Vkdummy.gen cfg ast with
      | Ok () -> print_endline "====== build succeeded ======"
      | Error err ->
          print_endline (Vkdummy.Error.string_of_error err);
          print_endline "====== build failed ======";
          exit 1 )
  | Error err ->
      print_endline (Error.string_of_error err);
      print_endline "====== build failed ======";
      exit 1
