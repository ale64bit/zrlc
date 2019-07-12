type t = {
  cfg_source_file : string;
  cfg_verbose : bool;
  cfg_output_ast_file : string option;
  cfg_output_directory : string;
  cfg_bazel_package : string list;
}
