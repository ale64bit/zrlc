type t = {
  name : string;
  loads : (string * string) list;
  cc_libraries : Cpp.Library.t list;
  glsl_libraries : Glsl.Library.t list;
}

let empty name = { name; loads = []; cc_libraries = []; glsl_libraries = [] }

let load bzl name t = { t with loads = (bzl, name) :: t.loads }

let add_cc_library lib t = { t with cc_libraries = lib :: t.cc_libraries }

let add_glsl_library lib t =
  { t with glsl_libraries = lib :: t.glsl_libraries }

let write_file fname contents =
  let out = open_out fname in
  let () = Printf.fprintf out "%s" contents in
  close_out out

let write_cc_library_files path libraries =
  let open Cpp in
  List.iter
    (fun lib ->
      let classes = Library.classes lib in
      let headers = Library.headers lib in
      let () =
        List.iter
          (fun c ->
            let open Class in
            let hdr_file = path ^ "/" ^ name c ^ ".h" in
            let src_file = path ^ "/" ^ name c ^ ".cc" in
            let () = write_file hdr_file (string_of_header c) in
            write_file src_file (string_of_source c))
          classes
      in
      List.iter
        (fun h ->
          let open Header in
          let hdr_file = path ^ "/" ^ name h ^ ".h" in
          write_file hdr_file (string_of_header h))
        headers)
    libraries

let write_glsl_library_files path libraries =
  let open Glsl in
  List.iter
    (fun lib ->
      let shaders = Library.shaders lib in
      List.iter
        (fun shader ->
          let open Shader in
          let stage_ext =
            match stage shader with
            | Vertex -> ".vert"
            | Geometry -> ".geom"
            | Fragment -> ".frag"
            | Compute -> ".comp"
          in
          let fname = path ^ "/" ^ name shader ^ stage_ext ^ ".glsl" in
          write_file fname (string_of_source shader))
        shaders)
    libraries

let write_to path t =
  let () = write_cc_library_files path t.cc_libraries in
  let () = write_glsl_library_files path t.glsl_libraries in
  let fname = path ^ "/" ^ t.name in
  let loads =
    String.concat "\n"
      (List.map
         (fun (bzl, name) -> Printf.sprintf {|load("%s", "%s")|} bzl name)
         (List.rev t.loads))
  in
  let visibility = {|package(default_visibility = ["//visibility:public"])|} in
  let cc_libs =
    String.concat "\n"
      (List.map Cpp.Library.string_of_library (List.rev t.cc_libraries))
  in
  let glsl_libs =
    String.concat "\n"
      (List.map Glsl.Library.string_of_library (List.rev t.glsl_libraries))
  in
  let contents =
    Printf.sprintf "%s\n%s\n%s\n%s" loads visibility cc_libs glsl_libs
  in
  write_file fname contents
