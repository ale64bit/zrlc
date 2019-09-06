(**
   Copyright 2019 Google LLC

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

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

let join_path xs = String.concat "/" xs

let create_dir_if_needed dir =
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755 else ()

let write_file fname contents =
  let out = open_out_bin fname in
  let () = Printf.fprintf out "%s" contents in
  close_out out

let write_cc_library_files include_dir src_dir libraries =
  let open Cpp in
  List.iter
    (fun lib ->
      let classes = Library.classes lib in
      let headers = Library.headers lib in
      let () =
        List.iter
          (fun c ->
            let open Class in
            let hdr_file = join_path [ include_dir; name c ^ ".h" ] in
            let src_file = join_path [ src_dir; name c ^ ".cc" ] in
            write_file hdr_file (string_of_header c);
            write_file src_file (string_of_source c))
          classes
      in
      List.iter
        (fun h ->
          let open Header in
          let hdr_file = join_path [ include_dir; name h ^ ".h" ] in
          write_file hdr_file (string_of_header h))
        headers)
    libraries

let write_glsl_library_files shader_dir libraries =
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
          let fname =
            join_path [ shader_dir; name shader ^ stage_ext ^ ".glsl" ]
          in
          write_file fname (string_of_source shader))
        shaders)
    libraries

let write_to dir t =
  let shader_dir = join_path [ dir; "shader" ] in
  let include_dir = join_path [ dir; "include" ] in
  let src_dir = join_path [ dir; "src" ] in
  create_dir_if_needed dir;
  create_dir_if_needed shader_dir;
  create_dir_if_needed include_dir;
  create_dir_if_needed src_dir;
  write_cc_library_files include_dir src_dir t.cc_libraries;
  write_glsl_library_files shader_dir t.glsl_libraries
