module Function = struct
  type t = {
    name : string;
    returns : string;
    parameters : (string * string) list;
    body : string list;
  }

  let empty name = { name; returns = "void"; parameters = []; body = [] }

  let set_return_type returns t = { t with returns }

  let add_in_param (t, name) f =
    { f with parameters = ("in " ^ t, name) :: f.parameters }

  let add_out_param (t, name) f =
    { f with parameters = ("out " ^ t, name) :: f.parameters }

  let add_inout_param (t, name) f =
    { f with parameters = ("inout " ^ t, name) :: f.parameters }

  let append_code_section s f = { f with body = s :: f.body }

  let append_code_sections ss f =
    List.fold_left (fun f s -> append_code_section s f) f ss

  let prepend_code_section s f = { f with body = f.body @ [ s ] }

  let prepend_code_sections ss f =
    List.fold_left (fun f s -> prepend_code_section s f) f ss

  let string_of_implementation t =
    let params =
      String.concat ", "
        (List.map (fun (t, name) -> t ^ " " ^ name) (List.rev t.parameters))
    in
    let body = String.concat "\n" (List.rev t.body) in
    Printf.sprintf {|%s %s(%s) {
%s
}|} t.returns t.name params body
end

module Shader = struct
  type stage = Vertex | Geometry | Fragment | Compute

  type t = {
    name : string;
    stage : stage;
    constants : string list;
    structs : string list;
    uniforms : (int * int * (string * string)) list;
    inputs : (int * (string * string)) list;
    outputs : (int * (string * string)) list;
    functions : Function.t list;
  }

  let empty name stage =
    {
      name;
      stage;
      constants = [];
      structs = [];
      uniforms = [];
      inputs = [];
      outputs = [];
      functions = [];
    }

  let name t = t.name

  let stage t = t.stage

  let add_constant c t = { t with constants = c :: t.constants }

  let add_struct s t = { t with structs = s :: t.structs }

  let add_uniform set binding u t =
    { t with uniforms = (set, binding, u) :: t.uniforms }

  let add_input location i t = { t with inputs = (location, i) :: t.inputs }

  let add_output location o t = { t with outputs = (location, o) :: t.outputs }

  let add_function f t = { t with functions = f :: t.functions }

  let string_of_source t =
    let constants = String.concat "\n" t.constants in
    let structs =
      String.concat "\n" (List.map (fun s -> s ^ ";") (List.rev t.structs))
    in
    let uniforms =
      String.concat "\n"
        (List.map
           (fun (set, binding, (t, name)) ->
             Printf.sprintf "layout(set=%d, binding=%d) uniform %s %s;" set
               binding t name)
           t.uniforms)
    in
    let inputs =
      String.concat "\n"
        (List.map
           (fun (location, (t, name)) ->
             Printf.sprintf "layout(location=%d) in %s %s;" location t name)
           (List.rev t.inputs))
    in
    let outputs =
      String.concat "\n"
        (List.map
           (fun (location, (t, name)) ->
             Printf.sprintf "layout(location=%d) out %s %s;" location t name)
           (List.rev t.outputs))
    in
    let functions =
      String.concat "\n"
        (List.map Function.string_of_implementation (List.rev t.functions))
    in
    Printf.sprintf
      {|#version 450
#extension GL_ARB_separate_shader_objects : enable
// ======================================================================
// Constant
%s
// ======================================================================
// Structs
%s
// ======================================================================
// Uniforms
%s
// ======================================================================
// Inputs
%s
// ======================================================================
// Outputs
%s
// ======================================================================
// Functions
%s
|}
      constants structs uniforms inputs outputs functions
end

module Library = struct
  type t = { name : string; shaders : Shader.t list }

  let empty name = { name; shaders = [] }

  let name l = l.name

  let shaders l = l.shaders

  let add_shader sh l = { l with shaders = sh :: l.shaders }
end
