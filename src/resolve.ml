(*
let primitives = [
  "int";
  "float";
  "bool";
  "uint";
  "vec2";
  "vec3";
  "vec4";
  "uvec2";
  "uvec3";
  "uvec4";
  "mat2";
  "mat3";
  "mat4";
  "sampler2D";
  "crt";
  "dsrt";
  "set";
  "single";
]

let rec resolve_type types = function
  | Type.TypeRef name -> 
      Hashtbl.find types name
  | Type.Record fields -> 
      Type.Record (List.map (fun r -> {Type.name=r.Type.name; Type.t=resolve_type types r.t}) fields)
  | Type.Array (t, dims) -> 
      Type.Array (resolve_type types t, dims)
  | Type.Function (args, ret) -> 
      let args = List.map (fun r -> {Type.name=r.Type.name; Type.t=resolve_type types r.t}) args in
      let ret = List.map (resolve_type types) ret in
      Type.Function (args, ret)
(*
  | Type.Primitive name -> 
      Type.Primitive name
*)

let resolve_root types = function
  | Ast.ConstDef (id, expr) -> Ast.ConstDef (id, expr)
  | Ast.TypeDef (id, t) ->
      let resolved = resolve_type types t in
      Hashtbl.add types id resolved ;
      Printf.printf "resolved %s = %s\n" id (Type.string_of_type resolved) ;
      Ast.TypeDef (id, resolved)
  | Ast.PipelineDef (id, t, funcs) -> Ast.PipelineDef (id, resolve_type types t, funcs)
  | Ast.RendererDef (id, t, stmts) -> Ast.RendererDef (id, resolve_type types t, stmts)

let pass roots = 
  let types = Hashtbl.create 32 in
  List.iter (fun name -> Hashtbl.add types name (Type.Primitive name)) primitives ;
  Ok (List.map (resolve_root types) roots)


*)
