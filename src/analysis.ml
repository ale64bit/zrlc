open Monad

module SS = Set.Make(String)

type error = [
  | `Redefinition of string
  | `DuplicateMember of string * string
  | `DuplicateParameter of string
  | `Unimplemented of string
  | `UnknownTypeName of string
  | `NonIntegerArraySize 
  | `UndeclaredIdentifier of string
]

let check_const_decl env {Ast.cd_name; cd_value} = 
  if Env.constant_exists cd_name env then
    Error (`Redefinition cd_name)
  else match cd_value with
    | BoolLiteral b -> 
        Ok (Env.add_constant cd_name (Env.Bool b) env, [])
    | IntLiteral i -> 
        Ok (Env.add_constant cd_name (Env.Int i) env, [])
    | FloatLiteral f -> 
        Ok (Env.add_constant cd_name (Env.Float f) env, [])
    | _ -> 
        Error (`Unimplemented "constant initializer must be a boolean, integer or float literal")

let check_unique ids errfn = 
  List.fold_left 
    (fun seen id ->
      seen >>= fun seen ->
        if SS.mem id seen then Error (errfn id)
        else Ok (SS.add id seen))
  (Ok SS.empty)
  ids

let check_array_dim env = function
  | Type.OfInt _ -> Ok env
  | Type.OfName name -> (
      match Env.get_constant name env with
      | Some (Env.Int _) -> Ok env
      | Some (Env.Bool _) | Some (Env.Float _) -> Error (`NonIntegerArraySize)
      | None -> Error (`UndeclaredIdentifier name) )
  | Type.OfFloat _ | Type.OfBool _ ->
      Error (`NonIntegerArraySize)

let rec check_type env = function
  | Type.TypeRef name ->
      if Env.type_exists name env then Ok env
      else Error (`UnknownTypeName name) 
  | Type.Record fields ->
      List.fold_left
        (fun env f -> 
          env >>= fun env -> check_type env f.Type.t)
        (Ok env)
        fields
  | Type.Array (t, dims) ->
      check_type env t >>= fun env ->
      (List.fold_left
        (fun env dim ->
          env >>= fun env -> check_array_dim env dim)
        (Ok env)
        dims)
  | Type.Function (args, rets) ->
      let errfn = fun id -> `DuplicateParameter id in
      let ids = List.map (fun f -> f.Type.name) args in
      check_unique ids errfn >>= fun _ ->
      (List.fold_left
        (fun env f -> env >>= fun env -> check_type env f.Type.t)
        (Ok env)
        args) >>= fun env ->
      (List.fold_left
        (fun env t -> env >>= fun env -> check_type env t)
        (Ok env)
        rets)
  | Type.Primitive _ ->
      failwith "primitive types should not appear in analysis phase input"

let check_type_decl env {Ast.td_name; td_type} = 
  if Env.type_exists td_name env then
    Error (`Redefinition td_name)
  else match td_type with
    | Record fields ->
        let errfn = fun id -> `DuplicateMember (td_name, id) in
        let ids = List.map (fun f -> f.Type.name) fields in
        check_unique ids errfn >>= fun _ ->
        check_type env td_type >>= fun _ ->
        Ok (Env.add_type td_name td_type env, [])
    | TypeRef _ | Array _ | Function _ | Primitive _ ->
        let msg = Printf.sprintf "type %s: type declarations should always be Record" td_name in
        failwith msg

(* TODO: implement *)
let check_function_decl env {Ast.fd_name; fd_type; _} = 
  let tmp = {
    TypedAst.fd_name = fd_name;
    fd_type = fd_type;
    fd_body = [];
  } in
  Ok (env, tmp)
        
let check_pipeline_decl env {Ast.pd_name; pd_type; pd_functions} = 
  if Env.pipeline_exists pd_name env then
    Error (`Redefinition pd_name)
  else
    let errfn = fun id -> `Redefinition id in
    let ids = List.map (fun f -> f.Ast.fd_name) pd_functions in
    check_unique ids errfn >>= fun _ ->
    check_type env pd_type >>= fun env ->
    Ok (Env.add_pipeline pd_name pd_type env) >>= fun genv ->
    (List.fold_left
      (fun acc fdecl ->
        acc >>= fun (env, funcs) -> 
        check_function_decl env fdecl >>= fun f ->
        Ok (Env.add_function fdecl.fd_name fdecl.fd_type env, f :: funcs))
      (Ok (genv, []))
      pd_functions
    ) >>= fun (penv, funcs) ->
    let pd = {
      TypedAst.pd_name = pd_name;
      pd_type = pd_type;
      pd_functions = List.rev funcs; 
    } in Ok (genv, [TypedAst.PipelineDecl (penv, pd)])

(* TODO: implement *)
let check_renderer_decl env _ = Ok (env, [])

let check_toplevel env = function
  | Ast.ConstDecl cd -> check_const_decl env cd
  | Ast.TypeDecl td -> check_type_decl env td
  | Ast.PipelineDecl pd -> check_pipeline_decl env pd
  | Ast.RendererDecl rd -> check_renderer_decl env rd

let permutations l =
  let rec aux left right = function
    | [] -> [left]
    | hd :: tl ->
        let r = aux (hd :: left) [] (right @ tl) in
        if tl <> [] then r @ aux left (hd :: right) tl
        else r in
  aux [] [] l 

let rec combinations k l =
  if k <= 0 then [ [] ]
  else match l with
    | [] -> []
    | hd :: tl ->
        let with_h = List.map (fun l -> hd :: l) (combinations (k-1) tl) in
        let without_h = combinations k tl in
        with_h @ without_h

let generate_fields_for_swizzle len swizzle =
  let sizes = List.init len ((+) 1) in
  (List.concat 
    (List.map 
      (fun k ->
        let all_k_swizzles = List.map permutations (combinations k swizzle) in
        List.concat all_k_swizzles)
      sizes))

let generate_fields size ptname tprefix =
  let coord_base = (Stream.of_string "xyzw") in
  let color_base = (Stream.of_string "rgba") in
  let textr_base = (Stream.of_string "stpq") in
  let coord = List.init size (fun _ -> Stream.next coord_base) in 
  let color = List.init size (fun _ -> Stream.next color_base) in 
  let textr = List.init size (fun _ -> Stream.next textr_base) in 
  let all = [coord; color; textr] in
  let swizzles = List.concat (List.map (generate_fields_for_swizzle size) all) in
  let swizzles = List.map (fun sw -> String.concat "" (List.map Char.escaped sw)) swizzles in
  let swizzles = List.sort Pervasives.compare swizzles in
  List.map 
    (fun name -> 
      let sz = String.length name in
      let t = if sz = 1 then Type.TypeRef ptname 
        else Type.TypeRef (Printf.sprintf "%s%d" tprefix sz) in
      {Type.name=name; t=t})
    swizzles

let global_env = 
  let builtins = [
    (* Basic types *)
    ("bool", (Type.Primitive Bool));
    ("int", (Type.Primitive Int));
    ("uint", (Type.Primitive UInt));
    ("float", (Type.Primitive Float));
    ("double", (Type.Primitive Double));
    (* Vector types *)
    ("bvec2", (Type.Record (generate_fields 2 "bool" "bvec")));
    ("bvec3", (Type.Record (generate_fields 3 "bool" "bvec")));
    ("bvec4", (Type.Record (generate_fields 4 "bool" "bvec")));
    ("ivec2", (Type.Record (generate_fields 2 "int" "ivec")));
    ("ivec3", (Type.Record (generate_fields 3 "int" "ivec")));
    ("ivec4", (Type.Record (generate_fields 4 "int" "ivec")));
    ("uvec2", (Type.Record (generate_fields 2 "uint" "uvec")));
    ("uvec3", (Type.Record (generate_fields 3 "uint" "uvec")));
    ("uvec4", (Type.Record (generate_fields 4 "uint" "uvec")));
    ("vec2", (Type.Record (generate_fields 2 "float" "vec")));
    ("vec3", (Type.Record (generate_fields 3 "float" "vec")));
    ("vec4", (Type.Record (generate_fields 4 "float" "vec")));
    ("dvec2", (Type.Record (generate_fields 2 "double" "dvec")));
    ("dvec3", (Type.Record (generate_fields 3 "double" "dvec")));
    ("dvec4", (Type.Record (generate_fields 4 "double" "dvec")));
    (* Matrix types *)
    ("mat2", (Type.Array (Type.TypeRef "float", [OfInt 2; OfInt 2])));
    ("mat3", (Type.Array (Type.TypeRef "float", [OfInt 3; OfInt 3])));
    ("mat4", (Type.Array (Type.TypeRef "float", [OfInt 4; OfInt 4])));
    ("mat2x2", (Type.Array (Type.TypeRef "float", [OfInt 2; OfInt 2])));
    ("mat2x3", (Type.Array (Type.TypeRef "float", [OfInt 2; OfInt 3])));
    ("mat2x4", (Type.Array (Type.TypeRef "float", [OfInt 2; OfInt 4])));
    ("mat3x2", (Type.Array (Type.TypeRef "float", [OfInt 3; OfInt 2])));
    ("mat3x3", (Type.Array (Type.TypeRef "float", [OfInt 3; OfInt 3])));
    ("mat3x4", (Type.Array (Type.TypeRef "float", [OfInt 3; OfInt 4])));
    ("mat4x2", (Type.Array (Type.TypeRef "float", [OfInt 4; OfInt 2])));
    ("mat4x3", (Type.Array (Type.TypeRef "float", [OfInt 4; OfInt 3])));
    ("mat4x4", (Type.Array (Type.TypeRef "float", [OfInt 4; OfInt 4])));
    ("dmat2", (Type.Array (Type.TypeRef "double", [OfInt 2; OfInt 2])));
    ("dmat3", (Type.Array (Type.TypeRef "double", [OfInt 3; OfInt 3])));
    ("dmat4", (Type.Array (Type.TypeRef "double", [OfInt 4; OfInt 4])));
    ("dmat2x2", (Type.Array (Type.TypeRef "double", [OfInt 2; OfInt 2])));
    ("dmat2x3", (Type.Array (Type.TypeRef "double", [OfInt 2; OfInt 3])));
    ("dmat2x4", (Type.Array (Type.TypeRef "double", [OfInt 2; OfInt 4])));
    ("dmat3x2", (Type.Array (Type.TypeRef "double", [OfInt 3; OfInt 2])));
    ("dmat3x3", (Type.Array (Type.TypeRef "double", [OfInt 3; OfInt 3])));
    ("dmat3x4", (Type.Array (Type.TypeRef "double", [OfInt 3; OfInt 4])));
    ("dmat4x2", (Type.Array (Type.TypeRef "double", [OfInt 4; OfInt 2])));
    ("dmat4x3", (Type.Array (Type.TypeRef "double", [OfInt 4; OfInt 3])));
    ("dmat4x4", (Type.Array (Type.TypeRef "double", [OfInt 4; OfInt 4])));
  ] in
  List.fold_left 
    (fun env (name, t) -> Env.add_type name t env) 
    (Env.empty "global")
    builtins

let check ast = 
  List.fold_left
    (fun acc tl ->
      acc >>= fun (env, tls) ->
      check_toplevel env tl >>= fun (env, res) ->
      Ok (env, tls @ res))
    (Ok (global_env, []))
    ast

