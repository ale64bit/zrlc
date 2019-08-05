module SymbolTable = Map.Make (String)

module FunctionSymbolTable = Map.Make (struct
  type t = string * Type.t list

  let compare = Pervasives.compare
end)

module L = Located

type const_value = Bool of bool | Int of int | Float of float
[@@deriving to_yojson]

type located_type = Type.t L.t [@@deriving to_yojson]

type located_const_value = const_value L.t [@@deriving to_yojson]

type type_symbol_table = located_type SymbolTable.t

type function_symbol_table = located_type FunctionSymbolTable.t

type const_value_symbol_table = located_const_value SymbolTable.t

let builtin_pos =
  Lexing.{ pos_fname = "builtin"; pos_lnum = 0; pos_cnum = 0; pos_bol = 0 }

let builtin_loc = (builtin_pos, builtin_pos)

let type_symbol_table_to_yojson st =
  let f (k, v) = (k, located_type_to_yojson v) in
  `Assoc (List.map f (SymbolTable.bindings st))

let function_symbol_table_to_yojson st =
  let f ((name, _), v) = (name, located_type_to_yojson v) in
  `Assoc (List.map f (FunctionSymbolTable.bindings st))

let const_value_symbol_table_to_yojson st =
  let f (k, v) = (k, located_const_value_to_yojson v) in
  `Assoc (List.map f (SymbolTable.bindings st))

type summary =
  | Global
  | Module of string
  | Pipeline of string * Type.t
  | Renderer of string * Type.t
  | Function of string * Type.t
  | Block of (L.lexing_position * L.lexing_position)
[@@deriving to_yojson]

type t = {
  id : string;
  summary : summary;
  parent : t option;
  types : type_symbol_table;
  constants : const_value_symbol_table;
  vars : type_symbol_table;
  vals : type_symbol_table;
  pipelines : type_symbol_table;
  renderers : type_symbol_table;
  functions : function_symbol_table;
}
[@@deriving to_yojson]

(* Find *)

let rec find ~local name env f =
  let open Monad.Option in
  SymbolTable.find_opt name (f env)
  >>?
  match env.parent with
  | Some penv when not local -> find ~local name penv f
  | _ -> None

let find_constant ~local name env =
  let f env = env.constants in
  find ~local name env f

let find_constant_type ~local name env =
  match find_constant ~local name env with
  | Some L.{ loc; value = Bool _ } ->
      Some L.{ loc; value = Type.TypeRef "bool" }
  | Some L.{ loc; value = Int _ } -> Some L.{ loc; value = Type.TypeRef "int" }
  | Some L.{ loc; value = Float _ } ->
      Some L.{ loc; value = Type.TypeRef "float" }
  | None -> None

let find_type ~local name env =
  let f env = env.types in
  find ~local name env f

let find_pipeline ~local name env =
  let f env = env.pipelines in
  find ~local name env f

let find_renderer ~local name env =
  let f env = env.renderers in
  find ~local name env f

let rec find_function ~local name params env =
  let open Monad.Option in
  FunctionSymbolTable.find_opt (name, params) env.functions
  >>?
  match env.parent with
  | Some penv when not local -> find_function ~local name params penv
  | _ -> None

let rec find_any_function ~local name env =
  let open Monad.Option in
  let all_functions = FunctionSymbolTable.bindings env.functions in
  ( match List.find_opt (fun ((fname, _), _) -> name = fname) all_functions with
  | Some (_, f_type) -> Some f_type
  | None -> None )
  >>?
  match env.parent with
  | Some penv when not local -> find_any_function ~local name penv
  | _ -> None

let find_var ~local name env =
  let f env = env.vars in
  find ~local name env f

let find_val ~local name env =
  let f env = env.vals in
  find ~local name env f

let find_name ~local name env =
  let open Monad.Option in
  find_constant_type ~local name env
  >>? find_type ~local name env >>? find_var ~local name env
  >>? find_val ~local name env
  >>? find_any_function ~local name env
  >>? find_pipeline ~local name env

let rec find_name_scope name env =
  let open Monad.Option in
  let res =
    find_constant_type ~local:true name env
    >>? find_type ~local:true name env
    >>? find_var ~local:true name env
    >>? find_val ~local:true name env
    (*     >>? find_function ~local:true name env *)
    >>? find_pipeline ~local:true name env
  in
  match res with
  | Some _ -> env.summary
  | None -> (
      match env.parent with
      | Some penv -> find_name_scope name penv
      | None -> failwith ("no such name: " ^ name) )

let find_lvalue name env = find_var ~local:false name env

let find_rvalue name env =
  let open Monad.Option in
  find_constant_type ~local:false name env
  >>? find_var ~local:false name env
  >>? find_val ~local:false name env
  >>? find_any_function ~local:false name env
  >>? find_pipeline ~local:false name env

(* Exists *)
let constant_exists name env = find_constant ~local:false name env <> None

let type_exists name env = find_type ~local:false name env <> None

let pipeline_exists name env = find_pipeline ~local:false name env <> None

let renderer_exists name env = find_renderer ~local:false name env <> None

let function_exists name params env =
  find_function ~local:false name params env <> None

let var_exists ~local name env = find_var ~local name env <> None

let val_exists ~local name env = find_val ~local name env <> None

let name_exists name env = find_name ~local:false name env <> None

(* Add *)

let add_constant name value env =
  let () = assert (not (constant_exists name env)) in
  { env with constants = SymbolTable.add name value env.constants }

let add_type name typ env =
  ( match typ.L.value with
  | Type.TypeRef refname ->
      failwith
        (Printf.sprintf
           "Env.add_type: name=%s type=%s\n\
            Type references should never be added to the environment directly"
           name refname)
  | _ -> () );
  { env with types = SymbolTable.add name typ env.types }

let add_pipeline name typ env =
  let () = assert (not (pipeline_exists name env)) in
  { env with pipelines = SymbolTable.add name typ env.pipelines }

let add_renderer name typ env =
  let () = assert (not (renderer_exists name env)) in
  { env with renderers = SymbolTable.add name typ env.renderers }

let add_function name typ env =
  let L.{ value = t; _ } = typ in
  match t with
  | Type.Function (params, _) ->
      let param_types = List.map (fun (_, t) -> t) params in
      let () = assert (not (function_exists name param_types env)) in
      {
        env with
        functions =
          FunctionSymbolTable.add (name, param_types) typ env.functions;
      }
  | _ ->
      failwith
        (Printf.sprintf "cannot add non-function %s (type %s) as function" name
           (Type.string_of_type t))

let add_var name typ env =
  let () = assert (not (var_exists ~local:true name env)) in
  { env with vars = SymbolTable.add name typ env.vars }

let add_val name typ env =
  let () = assert (not (val_exists ~local:true name env)) in
  { env with vals = SymbolTable.add name typ env.vals }

(* Constructors *)
let empty id =
  {
    id;
    summary = Global;
    parent = None;
    types = SymbolTable.empty;
    constants = SymbolTable.empty;
    vars = SymbolTable.empty;
    vals = SymbolTable.empty;
    pipelines = SymbolTable.empty;
    renderers = SymbolTable.empty;
    functions = FunctionSymbolTable.empty;
  }

let global =
  let open Type in
  let open Type.Common in
  (* Builtin types *)
  let builtin_types =
    [ (* Primitive Types *)
      ("bool", bool);
      ("int", int);
      ("uint", uint);
      ("float", float);
      ("double", double);
      (* Vector Types *)
      ("bvec2", bvec2);
      ("bvec3", bvec3);
      ("bvec4", bvec4);
      ("ivec2", ivec2);
      ("ivec3", ivec3);
      ("ivec4", ivec4);
      ("uvec2", uvec2);
      ("uvec3", uvec3);
      ("uvec4", uvec4);
      ("fvec2", fvec2);
      ("fvec3", fvec3);
      ("fvec4", fvec4);
      ("dvec2", dvec2);
      ("dvec3", dvec3);
      ("dvec4", dvec4);
      (* Matrix Types *)
      ("bmat2", bmat2);
      ("bmat3", bmat3);
      ("bmat4", bmat4);
      ("bmat2x2", bmat2x2);
      ("bmat2x3", bmat2x3);
      ("bmat2x4", bmat2x4);
      ("bmat3x2", bmat3x2);
      ("bmat3x3", bmat3x3);
      ("bmat3x4", bmat3x4);
      ("bmat4x2", bmat4x2);
      ("bmat4x3", bmat4x3);
      ("bmat4x4", bmat4x4);
      ("imat2", imat2);
      ("imat3", imat3);
      ("imat4", imat4);
      ("imat2x2", imat2x2);
      ("imat2x3", imat2x3);
      ("imat2x4", imat2x4);
      ("imat3x2", imat3x2);
      ("imat3x3", imat3x3);
      ("imat3x4", imat3x4);
      ("imat4x2", imat4x2);
      ("imat4x3", imat4x3);
      ("imat4x4", imat4x4);
      ("umat2", umat2);
      ("umat3", umat3);
      ("umat4", umat4);
      ("umat2x2", umat2x2);
      ("umat2x3", umat2x3);
      ("umat2x4", umat2x4);
      ("umat3x2", umat3x2);
      ("umat3x3", umat3x3);
      ("umat3x4", umat3x4);
      ("umat4x2", umat4x2);
      ("umat4x3", umat4x3);
      ("umat4x4", umat4x4);
      ("fmat2", fmat2);
      ("fmat3", fmat3);
      ("fmat4", fmat4);
      ("fmat2x2", fmat2x2);
      ("fmat2x3", fmat2x3);
      ("fmat2x4", fmat2x4);
      ("fmat3x2", fmat3x2);
      ("fmat3x3", fmat3x3);
      ("fmat3x4", fmat3x4);
      ("fmat4x2", fmat4x2);
      ("fmat4x3", fmat4x3);
      ("fmat4x4", fmat4x4);
      ("dmat2", dmat2);
      ("dmat3", dmat3);
      ("dmat4", dmat4);
      ("dmat2x2", dmat2x2);
      ("dmat2x3", dmat2x3);
      ("dmat2x4", dmat2x4);
      ("dmat3x2", dmat3x2);
      ("dmat3x3", dmat3x3);
      ("dmat3x4", dmat3x4);
      ("dmat4x2", dmat4x2);
      ("dmat4x3", dmat4x3);
      ("dmat4x4", dmat4x4);
      (* Sampler Types *)
      ("sampler1D", sampler1D);
      ("sampler2D", sampler2D);
      ("sampler3D", sampler3D);
      (* Texture Types *)
      ("texture1D", texture1D);
      ("texture2D", texture2D);
      ("texture3D", texture3D);
      (* Render Target Types *)
      ("rt_rgb", rt_rgb);
      ("rt_rgba", rt_rgba);
      ("rt_ds", rt_ds);
      (* Atom Types *)
      ("atom", atom);
      ("atomlist", atomlist);
      ("atomset", atomset)
    ]
  in
  let vector_ctor pt dim =
    let mask = "xyzw" in
    let t = Vector (pt, dim) in
    let params =
      List.init dim (fun i -> (String.make 1 mask.[i], Primitive pt))
    in
    (Type.string_of_type t, Function (params, [ t ]))
  in
  let matrix_ctor pt n m =
    let mask = "xyzw" in
    let t = Matrix (pt, n, m) in
    let params =
      List.init n (fun i -> (String.make 1 mask.[i], Vector (pt, m)))
    in
    let ctor_name =
      if n = m then
        match pt with
        | Bool -> Printf.sprintf "bmat%d" n
        | Int -> Printf.sprintf "imat%d" n
        | UInt -> Printf.sprintf "umat%d" n
        | Float -> Printf.sprintf "fmat%d" n
        | Double -> Printf.sprintf "dmat%d" n
      else Type.string_of_type t
    in
    (ctor_name, Function (params, [ t ]))
  in
  let builtin_functions =
    [ (* Built-in vector types *)
      vector_ctor Bool 2;
      vector_ctor Bool 3;
      vector_ctor Bool 4;
      vector_ctor Int 2;
      vector_ctor Int 3;
      vector_ctor Int 4;
      vector_ctor UInt 2;
      vector_ctor UInt 3;
      vector_ctor UInt 4;
      vector_ctor Float 2;
      vector_ctor Float 3;
      vector_ctor Float 4;
      vector_ctor Double 2;
      vector_ctor Double 3;
      vector_ctor Double 4;
      (* Built-in matrix types *)
      matrix_ctor Float 2 2;
      matrix_ctor Float 2 3;
      matrix_ctor Float 2 4;
      matrix_ctor Float 3 2;
      matrix_ctor Float 3 3;
      matrix_ctor Float 3 4;
      matrix_ctor Float 4 2;
      matrix_ctor Float 4 3;
      matrix_ctor Float 4 4;
      matrix_ctor Double 2 2;
      matrix_ctor Double 2 3;
      matrix_ctor Double 2 4;
      matrix_ctor Double 3 2;
      matrix_ctor Double 3 3;
      matrix_ctor Double 3 4;
      matrix_ctor Double 4 2;
      matrix_ctor Double 4 3;
      matrix_ctor Double 4 4;
      (* Additional constructors for vector and matrix types *)
      ("bvec2", Function ([ ("x", bool) ], [ bvec2 ]));
      ("bvec3", Function ([ ("x", bool) ], [ bvec3 ]));
      ("bvec4", Function ([ ("x", bool) ], [ bvec4 ]));
      ("ivec2", Function ([ ("x", int) ], [ ivec2 ]));
      ("ivec3", Function ([ ("x", int) ], [ ivec3 ]));
      ("ivec4", Function ([ ("x", int) ], [ ivec4 ]));
      ("uvec2", Function ([ ("x", uint) ], [ uvec2 ]));
      ("uvec3", Function ([ ("x", uint) ], [ uvec3 ]));
      ("uvec4", Function ([ ("x", uint) ], [ uvec4 ]));
      ("fvec2", Function ([ ("x", float) ], [ fvec2 ]));
      ("fvec3", Function ([ ("x", float) ], [ fvec3 ]));
      ("fvec4", Function ([ ("x", float) ], [ fvec4 ]));
      ("dvec2", Function ([ ("x", double) ], [ dvec2 ]));
      ("dvec3", Function ([ ("x", double) ], [ dvec3 ]));
      ("dvec4", Function ([ ("x", double) ], [ dvec4 ]));
      ("bvec3", Function ([ ("x", bvec2); ("y", bool) ], [ bvec3 ]));
      ("bvec4", Function ([ ("x", bvec3); ("y", bool) ], [ bvec4 ]));
      ("ivec3", Function ([ ("x", ivec2); ("y", int) ], [ ivec3 ]));
      ("ivec4", Function ([ ("x", ivec3); ("y", int) ], [ ivec4 ]));
      ("uvec3", Function ([ ("x", uvec2); ("y", uint) ], [ uvec3 ]));
      ("uvec4", Function ([ ("x", uvec3); ("y", uint) ], [ uvec4 ]));
      ("fvec3", Function ([ ("x", fvec2); ("y", float) ], [ fvec3 ]));
      ("fvec4", Function ([ ("x", fvec3); ("y", float) ], [ fvec4 ]));
      ("dvec3", Function ([ ("x", dvec2); ("y", double) ], [ dvec3 ]));
      ("dvec4", Function ([ ("x", dvec3); ("y", double) ], [ dvec4 ]));
      (* Texture lookup *)
      ( "texture",
        Function ([ ("sampler", sampler2D); ("coord", fvec2) ], [ fvec4 ]) );
      (* Math functions *)
      ("normalize", Function ([ ("v", fvec2) ], [ fvec2 ]));
      ("normalize", Function ([ ("v", fvec3) ], [ fvec3 ]));
      ("normalize", Function ([ ("v", fvec4) ], [ fvec4 ]));
      ("pow", Function ([ ("x", float); ("y", float) ], [ float ]));
      ("pow", Function ([ ("x", fvec2); ("y", fvec2) ], [ fvec2 ]));
      ("pow", Function ([ ("x", fvec3); ("y", fvec3) ], [ fvec3 ]));
      ("pow", Function ([ ("x", fvec4); ("y", fvec4) ], [ fvec4 ]));
      ( "mix",
        Function ([ ("x", fvec2); ("y", fvec2); ("k", float) ], [ fvec2 ]) );
      ( "mix",
        Function ([ ("x", fvec3); ("y", fvec3); ("k", float) ], [ fvec3 ]) );
      ( "mix",
        Function ([ ("x", fvec4); ("y", fvec4); ("k", float) ], [ fvec4 ]) );
      ( "clamp",
        Function
          ([ ("x", float); ("minVal", float); ("maxVal", float) ], [ float ])
      );
      ("sqrt", Function ([ ("x", float) ], [ float ]));
      ("length", Function ([ ("x", fvec2) ], [ float ]));
      ("length", Function ([ ("x", fvec3) ], [ float ]));
      ("length", Function ([ ("x", fvec4) ], [ float ]));
      ("max", Function ([ ("x", float); ("y", float) ], [ float ]));
      ("min", Function ([ ("x", float); ("y", float) ], [ float ]));
      ("dot", Function ([ ("x", fvec2); ("y", fvec2) ], [ float ]));
      ("dot", Function ([ ("x", fvec3); ("y", fvec3) ], [ float ]));
      ("dot", Function ([ ("x", fvec4); ("y", fvec4) ], [ float ]));
      ("cross", Function ([ ("x", fvec2); ("y", fvec2) ], [ fvec2 ]));
      ("cross", Function ([ ("x", fvec3); ("y", fvec3) ], [ fvec3 ]));
      ("cross", Function ([ ("x", fvec4); ("y", fvec4) ], [ fvec4 ]))
    ]
  in
  let env =
    List.fold_left
      (fun env (name, t) -> add_type name { loc = builtin_loc; value = t } env)
      (empty "global") builtin_types
  in
  let env =
    List.fold_left
      (fun env (name, f) ->
        add_function name { loc = builtin_loc; value = f } env)
      env builtin_functions
  in
  env

(* Scope *)
let scope_summary env = env.summary

let enter_module_scope id env =
  let scope_id = "module$" ^ id in
  { (empty scope_id) with summary = Module id; parent = Some env }

let enter_pipeline_scope id typ env =
  let scope_id = "pipeline$" ^ id in
  { (empty scope_id) with summary = Pipeline (id, typ); parent = Some env }

let enter_renderer_scope id typ env =
  let scope_id = "renderer$" ^ id in
  { (empty scope_id) with summary = Renderer (id, typ); parent = Some env }

let enter_function_scope id typ env =
  let scope_id = "function$" ^ id in
  { (empty scope_id) with summary = Function (id, typ); parent = Some env }

let enter_block_scope id loc env =
  let id = "block$id" ^ id in
  { (empty id) with summary = Block loc; parent = Some env }

let exit_scope env =
  match env.parent with
  | Some penv -> penv
  | None -> failwith (Printf.sprintf "Trying to exit root scope: %s" env.id)

let rec is_pipeline_scope env =
  match env.summary with
  | Pipeline _ -> true
  | _ -> (
      match env.parent with Some env -> is_pipeline_scope env | None -> false )

let rec is_renderer_scope env =
  match env.summary with
  | Renderer _ -> true
  | _ -> (
      match env.parent with Some env -> is_renderer_scope env | None -> false )

let rec is_function_scope env =
  match env.summary with
  | Function _ -> true
  | Block _ -> (
      match env.parent with Some env -> is_function_scope env | None -> false )
  | _ -> false

let rec match_function_scope env =
  match env.summary with
  | Function _ -> Some env.summary
  | Block _ -> (
      match env.parent with
      | Some env -> match_function_scope env
      | None -> None )
  | _ -> None

(* Others *)
let add_builtin fname env =
  let open Type in
  match env.parent with
  | Some penv -> (
      match (scope_summary penv, fname) with
      | Pipeline _, "vertex" ->
          let t =
            Record
              [ ("position", TypeRef "fvec4");
                ("vertexID", TypeRef "int");
                ("instanceID", TypeRef "int")
              ]
          in
          add_var "builtin" L.{ loc = builtin_loc; value = t } env
      | Pipeline _, "fragment" ->
          let t =
            Record
              [ ("fragCoord", TypeRef "fvec4");
                ("currentDepth", TypeRef "float");
                ("frontFacing", TypeRef "bool")
              ]
          in
          add_val "builtin" L.{ loc = builtin_loc; value = t } env
      | Renderer _, "main" ->
          let t = Record [ ("screen", TypeRef "rt_rgba") ] in
          add_var "builtin" L.{ loc = builtin_loc; value = t } env
      | _ -> env )
  | None ->
      failwith
        "builtin must be added in function scopes, which should be nested in \
         pipeline or renderer scopes"

(* Printing *)
let rec filter_global env =
  let open Monad.Option in
  SymbolTable.(
    let skip src k _ = not (mem k src) in
    let skip_function src k _ = not (FunctionSymbolTable.mem k src) in
    {
      env with
      parent =
        ( env.parent >>= fun t ->
          Some (filter_global t) );
      types = filter (skip global.types) env.types;
      constants = filter (skip global.constants) env.constants;
      vals = filter (skip global.vals) env.vals;
      vars = filter (skip global.vars) env.vars;
      pipelines = filter (skip global.pipelines) env.pipelines;
      renderers = filter (skip global.renderers) env.renderers;
      functions =
        FunctionSymbolTable.(
          filter (skip_function global.functions) env.functions);
    })

let to_yojson t = to_yojson (filter_global t)

let string_of_env env = Yojson.Safe.pretty_to_string (to_yojson env)
