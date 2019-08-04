module SymbolTable = Map.Make (String)
module L = Located

type const_value = Bool of bool | Int of int | Float of float
[@@deriving to_yojson]

type located_type = Type.t L.t [@@deriving to_yojson]

type located_const_value = const_value L.t [@@deriving to_yojson]

type type_symbol_table = located_type SymbolTable.t

type const_value_symbol_table = located_const_value SymbolTable.t

let builtin_pos =
  Lexing.{ pos_fname = "builtin"; pos_lnum = 0; pos_cnum = 0; pos_bol = 0 }

let builtin_loc = (builtin_pos, builtin_pos)

let type_symbol_table_to_yojson st =
  let f (k, v) = (k, located_type_to_yojson v) in
  `Assoc (List.map f (SymbolTable.bindings st))

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
  functions : type_symbol_table;
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

let find_function ~local name env =
  let f env = env.functions in
  find ~local name env f

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
  >>? find_function ~local name env
  >>? find_pipeline ~local name env

let rec find_name_scope name env =
  let open Monad.Option in
  let res =
    find_constant_type ~local:true name env
    >>? find_type ~local:true name env
    >>? find_var ~local:true name env
    >>? find_val ~local:true name env
    >>? find_function ~local:true name env
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
  >>? find_function ~local:false name env
  >>? find_pipeline ~local:false name env

(* Exists *)
let constant_exists name env = find_constant ~local:false name env <> None

let type_exists name env = find_type ~local:false name env <> None

let pipeline_exists name env = find_pipeline ~local:false name env <> None

let renderer_exists name env = find_renderer ~local:false name env <> None

let function_exists name env = find_function ~local:false name env <> None

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
  let () = assert (not (function_exists name env)) in
  { env with functions = SymbolTable.add name typ env.functions }

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
    functions = SymbolTable.empty;
  }

let generate_constructor_type base params ret =
  Type.Function
    ( List.init (String.length params) (fun i ->
          (Char.escaped params.[i], Type.TypeRef base)),
      [ Type.TypeRef ret ] )

let global =
  let open Type in
  let builtin_types =
    [ (* Primitive Types *)
      ("bool", Primitive Bool);
      ("int", Primitive Int);
      ("uint", Primitive UInt);
      ("float", Primitive Float);
      ("double", Primitive Double);
      (* Vector Types *)
      ("bvec2", Vector (Bool, 2));
      ("bvec3", Vector (Bool, 3));
      ("bvec4", Vector (Bool, 4));
      ("ivec2", Vector (Int, 2));
      ("ivec3", Vector (Int, 3));
      ("ivec4", Vector (Int, 4));
      ("uvec2", Vector (UInt, 2));
      ("uvec3", Vector (UInt, 3));
      ("uvec4", Vector (UInt, 4));
      ("fvec2", Vector (Float, 2));
      ("fvec3", Vector (Float, 3));
      ("fvec4", Vector (Float, 4));
      ("dvec2", Vector (Double, 2));
      ("dvec3", Vector (Double, 3));
      ("dvec4", Vector (Double, 4));
      (* Matrix Types *)
      ("bmat2", Matrix (Bool, 2, 2));
      ("bmat3", Matrix (Bool, 3, 3));
      ("bmat4", Matrix (Bool, 4, 4));
      ("bmat2x2", Matrix (Bool, 2, 2));
      ("bmat2x3", Matrix (Bool, 2, 3));
      ("bmat2x4", Matrix (Bool, 2, 4));
      ("bmat3x2", Matrix (Bool, 3, 2));
      ("bmat3x3", Matrix (Bool, 3, 3));
      ("bmat3x4", Matrix (Bool, 3, 4));
      ("bmat4x2", Matrix (Bool, 4, 2));
      ("bmat4x3", Matrix (Bool, 4, 3));
      ("bmat4x4", Matrix (Bool, 4, 4));
      ("imat2", Matrix (Int, 2, 2));
      ("imat3", Matrix (Int, 3, 3));
      ("imat4", Matrix (Int, 4, 4));
      ("imat2x2", Matrix (Int, 2, 2));
      ("imat2x3", Matrix (Int, 2, 3));
      ("imat2x4", Matrix (Int, 2, 4));
      ("imat3x2", Matrix (Int, 3, 2));
      ("imat3x3", Matrix (Int, 3, 3));
      ("imat3x4", Matrix (Int, 3, 4));
      ("imat4x2", Matrix (Int, 4, 2));
      ("imat4x3", Matrix (Int, 4, 3));
      ("imat4x4", Matrix (Int, 4, 4));
      ("umat2", Matrix (UInt, 2, 2));
      ("umat3", Matrix (UInt, 3, 3));
      ("umat4", Matrix (UInt, 4, 4));
      ("umat2x2", Matrix (UInt, 2, 2));
      ("umat2x3", Matrix (UInt, 2, 3));
      ("umat2x4", Matrix (UInt, 2, 4));
      ("umat3x2", Matrix (UInt, 3, 2));
      ("umat3x3", Matrix (UInt, 3, 3));
      ("umat3x4", Matrix (UInt, 3, 4));
      ("umat4x2", Matrix (UInt, 4, 2));
      ("umat4x3", Matrix (UInt, 4, 3));
      ("umat4x4", Matrix (UInt, 4, 4));
      ("fmat2", Matrix (Float, 2, 2));
      ("fmat3", Matrix (Float, 3, 3));
      ("fmat4", Matrix (Float, 4, 4));
      ("fmat2x2", Matrix (Float, 2, 2));
      ("fmat2x3", Matrix (Float, 2, 3));
      ("fmat2x4", Matrix (Float, 2, 4));
      ("fmat3x2", Matrix (Float, 3, 2));
      ("fmat3x3", Matrix (Float, 3, 3));
      ("fmat3x4", Matrix (Float, 3, 4));
      ("fmat4x2", Matrix (Float, 4, 2));
      ("fmat4x3", Matrix (Float, 4, 3));
      ("fmat4x4", Matrix (Float, 4, 4));
      ("dmat2", Matrix (Double, 2, 2));
      ("dmat3", Matrix (Double, 3, 3));
      ("dmat4", Matrix (Double, 4, 4));
      ("dmat2x2", Matrix (Double, 2, 2));
      ("dmat2x3", Matrix (Double, 2, 3));
      ("dmat2x4", Matrix (Double, 2, 4));
      ("dmat3x2", Matrix (Double, 3, 2));
      ("dmat3x3", Matrix (Double, 3, 3));
      ("dmat3x4", Matrix (Double, 3, 4));
      ("dmat4x2", Matrix (Double, 4, 2));
      ("dmat4x3", Matrix (Double, 4, 3));
      ("dmat4x4", Matrix (Double, 4, 4));
      (* Sampler Types *)
      ("sampler1D", Sampler 1);
      ("sampler2D", Sampler 2);
      ("sampler3D", Sampler 3);
      (* Texture Types *)
      ("texture1D", Texture 1);
      ("texture2D", Texture 2);
      ("texture3D", Texture 3);
      (* Render Target Types *)
      ("rt_rgb", RenderTarget RGB);
      ("rt_rgba", RenderTarget RGBA);
      ("rt_ds", RenderTarget DS);
      (* Atom Types *)
      ("atom", Atom Singleton);
      ("atomlist", Atom List);
      ("atomset", Atom Set)
    ]
  in
  let builtin_functions =
    [ (* Primitive types *)
      ("uint", generate_constructor_type "int" "i" "uint");
      ("double", generate_constructor_type "float" "f" "double");
      (* Built-in vector types *)
      ("bvec2", generate_constructor_type "bool" "xy" "bvec2");
      ("bvec3", generate_constructor_type "bool" "xyz" "bvec3");
      ("bvec4", generate_constructor_type "bool" "xyzw" "bvec4");
      ("ivec2", generate_constructor_type "int" "xy" "ivec2");
      ("ivec3", generate_constructor_type "int" "xyz" "ivec3");
      ("ivec4", generate_constructor_type "int" "xyzw" "ivec4");
      ("uvec2", generate_constructor_type "uint" "xy" "uvec2");
      ("uvec3", generate_constructor_type "uint" "xyz" "uvec3");
      ("uvec4", generate_constructor_type "uint" "xyzw" "uvec4");
      ("fvec2", generate_constructor_type "float" "xy" "fvec2");
      ("fvec3", generate_constructor_type "float" "xyz" "fvec3");
      ("fvec4", generate_constructor_type "float" "xyzw" "fvec4");
      ("dvec2", generate_constructor_type "double" "xy" "dvec2");
      ("dvec3", generate_constructor_type "double" "xyz" "dvec3");
      ("dvec4", generate_constructor_type "double" "xyzw" "dvec4");
      ("fmat2", generate_constructor_type "fvec2" "xy" "fmat2");
      ("fmat3", generate_constructor_type "fvec3" "xyz" "fmat3");
      ("fmat4", generate_constructor_type "fvec4" "xyzw" "fmat4");
      (* TODO: Built-in matrix types *)
      (* Texture lookup functions *)
      ( "texture",
        Function
          ( [ ("sampler", TypeRef "sampler2D"); ("coord", TypeRef "fvec2") ],
            [ TypeRef "fvec4" ] ) );
      (* Math functions *)
      ("normalize", Function ([ ("v", TypeRef "fvec3") ], [ TypeRef "fvec3" ]));
      ( "pow",
        Function
          ( [ ("x", TypeRef "float"); ("y", TypeRef "float") ],
            [ TypeRef "float" ] ) );
      ( "pow2",
        Function
          ( [ ("x", TypeRef "fvec2"); ("y", TypeRef "fvec2") ],
            [ TypeRef "fvec2" ] ) );
      ( "pow3",
        Function
          ( [ ("x", TypeRef "fvec3"); ("y", TypeRef "fvec3") ],
            [ TypeRef "fvec3" ] ) );
      ( "pow4",
        Function
          ( [ ("x", TypeRef "fvec4"); ("y", TypeRef "fvec4") ],
            [ TypeRef "fvec4" ] ) );
      ( "mix",
        Function
          ( [ ("x", TypeRef "fvec3");
              ("y", TypeRef "fvec3");
              ("k", TypeRef "float")
            ],
            [ TypeRef "fvec3" ] ) );
      ( "clamp",
        Function
          ( [ ("x", TypeRef "float");
              ("minVal", TypeRef "float");
              ("maxVal", TypeRef "float")
            ],
            [ TypeRef "float" ] ) );
      ("sqrt", Function ([ ("x", TypeRef "float") ], [ TypeRef "float" ]));
      ("length", Function ([ ("x", TypeRef "fvec3") ], [ TypeRef "float" ]));
      ( "max",
        Function
          ( [ ("x", TypeRef "float"); ("y", TypeRef "float") ],
            [ TypeRef "float" ] ) );
      ( "min",
        Function
          ( [ ("x", TypeRef "float"); ("y", TypeRef "float") ],
            [ TypeRef "float" ] ) );
      ( "dot",
        Function
          ( [ ("x", TypeRef "fvec3"); ("y", TypeRef "fvec3") ],
            [ TypeRef "float" ] ) );
      ( "cross",
        Function
          ( [ ("x", TypeRef "fvec3"); ("y", TypeRef "fvec3") ],
            [ TypeRef "fvec3" ] ) )
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
    {
      env with
      parent =
        ( env.parent >>= fun t ->
          Some (filter_global t) );
      types = SymbolTable.(filter (skip global.types) env.types);
      constants = SymbolTable.(filter (skip global.constants) env.constants);
      vals = SymbolTable.(filter (skip global.vals) env.vals);
      vars = SymbolTable.(filter (skip global.vars) env.vars);
      pipelines = SymbolTable.(filter (skip global.pipelines) env.pipelines);
      renderers = SymbolTable.(filter (skip global.renderers) env.renderers);
      functions = SymbolTable.(filter (skip global.functions) env.functions);
    })

let to_yojson t = to_yojson (filter_global t)

let string_of_env env = Yojson.Safe.pretty_to_string (to_yojson env)
