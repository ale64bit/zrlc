module SymbolTable = Map.Make (String)
module L = Located

type const_value = Bool of bool | Int of int | Float of float
[@@deriving to_yojson]

type located_type = Type.t L.t [@@deriving to_yojson]

type located_const_value = const_value L.t [@@deriving to_yojson]

type type_symbol_table = located_type SymbolTable.t

type const_value_symbol_table = located_const_value SymbolTable.t

let builtin_pos =
  Lexing.{pos_fname= "builtin"; pos_lnum= 0; pos_cnum= 0; pos_bol= 0}

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
  | Pipeline of Type.t
  | Renderer of Type.t
  | Function of Type.t
  | Block of (L.lexing_position * L.lexing_position)
[@@deriving to_yojson]

type t =
  { id: string
  ; summary: summary
  ; parent: t option
  ; types: type_symbol_table
  ; constants: const_value_symbol_table
  ; vars: type_symbol_table
  ; vals: type_symbol_table
  ; pipelines: type_symbol_table
  ; renderers: type_symbol_table
  ; functions: type_symbol_table }
[@@deriving to_yojson]

let permutations l =
  let rec aux left right = function
    | [] ->
        [left]
    | hd :: tl ->
        let r = aux (hd :: left) [] (right @ tl) in
        if tl <> [] then r @ aux left (hd :: right) tl else r
  in
  aux [] [] l

let rec combinations k l =
  if k <= 0 then [[]]
  else
    match l with
    | [] ->
        []
    | hd :: tl ->
        let with_h = List.map (fun l -> hd :: l) (combinations (k - 1) tl) in
        let without_h = combinations k tl in
        with_h @ without_h

let generate_fields_for_swizzle len swizzle =
  let sizes = List.init len (( + ) 1) in
  List.concat
    (List.map
       (fun k ->
         let all_k_swizzles = List.map permutations (combinations k swizzle) in
         List.concat all_k_swizzles )
       sizes)

let generate_fields size ptname tprefix =
  let coord_base = Stream.of_string "xyzw" in
  let color_base = Stream.of_string "rgba" in
  let textr_base = Stream.of_string "stpq" in
  let coord = List.init size (fun _ -> Stream.next coord_base) in
  let color = List.init size (fun _ -> Stream.next color_base) in
  let textr = List.init size (fun _ -> Stream.next textr_base) in
  let all = [coord; color; textr] in
  let swizzles =
    List.concat (List.map (generate_fields_for_swizzle size) all)
  in
  let swizzles =
    List.map (fun sw -> String.concat "" (List.map Char.escaped sw)) swizzles
  in
  let swizzles = List.sort Pervasives.compare swizzles in
  List.map
    (fun name ->
      let sz = String.length name in
      let t =
        if sz = 1 then Type.TypeRef ptname
        else Type.TypeRef (Printf.sprintf "%s%d" tprefix sz)
      in
      (name, t) )
    swizzles

(* Find *)

let rec find ~local name env f =
  let open Monad.Option in
  SymbolTable.find_opt name (f env)
  >>?
  match env.parent with
  | Some penv when not local ->
      find ~local name penv f
  | _ ->
      None

let find_constant ~local name env =
  let f env = env.constants in
  find ~local name env f

let find_constant_type ~local name env =
  match find_constant ~local name env with
  | Some L.{loc; value= Bool _} ->
      Some L.{loc; value= Type.TypeRef "bool"}
  | Some L.{loc; value= Int _} ->
      Some L.{loc; value= Type.TypeRef "int"}
  | Some L.{loc; value= Float _} ->
      Some L.{loc; value= Type.TypeRef "float"}
  | None ->
      None

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
  {env with constants= SymbolTable.add name value env.constants}

let add_type name typ env =
  ( match typ.L.value with
  | Type.TypeRef refname ->
      failwith
        (Printf.sprintf
           "Env.add_type: name=%s type=%s\n\
            Type references should never be added to the environment directly"
           name refname)
  | _ ->
      () ) ;
  {env with types= SymbolTable.add name typ env.types}

let add_pipeline name typ env =
  let () = assert (not (pipeline_exists name env)) in
  {env with pipelines= SymbolTable.add name typ env.pipelines}

let add_renderer name typ env =
  let () = assert (not (renderer_exists name env)) in
  {env with renderers= SymbolTable.add name typ env.renderers}

let add_function name typ env =
  let () = assert (not (function_exists name env)) in
  {env with functions= SymbolTable.add name typ env.functions}

let add_var name typ env =
  let () = assert (not (var_exists ~local:true name env)) in
  {env with vars= SymbolTable.add name typ env.vars}

let add_val name typ env =
  let () = assert (not (val_exists ~local:true name env)) in
  {env with vals= SymbolTable.add name typ env.vals}

(* Constructors *)
let empty id =
  { id
  ; summary= Global
  ; parent= None
  ; types= SymbolTable.empty
  ; constants= SymbolTable.empty
  ; vars= SymbolTable.empty
  ; vals= SymbolTable.empty
  ; pipelines= SymbolTable.empty
  ; renderers= SymbolTable.empty
  ; functions= SymbolTable.empty }

let generate_constructor_type base params ret =
  Type.Function
    ( List.init (String.length params) (fun i ->
          (Char.escaped params.[i], Type.TypeRef base) )
    , [Type.TypeRef ret] )

let generate_texture_type dim stype =
  let open Type in
  let lptype = Printf.sprintf "ivec%d" dim in
  let sptype = Printf.sprintf "fvec%d" dim in
  let ret = [TypeRef "fvec4"] in
  Record
    [ ("load", Function ([("p", TypeRef lptype)], ret))
    ; ( "sample"
      , Function ([("p", TypeRef sptype); ("sampler", TypeRef stype)], ret) )
    ]

let generate_vector_type dim base name =
  let open Type in
  let fields = generate_fields dim base name in
  Record fields

let global =
  let open Type in
  let builtin_types =
    [ (* Primitive types *)
      ("bool", Primitive Bool)
    ; ("int", Primitive Int)
    ; ("uint", Primitive UInt)
    ; ("float", Primitive Float)
    ; ("double", Primitive Double)
    ; ("atom", Primitive Atom)
    ; ("atomlist", Primitive AtomList)
    ; ("atomset", Primitive AtomSet)
    ; (* Built-in render target types *)
      (* ("rt_rgb", RenderTarget RGB) TODO: doesn't seem widely supported *)
      ("rt_rgba", RenderTarget RGBA)
    ; ("rt_ds", RenderTarget DS)
    ; (* Built-in vector types *)
      ("bvec2", generate_vector_type 2 "bool" "bvec")
    ; ("bvec3", generate_vector_type 3 "bool" "bvec")
    ; ("bvec4", generate_vector_type 4 "bool" "bvec")
    ; ("ivec2", generate_vector_type 2 "int" "ivec")
    ; ("ivec3", generate_vector_type 3 "int" "ivec")
    ; ("ivec4", generate_vector_type 4 "int" "ivec")
    ; ("uvec2", generate_vector_type 2 "uint" "uvec")
    ; ("uvec3", generate_vector_type 3 "uint" "uvec")
    ; ("uvec4", generate_vector_type 4 "uint" "uvec")
    ; ("fvec2", generate_vector_type 2 "float" "fvec")
    ; ("fvec3", generate_vector_type 3 "float" "fvec")
    ; ("fvec4", generate_vector_type 4 "float" "fvec")
    ; ("dvec2", generate_vector_type 2 "double" "dvec")
    ; ("dvec3", generate_vector_type 3 "double" "dvec")
    ; ("dvec4", generate_vector_type 4 "double" "dvec")
    ; (* Built-in matrix types *)
      ("fmat2", Array (TypeRef "float", [OfInt 2; OfInt 2]))
    ; ("fmat3", Array (TypeRef "float", [OfInt 3; OfInt 3]))
    ; ("fmat4", Array (TypeRef "float", [OfInt 4; OfInt 4]))
    ; ("fmat2x2", Array (TypeRef "float", [OfInt 2; OfInt 2]))
    ; ("fmat2x3", Array (TypeRef "float", [OfInt 2; OfInt 3]))
    ; ("fmat2x4", Array (TypeRef "float", [OfInt 2; OfInt 4]))
    ; ("fmat3x2", Array (TypeRef "float", [OfInt 3; OfInt 2]))
    ; ("fmat3x3", Array (TypeRef "float", [OfInt 3; OfInt 3]))
    ; ("fmat3x4", Array (TypeRef "float", [OfInt 3; OfInt 4]))
    ; ("fmat4x2", Array (TypeRef "float", [OfInt 4; OfInt 2]))
    ; ("fmat4x3", Array (TypeRef "float", [OfInt 4; OfInt 3]))
    ; ("fmat4x4", Array (TypeRef "float", [OfInt 4; OfInt 4]))
    ; ("dmat2", Array (TypeRef "double", [OfInt 2; OfInt 2]))
    ; ("dmat3", Array (TypeRef "double", [OfInt 3; OfInt 3]))
    ; ("dmat4", Array (TypeRef "double", [OfInt 4; OfInt 4]))
    ; ("dmat2x2", Array (TypeRef "double", [OfInt 2; OfInt 2]))
    ; ("dmat2x3", Array (TypeRef "double", [OfInt 2; OfInt 3]))
    ; ("dmat2x4", Array (TypeRef "double", [OfInt 2; OfInt 4]))
    ; ("dmat3x2", Array (TypeRef "double", [OfInt 3; OfInt 2]))
    ; ("dmat3x3", Array (TypeRef "double", [OfInt 3; OfInt 3]))
    ; ("dmat3x4", Array (TypeRef "double", [OfInt 3; OfInt 4]))
    ; ("dmat4x2", Array (TypeRef "double", [OfInt 4; OfInt 2]))
    ; ("dmat4x3", Array (TypeRef "double", [OfInt 4; OfInt 3]))
    ; ("dmat4x4", Array (TypeRef "double", [OfInt 4; OfInt 4]))
    ; (* Built-in opaque types *)
      ("sampler", Record [])
    ; ("sampler2D", Record [])
    ; ("texture2D", generate_texture_type 2 "sampler2D")
    ; ( "depthBuffer"
      , Record
          [("load", Function ([("p", TypeRef "ivec2")], [TypeRef "float"]))] )
    ]
  in
  let builtin_functions =
    [ (* Primitive types *)
      ("uint", generate_constructor_type "int" "i" "uint")
    ; ("double", generate_constructor_type "float" "f" "double")
    ; (* Built-in vector types *)
      ("bvec2", generate_constructor_type "bool" "xy" "bvec2")
    ; ("bvec3", generate_constructor_type "bool" "xyz" "bvec3")
    ; ("bvec4", generate_constructor_type "bool" "xyzw" "bvec4")
    ; ("ivec2", generate_constructor_type "int" "xy" "ivec2")
    ; ("ivec3", generate_constructor_type "int" "xyz" "ivec3")
    ; ("ivec4", generate_constructor_type "int" "xyzw" "ivec4")
    ; ("uvec2", generate_constructor_type "uint" "xy" "uvec2")
    ; ("uvec3", generate_constructor_type "uint" "xyz" "uvec3")
    ; ("uvec4", generate_constructor_type "uint" "xyzw" "uvec4")
    ; ("fvec2", generate_constructor_type "float" "xy" "fvec2")
    ; ("fvec3", generate_constructor_type "float" "xyz" "fvec3")
    ; ("fvec4", generate_constructor_type "float" "xyzw" "fvec4")
    ; ("dvec2", generate_constructor_type "double" "xy" "dvec2")
    ; ("dvec3", generate_constructor_type "double" "xyz" "dvec3")
    ; ("dvec4", generate_constructor_type "double" "xyzw" "dvec4")
      (* TODO: Built-in matrix types *)
     ]
  in
  let env =
    List.fold_left
      (fun env (name, t) -> add_type name {loc= builtin_loc; value= t} env)
      (empty "global") builtin_types
  in
  let env =
    List.fold_left
      (fun env (name, f) -> add_function name {loc= builtin_loc; value= f} env)
      env builtin_functions
  in
  env

(* Scope *)
let scope_summary env = env.summary

let enter_module_scope id env =
  let id = "module$" ^ id in
  {(empty id) with summary= Module id; parent= Some env}

let enter_pipeline_scope id typ env =
  let id = "pipeline$" ^ id in
  {(empty id) with summary= Pipeline typ; parent= Some env}

let enter_renderer_scope id typ env =
  let id = "renderer$" ^ id in
  {(empty id) with summary= Renderer typ; parent= Some env}

let enter_function_scope id typ env =
  let id = "function$" ^ id in
  {(empty id) with summary= Function typ; parent= Some env}

let enter_block_scope id loc env =
  let id = "block$id" ^ id in
  {(empty id) with summary= Block loc; parent= Some env}

let exit_scope env =
  match env.parent with
  | Some penv ->
      penv
  | None ->
      failwith (Printf.sprintf "Trying to exit root scope: %s" env.id)

let rec is_pipeline_scope env =
  match env.summary with
  | Pipeline _ ->
      true
  | _ -> (
    match env.parent with Some env -> is_pipeline_scope env | None -> false )

let rec is_renderer_scope env =
  match env.summary with
  | Renderer _ ->
      true
  | _ -> (
    match env.parent with Some env -> is_renderer_scope env | None -> false )

let rec is_function_scope env =
  match env.summary with
  | Function _ ->
      true
  | _ -> (
    match env.parent with Some env -> is_function_scope env | None -> false )

(* Others *)
let add_builtin fname env =
  let open Type in
  match env.parent with
  | Some penv -> (
    match (scope_summary penv, fname) with
    | Pipeline _, "vertex" ->
        let t =
          Record
            [ ("position", TypeRef "fvec4")
            ; ("vertexID", TypeRef "int")
            ; ("instanceID", TypeRef "int") ]
        in
        add_var "builtin" L.{loc= builtin_loc; value= t} env
    | Pipeline _, "fragment" ->
        let t =
          Record
            [("fragCoord", TypeRef "fvec4"); ("frontFacing", TypeRef "bool")]
        in
        add_var "builtin" L.{loc= builtin_loc; value= t} env
    | Renderer _, "main" ->
        let t = Record [("screen", TypeRef "rt_rgba")] in
        add_var "builtin" L.{loc= builtin_loc; value= t} env
    | _ ->
        env )
  | None ->
      failwith
        "builtin must be added in function scopes, which should be nested in \
         pipeline or renderer scopes"

(* Printing *)
let filter_global env =
  SymbolTable.(
    let skip src k _ = not (mem k src) in
    { env with
      types= SymbolTable.(filter (skip global.types) env.types)
    ; constants= SymbolTable.(filter (skip global.constants) env.constants)
    ; vars= SymbolTable.(filter (skip global.vars) env.vars)
    ; pipelines= SymbolTable.(filter (skip global.pipelines) env.pipelines)
    ; functions= SymbolTable.(filter (skip global.functions) env.functions) })

let to_yojson t = to_yojson (filter_global t)

let string_of_env env = Yojson.Safe.pretty_to_string (to_yojson env)
