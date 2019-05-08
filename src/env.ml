module SymbolTable = Map.Make (String)

type const_value = Bool of bool | Int of int | Float of float
[@@deriving to_yojson]

type located_type = Type.t Located.t [@@deriving to_yojson]

type located_const_value = const_value Located.t [@@deriving to_yojson]

type type_symbol_table = located_type SymbolTable.t

type const_value_symbol_table = located_const_value SymbolTable.t

let type_symbol_table_to_yojson st =
  let f (k, v) = (k, located_type_to_yojson v) in
  `Assoc (List.map f (SymbolTable.bindings st))

let const_value_symbol_table_to_yojson st =
  let f (k, v) = (k, located_const_value_to_yojson v) in
  `Assoc (List.map f (SymbolTable.bindings st))

type t =
  { id: string
  ; parent: t option
  ; types: type_symbol_table
  ; constants: const_value_symbol_table
  ; vars: type_symbol_table
  ; pipelines: type_symbol_table
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
      {Type.name; t} )
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
  | Some Located.{loc; value= Bool _} ->
      Some Located.{loc; value= Type.TypeRef "bool"}
  | Some Located.{loc; value= Int _} ->
      Some Located.{loc; value= Type.TypeRef "int"}
  | Some Located.{loc; value= Float _} ->
      Some Located.{loc; value= Type.TypeRef "float"}
  | None ->
      None

let find_type ~local name env =
  let f env = env.types in
  find ~local name env f

let find_pipeline ~local name env =
  let f env = env.pipelines in
  find ~local name env f

let find_function ~local name env =
  let f env = env.functions in
  find ~local name env f

let find_var ~local name env =
  let f env = env.vars in
  find ~local name env f

let find_name ~local name env =
  let open Monad.Option in
  find_constant_type ~local name env
  >>? find_type ~local name env >>? find_var ~local name env
  >>? find_function ~local name env
  >>? find_pipeline ~local name env

let find_lvalue name env = find_var ~local:false name env

let find_rvalue name env =
  let open Monad.Option in
  find_constant_type ~local:false name env
  >>? find_var ~local:false name env
  >>? find_function ~local:false name env
  >>? find_pipeline ~local:false name env

(* Exists *)
let constant_exists name env = find_constant ~local:false name env <> None

let type_exists name env = find_type ~local:false name env <> None

let pipeline_exists name env = find_pipeline ~local:false name env <> None

let function_exists name env = find_function ~local:false name env <> None

let var_exists name env = find_var ~local:false name env <> None

(* Add *)

let add_constant name value env =
  let () = assert (not (constant_exists name env)) in
  {env with constants= SymbolTable.add name value env.constants}

let add_type name typ env =
  ( match typ.Located.value with
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

let add_function name typ env =
  let () = assert (not (function_exists name env)) in
  {env with functions= SymbolTable.add name typ env.functions}

let add_var name typ env =
  let () = assert (not (var_exists name env)) in
  {env with vars= SymbolTable.add name typ env.vars}

(* Constructors *)
let empty id =
  { id
  ; parent= None
  ; types= SymbolTable.empty
  ; constants= SymbolTable.empty
  ; vars= SymbolTable.empty
  ; pipelines= SymbolTable.empty
  ; functions= SymbolTable.empty }

let global =
  let builtin_pos =
    Lexing.{pos_fname= "builtin"; pos_lnum= 0; pos_cnum= 0; pos_bol= 0}
  in
  let builtin_loc = (builtin_pos, builtin_pos) in
  let builtins =
    [ (* Primitive types *)
      ("bool", Type.Primitive Bool)
    ; ("int", Type.Primitive Int)
    ; ("uint", Type.Primitive UInt)
    ; ("float", Type.Primitive Float)
    ; ("double", Type.Primitive Double)
    ; (* Built-in vector types *)
      ("bvec2", Type.Record (generate_fields 2 "bool" "bvec"))
    ; ("bvec3", Type.Record (generate_fields 3 "bool" "bvec"))
    ; ("bvec4", Type.Record (generate_fields 4 "bool" "bvec"))
    ; ("ivec2", Type.Record (generate_fields 2 "int" "ivec"))
    ; ("ivec3", Type.Record (generate_fields 3 "int" "ivec"))
    ; ("ivec4", Type.Record (generate_fields 4 "int" "ivec"))
    ; ("uvec2", Type.Record (generate_fields 2 "uint" "uvec"))
    ; ("uvec3", Type.Record (generate_fields 3 "uint" "uvec"))
    ; ("uvec4", Type.Record (generate_fields 4 "uint" "uvec"))
    ; ("vec2", Type.Record (generate_fields 2 "float" "vec"))
    ; ("vec3", Type.Record (generate_fields 3 "float" "vec"))
    ; ("vec4", Type.Record (generate_fields 4 "float" "vec"))
    ; ("dvec2", Type.Record (generate_fields 2 "double" "dvec"))
    ; ("dvec3", Type.Record (generate_fields 3 "double" "dvec"))
    ; ("dvec4", Type.Record (generate_fields 4 "double" "dvec"))
    ; (* Built-in matrix types *)
      ("mat2", Type.Array (Type.TypeRef "float", [OfInt 2; OfInt 2]))
    ; ("mat3", Type.Array (Type.TypeRef "float", [OfInt 3; OfInt 3]))
    ; ("mat4", Type.Array (Type.TypeRef "float", [OfInt 4; OfInt 4]))
    ; ("mat2x2", Type.Array (Type.TypeRef "float", [OfInt 2; OfInt 2]))
    ; ("mat2x3", Type.Array (Type.TypeRef "float", [OfInt 2; OfInt 3]))
    ; ("mat2x4", Type.Array (Type.TypeRef "float", [OfInt 2; OfInt 4]))
    ; ("mat3x2", Type.Array (Type.TypeRef "float", [OfInt 3; OfInt 2]))
    ; ("mat3x3", Type.Array (Type.TypeRef "float", [OfInt 3; OfInt 3]))
    ; ("mat3x4", Type.Array (Type.TypeRef "float", [OfInt 3; OfInt 4]))
    ; ("mat4x2", Type.Array (Type.TypeRef "float", [OfInt 4; OfInt 2]))
    ; ("mat4x3", Type.Array (Type.TypeRef "float", [OfInt 4; OfInt 3]))
    ; ("mat4x4", Type.Array (Type.TypeRef "float", [OfInt 4; OfInt 4]))
    ; ("dmat2", Type.Array (Type.TypeRef "double", [OfInt 2; OfInt 2]))
    ; ("dmat3", Type.Array (Type.TypeRef "double", [OfInt 3; OfInt 3]))
    ; ("dmat4", Type.Array (Type.TypeRef "double", [OfInt 4; OfInt 4]))
    ; ("dmat2x2", Type.Array (Type.TypeRef "double", [OfInt 2; OfInt 2]))
    ; ("dmat2x3", Type.Array (Type.TypeRef "double", [OfInt 2; OfInt 3]))
    ; ("dmat2x4", Type.Array (Type.TypeRef "double", [OfInt 2; OfInt 4]))
    ; ("dmat3x2", Type.Array (Type.TypeRef "double", [OfInt 3; OfInt 2]))
    ; ("dmat3x3", Type.Array (Type.TypeRef "double", [OfInt 3; OfInt 3]))
    ; ("dmat3x4", Type.Array (Type.TypeRef "double", [OfInt 3; OfInt 4]))
    ; ("dmat4x2", Type.Array (Type.TypeRef "double", [OfInt 4; OfInt 2]))
    ; ("dmat4x3", Type.Array (Type.TypeRef "double", [OfInt 4; OfInt 3]))
    ; ("dmat4x4", Type.Array (Type.TypeRef "double", [OfInt 4; OfInt 4])) ]
  in
  List.fold_left
    (fun env (name, t) -> add_type name {loc= builtin_loc; value= t} env)
    (empty "global") builtins

(* Scope *)
let enter_module_scope id env =
  let id = "module$" ^ id in
  {(empty id) with parent= Some env}

let enter_pipeline_scope id env =
  let id = "pipeline$" ^ id in
  {(empty id) with parent= Some env}

let enter_renderer_scope id env =
  let id = "renderer$" ^ id in
  {(empty id) with parent= Some env}

let enter_function_scope id env =
  let id = "function$" ^ id in
  {(empty id) with parent= Some env}

let exit_scope env =
  match env.parent with
  | Some penv ->
      penv
  | None ->
      failwith (Printf.sprintf "Trying to exit root scope: %s" env.id)

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
