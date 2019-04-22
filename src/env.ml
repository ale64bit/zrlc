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
  ; types: type_symbol_table
  ; constants: const_value_symbol_table
  ; vars: type_symbol_table
  ; pipelines: type_symbol_table
  ; functions: type_symbol_table }
[@@deriving to_yojson]

let empty id =
  { id
  ; types= SymbolTable.empty
  ; constants= SymbolTable.empty
  ; vars= SymbolTable.empty
  ; pipelines= SymbolTable.empty
  ; functions= SymbolTable.empty }

let constant_exists name env = SymbolTable.mem name env.constants

let type_exists name env = SymbolTable.mem name env.types

let pipeline_exists name env = SymbolTable.mem name env.pipelines

let function_exists name env = SymbolTable.mem name env.functions

let var_exists name env = SymbolTable.mem name env.vars

let add_constant name value env =
  {env with constants= SymbolTable.add name value env.constants}

let add_type name typ env = {env with types= SymbolTable.add name typ env.types}

let add_pipeline name typ env =
  {env with pipelines= SymbolTable.add name typ env.pipelines}

let add_function name typ env =
  {env with functions= SymbolTable.add name typ env.functions}

let add_var name typ env = {env with vars= SymbolTable.add name typ env.vars}

let get_constant name env = SymbolTable.find_opt name env.constants

let enter_pipeline_scope id env =
  {env with id= Printf.sprintf "%s::p$%s" env.id id}

let enter_renderer_scope id env =
  {env with id= Printf.sprintf "%s::r$%s" env.id id}

let enter_function_scope id env =
  {env with id= Printf.sprintf "%s::f$%s" env.id id}

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

let global =
  let builtin_pos =
    Lexing.{pos_fname= "builtin"; pos_lnum= 0; pos_cnum= 0; pos_bol= 0}
  in
  let builtin_loc = (builtin_pos, builtin_pos) in
  let builtins =
    [ (* Basic types *)
      ("bool", Type.Primitive Bool)
    ; ("int", Type.Primitive Int)
    ; ("uint", Type.Primitive UInt)
    ; ("float", Type.Primitive Float)
    ; ("double", Type.Primitive Double)
    ; (* Vector types *)
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
    ; (* Matrix types *)
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
