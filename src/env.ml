module SymbolTable = Map.Make(String)

type const_value = 
  | Bool of bool
  | Int of int
  | Float of float

type t = {
  id: string;
  types: Type.t SymbolTable.t;
  constants: const_value SymbolTable.t;
  vars: Type.t SymbolTable.t;
  pipelines: Type.t SymbolTable.t;
  functions: Type.t SymbolTable.t;
}

let empty id = 
  {
    id = id; 
    types = SymbolTable.empty;
    constants = SymbolTable.empty;
    vars = SymbolTable.empty;
    pipelines = SymbolTable.empty;
    functions = SymbolTable.empty;
  }

let constant_exists name env = SymbolTable.mem name env.constants

let type_exists name env = SymbolTable.mem name env.types

let pipeline_exists name env = SymbolTable.mem name env.pipelines

let function_exists name env = SymbolTable.mem name env.functions

let add_constant name value env = 
  { env with constants = SymbolTable.add name value env.constants }

let add_type name typ env = 
  { env with types = SymbolTable.add name typ env.types }

let add_pipeline name typ env = 
  { env with pipelines = SymbolTable.add name typ env.pipelines }

let add_function name typ env = 
  { env with functions = SymbolTable.add name typ env.functions }

let get_constant name env = SymbolTable.find_opt name env.constants

let string_of_types m =
  let elems = List.map 
    (fun (k, v) -> Printf.sprintf "    %s = %s" k (Type.string_of_type v)) 
    (SymbolTable.bindings m) in
  ("\n" ^ (String.concat "\n" elems))

let string_of_const_value = function
  | Bool b -> "<bool:" ^ (string_of_bool b) ^ ">"
  | Int i -> "<int:" ^ (string_of_int i) ^ ">"
  | Float f -> "<float:" ^ (string_of_float f) ^ ">"

let string_of_constants m = 
  let elems = List.map 
    (fun (k, v) -> Printf.sprintf "    %s = %s" k (string_of_const_value v)) 
    (SymbolTable.bindings m) in
  ("\n" ^ (String.concat "\n" elems))

let string_of_vars _ = "TODO"

let string_of_env env =
  Printf.sprintf
    {|environment: %s {
  types: %s
  constants: %s
  vars: %s
}|}
  env.id 
  (string_of_types env.types)
  (string_of_constants env.constants)
  (string_of_vars env.vars)
   
