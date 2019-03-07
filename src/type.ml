type t =
  | TypeRef of string
  | Record of field list
  | Array of t * const list
  | Function of field list * t list
  | Primitive of primitive_type

and primitive_type = 
  | Bool | Int | UInt | Float | Double

and field = {name: string; t: t}

and const =
  | OfBool of bool
  | OfInt of int
  | OfFloat of float
  | OfName of string

let string_of_const = function
  | OfBool b -> Printf.sprintf "<const_bool:%b>" b
  | OfInt i -> Printf.sprintf "<const_int:%d>" i
  | OfFloat f -> Printf.sprintf "<const_float:%f>" f
  | OfName s -> Printf.sprintf "<const_ref:%s>" s

let rec string_of_field f = 
  Printf.sprintf "%s:%s" f.name (string_of_type f.t)

and string_of_primitive = function
  | Bool -> "bool"
  | Int -> "int"
  | UInt -> "uint"
  | Float -> "float"
  | Double -> "double"

and string_of_type = function
  | TypeRef s -> "&" ^ s
  | Record fields -> "{" ^ (String.concat ";" (List.map string_of_field fields)) ^ "}"
  | Array (t, dims) -> 
      let dims = List.map string_of_const dims in
      Printf.sprintf "[%s;%s]" (string_of_type t) (String.concat ";" dims)
  | Function (args, ret) -> 
      let args = List.map string_of_field args in
      let ret = List.map string_of_type ret in
      Printf.sprintf "function (%s):(%s)" (String.concat "," args) (String.concat "," ret)
  | Primitive p -> string_of_primitive p
