type t =
  | TypeRef of string
  | Record of field list
  | Array of t * const list
  | Function of field list * t list
  | Primitive of primitive_type
[@@deriving to_yojson]

and primitive_type = Bool | Int | UInt | Float | Double
[@@deriving to_yojson]

and field = {name: string; t: t} [@@deriving to_yojson]

and const =
  | OfBool of bool
  | OfInt of int
  | OfFloat of float
  | OfName of string
[@@deriving to_yojson]

let rec string_of_type = function
  | TypeRef name ->
      name
  | Record fields ->
      let field_strs =
        List.map
          (fun {name; t} -> Printf.sprintf "%s: %s" name (string_of_type t))
          fields
      in
      "record { " ^ String.concat "; " field_strs ^ " }"
  | Array (t, dims) ->
      let dim_strs =
        List.map
          (function
            | OfBool b ->
                string_of_bool b
            | OfInt i ->
                string_of_int i
            | OfFloat f ->
                string_of_float f
            | OfName name ->
                name )
          dims
      in
      Printf.sprintf "[%s]%s" (String.concat "; " dim_strs) (string_of_type t)
  | Function (args, rets) ->
      let arg_strs =
        List.map
          (fun {name; t} -> Printf.sprintf "%s: %s" name (string_of_type t))
          args
      in
      let ret_strs = List.map string_of_type rets in
      Printf.sprintf "fun (%s) -> (%s)"
        (String.concat ", " arg_strs)
        (String.concat ", " ret_strs)
  | Primitive Bool ->
      "bool"
  | Primitive Int ->
      "int"
  | Primitive UInt ->
      "uint"
  | Primitive Float ->
      "float"
  | Primitive Double ->
      "double"
