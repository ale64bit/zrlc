type t =
  | Primitive of string
  | Record of field list
  | Array of t * (int list)
  | Function of (field list) * (t list)

and field = {name: string; t: t}

let rec string_of_field f = Printf.sprintf "%s:%s" f.name (string_of_type f.t)
and string_of_type = function
  | Primitive s -> s
  | Record fields -> "{" ^ (String.concat ";" (List.map string_of_field fields)) ^ "}"
  | Array (t, dims) -> 
      Printf.sprintf "[%s;%s]" (string_of_type t) (String.concat ";" (List.map string_of_int dims))
  | Function (_, _) -> "function:TODO"
