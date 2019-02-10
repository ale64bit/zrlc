type t = 
  | Id of string
  | Int of int
  | Float of float
  | Bool of bool
  | Keyword of string
  | Operator of string

let equal t1 t2 = match t1, t2 with
  | Id a, Id b -> a = b
  | Int a, Int b -> a = b
  | Float a, Float b -> a = b
  | Bool a, Bool b -> a = b
  | Keyword a, Keyword b -> a = b
  | Operator a, Operator b -> a = b
  | _, _ -> false

let to_string tok = match tok with
  | Id s -> "<id:" ^ s ^ ">"
  | Int i -> "<int:" ^ (string_of_int i) ^ ">"
  | Float f -> "<float:" ^ (string_of_float f) ^ ">"
  | Bool b -> "<bool:" ^ (string_of_bool b) ^ ">"
  | Keyword s -> "<kw:" ^ s ^ ">"
  | Operator s -> "<op:" ^ s ^ ">"

