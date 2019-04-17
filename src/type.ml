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
