type const_value = 
  | Bool of bool
  | Int of int
  | Float of float

type t

val empty : string -> t
val constant_exists : string -> t -> bool
val type_exists : string -> t -> bool
val add_constant : string -> const_value -> t -> t
val add_type : string -> Type.t -> t -> t
val string_of_env : t -> string
