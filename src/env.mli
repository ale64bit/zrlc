type const_value = 
  | Bool of bool
  | Int of int
  | Float of float

type t

val global : t
val empty : string -> t
val constant_exists : string -> t -> bool
val type_exists : string -> t -> bool
val pipeline_exists : string -> t -> bool
val function_exists : string -> t -> bool
val var_exists : string -> t -> bool
val add_constant : string -> const_value -> t -> t
val add_type : string -> Type.t -> t -> t
val add_pipeline : string -> Type.t -> t -> t
val add_function : string -> Type.t -> t -> t
val add_var : string -> Type.t -> t -> t
val get_constant : string -> t -> const_value option
val string_of_env : t -> string
