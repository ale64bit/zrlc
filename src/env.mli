type const_value = Bool of bool | Int of int | Float of float
[@@deriving to_yojson]

type t [@@deriving to_yojson]

(* Constructors *)
val global : t

val empty : string -> t

(* Exists *)
val constant_exists : string -> t -> bool

val type_exists : string -> t -> bool

val pipeline_exists : string -> t -> bool

val function_exists : string -> t -> bool

val var_exists : string -> t -> bool

(* Add *)
val add_constant : string -> const_value Located.t -> t -> t

val add_type : string -> Type.t Located.t -> t -> t

val add_pipeline : string -> Type.t Located.t -> t -> t

val add_function : string -> Type.t Located.t -> t -> t

val add_var : string -> Type.t Located.t -> t -> t

(* Scope *)
val enter_pipeline_scope : string -> t -> t

val enter_renderer_scope : string -> t -> t

val enter_function_scope : string -> t -> t

(* Get *)
val get_constant : string -> t -> const_value Located.t option

val get_type : string -> t -> Type.t Located.t option

val get_pipeline : string -> t -> Type.t Located.t option

val get_function : string -> t -> Type.t Located.t option

val get_var : string -> t -> Type.t Located.t option

val find_name : string -> t -> Type.t Located.t option

val string_of_env : t -> string
