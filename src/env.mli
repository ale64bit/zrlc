
type scope 
type env

val empty : env
val enter_scope : string -> env -> env
val current_scope : env -> string
val exit_scope : env -> env
val get : string -> env -> Type.t
val put : string -> Type.t -> env -> env 

