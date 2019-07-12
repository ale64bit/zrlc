type t

val empty : string -> t

val load : string -> string -> t -> t

val add_cc_library : Cpp.Library.t -> t -> t

val add_glsl_library : Glsl.Library.t -> t -> t

val write_to : string -> t -> unit
