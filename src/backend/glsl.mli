module Function : sig
  type t

  val empty : string -> t

  val set_return_type : string -> t -> t

  val add_in_param : string * string -> t -> t

  val add_out_param : string * string -> t -> t

  val add_inout_param : string * string -> t -> t

  val append_code_section : string -> t -> t

  val append_code_sections : string list -> t -> t

  val prepend_code_section : string -> t -> t

  val prepend_code_sections : string list -> t -> t

  val string_of_implementation : t -> string
end

module Shader : sig
  type t

  type stage = Vertex | Geometry | Fragment | Compute

  val empty : string -> stage -> t

  val name : t -> string

  val stage : t -> stage

  val add_struct : string -> t -> t

  val add_uniform : int -> int -> string * string -> t -> t

  val add_input : int -> string * string -> t -> t

  val add_output : int -> string * string -> t -> t

  val add_function : Function.t -> t -> t

  val string_of_source : t -> string
end

module Library : sig
  type t

  val empty : string -> t

  val shaders : t -> Shader.t list

  val add_shader : Shader.t -> t -> t

  val string_of_library : t -> string
end
