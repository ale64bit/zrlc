module Function : sig
  type t

  val empty : string -> t
  val is_template : t -> bool
  val set_return_type : string -> t -> t
  val set_parent_class : string -> t -> t
  val add_param : (string * string) -> t -> t
  val add_template_param : string -> t -> t
  val add_member_initializer : (string * string) -> t -> t
  val append_code_section : string -> t -> t
  val append_code_sections : string list -> t -> t
  val prepend_code_section : string -> t -> t
  val prepend_code_sections : string list -> t -> t
  val string_of_signature: t -> string
  val string_of_implementation: t -> string
end


module Class : sig
  type t

  val empty : string -> string list -> t
  val name : t -> string
  val add_include : string -> t -> t
  val add_public_function : Function.t -> t -> t
  val add_private_function : Function.t -> t -> t
  val add_private_member : (string * string) -> t -> t
  val add_static_section : string -> t -> t
  val string_of_header: t -> string
  val string_of_source: t -> string
end

module Library : sig
  type t

  val empty : string -> string -> t
  val add_class : Class.t -> t -> t
  val add_dep : string -> t -> t
  val string_of_library : t -> string
end
