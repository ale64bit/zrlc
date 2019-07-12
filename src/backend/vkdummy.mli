open Zrl

module Error : sig
  type t = [ `Unsupported of string | `MissingRendererEntryPoint of string ]

  val string_of_error : t Located.t -> string
end

val gen : Config.t -> TypedAst.root -> (unit, [> Error.t ] Located.t) result
