open Zrl

module Error : sig
  type t =
    [ `Unsupported of string
    | `MissingRendererEntryPoint of string
    | `InvalidUniformType of Type.t * string
    | `StageMismatch of string * Type.t list * string * Type.t list
    | `MissingInputBinding of string * string
    | `MultipleDepthBuffers of string ]

  val string_of_error : t Located.t -> string
end

val gen : Config.t -> TypedAst.root -> (unit, [> Error.t ] Located.t) result
