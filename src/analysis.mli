type error =
  [ `Redefinition of string
  | `DuplicateMember of string * string
  | `DuplicateParameter of string
  | `Unimplemented of string
  | `UnknownTypeName of string
  | `NonIntegerArraySize
  | `UndeclaredIdentifier of string ]

val check : Ast.root -> (TypedAst.root, [> error]) result
