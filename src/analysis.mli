type error =
  [ `Redefinition of string
  | `DuplicateMember of string
  | `DuplicateParameter of string
  | `Unimplemented of string
  | `UnknownTypeName of string
  | `NonIntegerArraySize
  | `UndeclaredIdentifier of string
  | `AssignmentMismatch of int * int ]

val check : Ast.root -> (TypedAst.root, [> error] Located.t) result
