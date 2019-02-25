type error = [
  | `Redefinition of string
  | `DuplicateMember of string * string
  | `Unimplemented of string
]

val check : Ast.root -> (TypedAst.root, [> error]) result
