type error = [
  | `TypeError of string
  | `SemanticError of string
]

val check : Ast.root -> (TypedAst.root, [> error]) result
