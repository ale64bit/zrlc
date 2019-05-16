type error =
  [ `Redefinition of string * (Lexing.position * Lexing.position)
  | `DuplicateMember of string
  | `DuplicateParameter of string
  | `Unimplemented of string
  | `UnknownTypeName of string
  | `NonIntegerArraySize
  | `NonIntegerArrayIndex of Ast.expression
  | `UndeclaredIdentifier of string
  | `AssignmentMismatch of int * int
  | `InvalidUnaryOperation of Ast.unop * Type.t
  | `InvalidBinaryOperation of Ast.expression * Type.t * Type.t
  | `InvalidIndexOperation of Ast.expression * Type.t
  | `InvalidCallOperation of Ast.expression * Type.t
  | `NotAnExpression of string
  | `NoSuchMember of Type.t * string
  | `NotEnoughArguments of Ast.expression * Type.t list * Type.t list
  | `TooManyArguments of Ast.expression * Type.t list * Type.t list
  | `NotEnoughIndices of Ast.expression * int * int
  | `TooManyIndices of Ast.expression * int * int
  | `MultipleValueInSingleValueContext of Ast.expression
  | `MixedArgumentStyle of Ast.expression ]

val check : Ast.root -> (TypedAst.root, [> error] Located.t) result

val check_expr :
  Env.t -> Ast.expression -> (Type.t list, [> error] Located.t) result

val check_stmt :
     Env.t
  -> Lexing.position * Lexing.position
  -> Ast.raw_stmt
  -> (Env.t * TypedAst.stmt list, [> error] Located.t) result
