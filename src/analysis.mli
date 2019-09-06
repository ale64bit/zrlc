(**
   Copyright 2019 Google LLC

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

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
  | `InvalidCast of Type.t * Type.t
  | `NotAnExpression of string
  | `NoSuchMember of Type.t * string
  | `NotEnoughReturnArguments of Type.t list * Type.t list
  | `TooManyReturnArguments of Type.t list * Type.t list
  | `NotEnoughIndices of Ast.expression * int * int
  | `TooManyIndices of Ast.expression * int * int
  | `MultipleValueInSingleValueContext of Ast.expression
  | `MixedArgumentStyle of Ast.expression
  | `InvalidArgument of Ast.expression * Type.t * Type.t * string
  | `InvalidReturnArgument of Ast.expression * Type.t * Type.t
  | `MissingNamedArgument of string * string
  | `UnexpectedNamedArgument of string * string
  | `UnitUsedAsValue of Ast.expression
  | `NotAnLValue of Ast.expression
  | `InvalidSingleAssignment of Ast.expression * Type.t * Type.t
  | `InvalidMultipleAssignment of Type.t * Ast.expression * Type.t
  | `NonBoolIfCondition of Ast.expression * Type.t
  | `CannotRangeOver of Ast.expression * Type.t
  | `NonIntegerRangeExpression of Ast.expression * Type.t
  | `MissingReturn of string
  | `NoMatchingFunction of string * Type.t list ]

val check : Ast.root -> (TypedAst.root, [> error ] Located.t) result

val check_expr :
  Env.t -> Ast.expression -> (Type.t list, [> error ] Located.t) result

val check_single_value_expr :
  Env.t -> Ast.expression -> (Type.t, [> error ] Located.t) result
