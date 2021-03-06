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

let string_of_error Located.{ loc; value } =
  let pos = Located.string_of_start_position loc in
  match value with
  | `LexerError msg -> Printf.sprintf "%s: error: %s" pos msg
  | `ParserError msg -> Printf.sprintf "%s: error: %s" pos msg
  | `Redefinition (id, prev) ->
      Printf.sprintf
        "%s: error: redefinition of '%s'\n\tpreviously defined at %s" pos id
        (Located.string_of_start_position prev)
  | `DuplicateMember id ->
      Printf.sprintf "%s: error: duplicate member '%s'" pos id
  | `DuplicateParameter id ->
      Printf.sprintf "%s: error: redefinition of parameter '%s'" pos id
  | `Unimplemented msg -> Printf.sprintf "%s: error: %s" pos msg
  | `UnknownTypeName name ->
      Printf.sprintf "%s: error: unknown type name '%s'" pos name
  | `NonIntegerArraySize ->
      Printf.sprintf "%s: error: non-integer array size" pos
  | `NonIntegerArrayIndex expr ->
      Printf.sprintf "%s: error: non-integer array index %s" pos
        (Ast.string_of_expression expr)
  | `UndeclaredIdentifier id ->
      Printf.sprintf "%s: error: undeclared identifier '%s'" pos id
  | `AssignmentMismatch (vars, values) ->
      let var_plural = if vars > 1 then "s" else "" in
      let val_plural = if values > 1 then "s" else "" in
      Printf.sprintf
        "%s: error: assignment mismatch: %d variable%s but %d value%s" pos vars
        var_plural values val_plural
  | `InvalidUnaryOperation (op, typ) ->
      Printf.sprintf "%s: error: invalid operation: %s %s" pos
        (Ast.string_of_unop op) (Type.string_of_type typ)
  | `InvalidBinaryOperation (expr, ltyp, rtyp) ->
      Printf.sprintf
        "%s: error: invalid operation: %s (mismatched types %s and %s)" pos
        (Ast.string_of_expression expr)
        (Type.string_of_type ltyp) (Type.string_of_type rtyp)
  | `InvalidIndexOperation (expr, typ) ->
      Printf.sprintf
        "%s: error: invalid operation: %s (type %s does not support indexing)"
        pos
        (Ast.string_of_expression expr)
        (Type.string_of_type typ)
  | `InvalidCallOperation (expr, typ) ->
      Printf.sprintf "%s: error: cannot call non-function %s (type %s)" pos
        (Ast.string_of_expression expr)
        (Type.string_of_type typ)
  | `InvalidCast (from_type, to_type) ->
      Printf.sprintf "%s: error: cast from '%s' to '%s' is not allowed" pos
        (Type.string_of_type from_type)
        (Type.string_of_type to_type)
  | `NotAnExpression id ->
      Printf.sprintf "%s: error: %s is not an expression" pos id
  | `NoSuchMember (typ, id) ->
      Printf.sprintf "%s: error: type %s has no member %s" pos
        (Type.string_of_type typ) id
  | `NotEnoughReturnArguments (have, want) ->
      Printf.sprintf
        "%s: error: not enough arguments to return\n\thave (%s)\n\twant (%s)"
        pos
        (String.concat ", " (List.map Type.string_of_type have))
        (String.concat ", " (List.map Type.string_of_type want))
  | `TooManyReturnArguments (have, want) ->
      Printf.sprintf
        "%s: error: too many arguments to return\n\thave (%s)\n\twant (%s)" pos
        (String.concat ", " (List.map Type.string_of_type have))
        (String.concat ", " (List.map Type.string_of_type want))
  | `NotEnoughIndices (expr, have, want) ->
      Printf.sprintf
        "%s: error: not enough indices when accessing %s: have %d, want %d" pos
        (Ast.string_of_expression expr)
        have want
  | `TooManyIndices (expr, have, want) ->
      Printf.sprintf
        "%s: error: too many indices when accessing %s: have %d, want %d" pos
        (Ast.string_of_expression expr)
        have want
  | `MultipleValueInSingleValueContext expr ->
      Printf.sprintf "%s: error: multiple-value %s in single-value context" pos
        (Ast.string_of_expression expr)
  | `MixedArgumentStyle expr ->
      Printf.sprintf
        "%s: error: cannot use both named and unnamed argument style in %s" pos
        (Ast.string_of_expression expr)
  | `InvalidArgument (expr, have, want, f) ->
      Printf.sprintf
        "%s: error: cannot use %s (type %s) as type %s in argument to %s" pos
        (Ast.string_of_expression expr)
        (Type.string_of_type have) (Type.string_of_type want) f
  | `InvalidReturnArgument (expr, have, want) ->
      Printf.sprintf
        "%s: error: cannot use %s (type %s) as type %s in return argument" pos
        (Ast.string_of_expression expr)
        (Type.string_of_type have) (Type.string_of_type want)
  | `MissingNamedArgument (name, f) ->
      Printf.sprintf "%s: error: missing named argument '%s' in call to %s" pos
        name f
  | `UnexpectedNamedArgument (name, f) ->
      Printf.sprintf "%s: error: unexpected named argument '%s' in call to %s"
        pos name f
  | `UnitUsedAsValue expr ->
      Printf.sprintf "%s: error: %s used as value" pos
        (Ast.string_of_expression expr)
  | `NotAnLValue expr ->
      Printf.sprintf "%s: error: cannot assign to %s" pos
        (Ast.string_of_expression expr)
  | `InvalidSingleAssignment (expr, have, want) ->
      Printf.sprintf
        "%s: error: cannot use %s (type %s) as type %s in assignment" pos
        (Ast.string_of_expression expr)
        (Type.string_of_type have) (Type.string_of_type want)
  | `InvalidMultipleAssignment (have, expr, want) ->
      Printf.sprintf
        "%s: error: cannot assign %s to %s (type %s) in multiple assignment"
        pos (Type.string_of_type have)
        (Ast.string_of_expression expr)
        (Type.string_of_type want)
  | `NonBoolIfCondition (expr, t) ->
      Printf.sprintf "%s: error: non-bool %s (type %s) used as if condition"
        pos
        (Ast.string_of_expression expr)
        (Type.string_of_type t)
  | `CannotRangeOver (expr, t) ->
      Printf.sprintf "%s: error: cannot range over %s (type %s)" pos
        (Ast.string_of_expression expr)
        (Type.string_of_type t)
  | `NonIntegerRangeExpression (expr, t) ->
      Printf.sprintf "%s: error: non-integer range expression %s (type %s)" pos
        (Ast.string_of_expression expr)
        (Type.string_of_type t)
  | `MissingReturn fname ->
      Printf.sprintf "%s: error: missing return in non-void function %s" pos
        fname
  | `NoMatchingFunction (fname, candidates) ->
      let notes =
        String.concat "\n"
          (List.map (fun t -> "\t" ^ Type.string_of_type t) candidates)
      in
      if String.length notes = 0 then
        Printf.sprintf "%s: error: no matching function for call to '%s'." pos
          fname
      else
        Printf.sprintf
          "%s: error: no matching function for call to '%s'. Candidates:\n%s"
          pos fname notes

let debug_string_of_error Located.{ loc; value } =
  let loc = Located.string_of_location loc in
  match value with
  | `LexerError msg -> Printf.sprintf "%s: LexerError msg=%s" loc msg
  | `ParserError msg -> Printf.sprintf "%s: ParserError msg=%s" loc msg
  | `Redefinition (id, prev) ->
      Printf.sprintf "%s: Redefinition id=%s prev=%s" loc id
        (Located.string_of_start_position prev)
  | `DuplicateMember id -> Printf.sprintf "%s: DuplicateMember id=%s" loc id
  | `DuplicateParameter id ->
      Printf.sprintf "%s: DuplicateParameter id=%s" loc id
  | `Unimplemented msg -> Printf.sprintf "%s: Unimplemented msg=%s" loc msg
  | `UnknownTypeName name ->
      Printf.sprintf "%s: UnknownTypeName name=%s" loc name
  | `NonIntegerArraySize -> Printf.sprintf "%s: NonIntegerArraySize" loc
  | `NonIntegerArrayIndex expr ->
      Printf.sprintf "%s: NonIntegerArrayIndex expr=%s" loc
        (Ast.string_of_expression expr)
  | `UndeclaredIdentifier id ->
      Printf.sprintf "%s: UndeclaredIdentifier id=%s" loc id
  | `AssignmentMismatch (vars, values) ->
      Printf.sprintf "%s: AssignmentMismatch (vars=%d, values=%d)" loc vars
        values
  | `InvalidUnaryOperation (op, typ) ->
      Printf.sprintf "%s: InvalidUnaryOperation (op=%s, typ=%s)" loc
        (Ast.string_of_unop op) (Type.string_of_type typ)
  | `InvalidBinaryOperation (expr, ltyp, rtyp) ->
      Printf.sprintf "%s: InvalidBinaryOperation (expr=%s, ltyp=%s, rtyp=%s)"
        loc
        (Ast.string_of_expression expr)
        (Type.string_of_type ltyp) (Type.string_of_type rtyp)
  | `InvalidIndexOperation (expr, typ) ->
      Printf.sprintf "%s: InvalidIndexOperation (expr=%s, typ=%s)" loc
        (Ast.string_of_expression expr)
        (Type.string_of_type typ)
  | `InvalidCallOperation (expr, typ) ->
      Printf.sprintf "%s: InvalidCallOperation (expr=%s, typ=%s)" loc
        (Ast.string_of_expression expr)
        (Type.string_of_type typ)
  | `InvalidCast (from_type, to_type) ->
      Printf.sprintf "%s: InvalidCast (from_type=%s, to_type=%s)" loc
        (Type.string_of_type from_type)
        (Type.string_of_type to_type)
  | `NotAnExpression id -> Printf.sprintf "%s: NotAnExpression id=%s" loc id
  | `NoSuchMember (typ, id) ->
      Printf.sprintf "%s: NoSuchMember (typ=%s, id=%s)" loc
        (Type.string_of_type typ) id
  | `NotEnoughReturnArguments (have, want) ->
      Printf.sprintf "%s: NotEnoughReturnArguments (have=%s, want=%s)" loc
        (String.concat ", " (List.map Type.string_of_type have))
        (String.concat ", " (List.map Type.string_of_type want))
  | `TooManyReturnArguments (have, want) ->
      Printf.sprintf "%s: TooManyReturnArguments (have=%s, want=%s)" loc
        (String.concat ", " (List.map Type.string_of_type have))
        (String.concat ", " (List.map Type.string_of_type want))
  | `NotEnoughIndices (expr, have, want) ->
      Printf.sprintf "%s: NotEnoughIndices (expr=%s, have=%d, want=%d)" loc
        (Ast.string_of_expression expr)
        have want
  | `TooManyIndices (expr, have, want) ->
      Printf.sprintf "%s: TooManyIndices (expr=%s, have=%d, want=%d)" loc
        (Ast.string_of_expression expr)
        have want
  | `MultipleValueInSingleValueContext expr ->
      Printf.sprintf "%s: MultipleValueInSingleValueContext expr=%s" loc
        (Ast.string_of_expression expr)
  | `MixedArgumentStyle expr ->
      Printf.sprintf "%s: MixedArgumentStyle expr=%s" loc
        (Ast.string_of_expression expr)
  | `InvalidArgument (expr, have, want, f) ->
      Printf.sprintf "%s: InvalidArgument (expr=%s, have=%s, want=%s, f=%s)"
        loc
        (Ast.string_of_expression expr)
        (Type.string_of_type have) (Type.string_of_type want) f
  | `InvalidReturnArgument (expr, have, want) ->
      Printf.sprintf "%s: InvalidReturnArgument (expr=%s, have=%s, want=%s)"
        loc
        (Ast.string_of_expression expr)
        (Type.string_of_type have) (Type.string_of_type want)
  | `MissingNamedArgument (name, f) ->
      Printf.sprintf "%s: MissingNamedArgument (name=%s, f=%s)" loc name f
  | `UnexpectedNamedArgument (name, f) ->
      Printf.sprintf "%s: UnexpectedNamedArgument (name=%s, f=%s)" loc name f
  | `UnitUsedAsValue expr ->
      Printf.sprintf "%s: UnitUsedAsValue expr=%s" loc
        (Ast.string_of_expression expr)
  | `NotAnLValue expr ->
      Printf.sprintf "%s: NotAnLValue expr=%s" loc
        (Ast.string_of_expression expr)
  | `InvalidSingleAssignment (expr, have, want) ->
      Printf.sprintf "%s: InvalidSingleAssignment (expr=%s, have=%s, want=%s)"
        loc
        (Ast.string_of_expression expr)
        (Type.string_of_type have) (Type.string_of_type want)
  | `InvalidMultipleAssignment (have, expr, want) ->
      Printf.sprintf
        "%s: InvalidMultipleAssignment (expr=%s, have=%s, want=%s)" loc
        (Type.string_of_type have)
        (Ast.string_of_expression expr)
        (Type.string_of_type want)
  | `NonBoolIfCondition (expr, t) ->
      Printf.sprintf "%s: NonBoolIfCondition (expr=%s, t=%s)" loc
        (Ast.string_of_expression expr)
        (Type.string_of_type t)
  | `CannotRangeOver (expr, t) ->
      Printf.sprintf "%s: CannotRangeOver (expr=%s, t=%s)" loc
        (Ast.string_of_expression expr)
        (Type.string_of_type t)
  | `NonIntegerRangeExpression (expr, t) ->
      Printf.sprintf "%s: NonIntegerRangeExpression (expr=%s, t=%s)" loc
        (Ast.string_of_expression expr)
        (Type.string_of_type t)
  | `MissingReturn fname ->
      Printf.sprintf "%s: MissingReturn fname=%s" loc fname
  | `NoMatchingFunction (fname, candidates) ->
      let candidates =
        String.concat ", " (List.map Type.string_of_type candidates)
      in
      Printf.sprintf "%s: NoMatchingFunction: (fname=%s, candidates=[%s])" loc
        fname candidates
