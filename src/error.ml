let string_of_unop = function
  | Ast.UPlus ->
      "+"
  | Ast.UMinus ->
      "-"
  | Ast.LogicalNot ->
      "!"
  | Ast.BitwiseComplement ->
      "~"

let string_of_binop = function _ -> "TODO(binop)"

let string_of_error Located.{loc; value} =
  let pos = Located.string_of_start_position loc in
  match value with
  | `LexerError msg ->
      Printf.sprintf "%s: error: %s" pos msg
  | `ParserError msg ->
      Printf.sprintf "%s: error: %s" pos msg
  | `Redefinition (id, prev) ->
      Printf.sprintf
        "%s: error: redefinition of '%s'\n\tpreviously defined at %s" pos id
        (Located.string_of_start_position prev)
  | `DuplicateMember id ->
      Printf.sprintf "%s: error: duplicate member '%s'" pos id
  | `DuplicateParameter id ->
      Printf.sprintf "%s: error: redefinition of parameter '%s'" pos id
  | `Unimplemented msg ->
      Printf.sprintf "%s: error: %s" pos msg
  | `UnknownTypeName name ->
      Printf.sprintf "%s: error: unknown type name '%s'" pos name
  | `NonIntegerArraySize ->
      Printf.sprintf "%s: error: non-integer array size" pos
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
        (string_of_unop op) (Type.string_of_type typ)
  | `InvalidBinaryOperation (ltyp, op, rtyp) ->
      Printf.sprintf "%s: error: invalid operation: %s %s %s" pos
        (Type.string_of_type ltyp) (string_of_binop op)
        (Type.string_of_type rtyp)

let debug_string_of_error Located.{loc; value} =
  let loc = Located.string_of_location loc in
  match value with
  | `LexerError msg ->
      Printf.sprintf "%s: LexerError msg=%s" loc msg
  | `ParserError msg ->
      Printf.sprintf "%s: ParserError msg=%s" loc msg
  | `Redefinition (id, prev) ->
      Printf.sprintf "%s: Redefinition id=%s prev=%s" loc id
        (Located.string_of_start_position prev)
  | `DuplicateMember id ->
      Printf.sprintf "%s: DuplicateMember id=%s" loc id
  | `DuplicateParameter id ->
      Printf.sprintf "%s: DuplicateParameter id=%s" loc id
  | `Unimplemented msg ->
      Printf.sprintf "%s: Unimplemented msg=%s" loc msg
  | `UnknownTypeName name ->
      Printf.sprintf "%s: UnknownTypeName name=%s" loc name
  | `NonIntegerArraySize ->
      Printf.sprintf "%s: NonIntegerArraySize" loc
  | `UndeclaredIdentifier id ->
      Printf.sprintf "%s: UndeclaredIdentifier id=%s" loc id
  | `AssignmentMismatch (vars, values) ->
      Printf.sprintf "%s: AssignmentMismatch (vars=%d, values=%d)" loc vars
        values
  | `InvalidUnaryOperation (op, typ) ->
      Printf.sprintf "%s: InvalidUnaryOperation (op=%s, typ=%s)" loc
        (string_of_unop op) (Type.string_of_type typ)
  | `InvalidBinaryOperation (ltyp, op, rtyp) ->
      Printf.sprintf "%s: InvalidBinaryOperation (ltyp=%s, op=%s, rtyp=%s)" loc
        (Type.string_of_type ltyp) (string_of_binop op)
        (Type.string_of_type rtyp)
