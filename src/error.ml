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
  | `NotAnExpression id ->
      Printf.sprintf "%s: error: %s is not an expression" pos id
  | `NoSuchMember (typ, id) ->
      Printf.sprintf "%s: error: type %s has no member %s" pos
        (Type.string_of_type typ) id
  | `NotEnoughArguments (expr, have, want) ->
      Printf.sprintf
        "%s: error: not enough arguments in call to %s\n\
         \thave (%s)\n\
         \twant (%s)"
        pos
        (Ast.string_of_expression expr)
        (String.concat ", " (List.map Type.string_of_type have))
        (String.concat ", " (List.map Type.string_of_type want))
  | `TooManyArguments (expr, have, want) ->
      Printf.sprintf
        "%s: error: too many arguments in call to %s\n\thave (%s)\n\twant (%s)"
        pos
        (Ast.string_of_expression expr)
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
  | `NotAnExpression id ->
      Printf.sprintf "%s: NotAnExpression id=%s" loc id
  | `NoSuchMember (typ, id) ->
      Printf.sprintf "%s: NoSuchMember (typ=%s, id=%s)" loc
        (Type.string_of_type typ) id
  | `NotEnoughArguments (expr, have, want) ->
      Printf.sprintf "%s: NotEnoughArguments (expr=%s, have=%s, want=%s)" loc
        (Ast.string_of_expression expr)
        (String.concat ", " (List.map Type.string_of_type have))
        (String.concat ", " (List.map Type.string_of_type want))
  | `TooManyArguments (expr, have, want) ->
      Printf.sprintf "%s: TooManyArguments (expr=%s, have=%s, want=%s)" loc
        (Ast.string_of_expression expr)
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
