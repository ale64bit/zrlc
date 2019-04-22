let string_of_error = function
  | `LexerError (pos, msg) ->
      Printf.sprintf "%s: error: %s" (Located.string_of_position pos) msg
  | `ParserError (pos, msg) ->
      Printf.sprintf "%s: error: %s" (Located.string_of_position pos) msg
  | `Redefinition id ->
      Printf.sprintf "???: error: redefinition of '%s'" id
  | `DuplicateMember id ->
      Printf.sprintf "???: error: duplicate member '%s'" id
  | `DuplicateParameter id ->
      Printf.sprintf "???: error: redefinition of parameter '%s'" id
  | `Unimplemented msg ->
      Printf.sprintf "???: error: %s" msg
  | `UnknownTypeName name ->
      Printf.sprintf "???: error: unknown type name '%s'" name
  | `NonIntegerArraySize ->
      "???: error: non-integer array size"
  | `UndeclaredIdentifier id ->
      Printf.sprintf "???: error: undeclared identifier '%s'" id

let debug_string_of_error = function
  | `LexerError (pos, msg) ->
      Printf.sprintf "LexerError (pos=%s, msg=%s)"
        (Located.string_of_position pos)
        msg
  | `ParserError (pos, msg) ->
      Printf.sprintf "ParserError (pos=%s, msg=%s)"
        (Located.string_of_position pos)
        msg
  | `Redefinition id ->
      Printf.sprintf "Redefinition id=%s" id
  | `DuplicateMember id ->
      Printf.sprintf "DuplicateMember id=%s" id
  | `DuplicateParameter id ->
      Printf.sprintf "DuplicateParameter id=%s" id
  | `Unimplemented msg ->
      Printf.sprintf "Unimplemented msg=%s" msg
  | `UnknownTypeName name ->
      Printf.sprintf "UnknownTypeName name=%s" name
  | `NonIntegerArraySize ->
      "NonIntegerArraySize"
  | `UndeclaredIdentifier id ->
      Printf.sprintf "UndeclaredIdentifier id=%s" id
