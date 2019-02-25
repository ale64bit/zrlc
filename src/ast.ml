type expression =
  | Access of expression * string
  | Index of expression * expression list
  | Call of expression * expression list
  | NamedArg of string * expression
  | BundledArg of expression list
  | BinExpr of expression * binop * expression
  | UnExpr of unop * expression
  | BoolLiteral of bool
  | IntLiteral of int
  | FloatLiteral of float
  | Id of string

and unop =
  | UPlus | UMinus | LogicalNot | BitwiseComplement

and binop =
  | LogicalOr | LogicalXor | LogicalAnd
  | BitwiseOr | BitwiseXor | BitwiseAnd
  | Equal | NotEqual
  | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual
  | ShiftLeft | ShiftRight
  | Plus | Minus | Mult | Div | Mod

and assignop =
  | Assign | AssignPlus | AssignMinus | AssignMult | AssignDiv | AssignMod

type var_declaration = {
  var_ids: string list;
  var_values: expression list;
}

and assignment = {
  asg_op: assignop;
  asg_lvalues: expression list;
  asg_rvalues: expression list;
}

and if_stmt = {
  if_cond: expression;
  if_body: stmt list;
}

and for_iter_stmt = {
  foriter_id: string;
  foriter_it: expression;
  foriter_body: stmt list;
}

and for_range_stmt = {
  forrange_id: string;
  forrange_from: expression;
  forrange_to: expression;
  forrange_body: stmt list;
}

and stmt =
  | Var of var_declaration
  | Assignment of assignment
  | If of if_stmt
  | ForIter of for_iter_stmt
  | ForRange of for_range_stmt
  | Return of expression list

type const_declaration = {
  cd_name: string;
  cd_value: expression;
}

type type_declaration = {
  td_name: string;
  td_type: Type.t;
}

type function_declaration = {
  fd_name: string; 
  fd_type: Type.t; 
  fd_body: stmt list
} 

type pipeline_declaration = {
  pd_name: string;
  pd_type: Type.t;
  pd_functions: function_declaration list;
}

type renderer_declaration = {
  rd_name: string;
  rd_type: Type.t;
  rd_body: stmt list;
}

type toplevel_elem =
  | ConstDecl of const_declaration
  | TypeDecl of type_declaration
  | PipelineDecl of pipeline_declaration
  | RendererDecl of renderer_declaration

type root = toplevel_elem list

let string_of_unop = function
  | UPlus -> "+"
  | UMinus -> "-"
  | LogicalNot -> "!"
  | BitwiseComplement-> "~"

let string_of_binop = function
  | LogicalOr -> "||"
  | LogicalXor -> "^^"
  | LogicalAnd-> "&&"
  | BitwiseOr -> "|"
  | BitwiseXor -> "^"
  | BitwiseAnd-> "&"
  | Equal -> "=="
  | NotEqual-> "!="
  | LessThan -> "<"
  | GreaterThan -> ">"
  | LessOrEqual -> "<="
  | GreaterOrEqual-> ">="
  | ShiftLeft -> "<<"
  | ShiftRight-> ">>"
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"

let rec string_of_expression = function
  | Access (l, r) ->
      Printf.sprintf "Access(%s, %s)" (string_of_expression l) r
  | Index  (l, r) ->
      Printf.sprintf "Index(%s, [%s])" 
        (string_of_expression l) 
        (String.concat ", " (List.map string_of_expression r))
  | Call (l, r) ->
      Printf.sprintf "Call(%s, [%s])" 
        (string_of_expression l) 
        (String.concat ", " (List.map string_of_expression r))
  | NamedArg (l, r) ->
      Printf.sprintf "NamedArg(%s, %s)" l (string_of_expression r)
  | BundledArg e ->
      Printf.sprintf "BundledArg(%s)"
        (String.concat ", " (List.map string_of_expression e))
  | BinExpr (l, op, r) ->
      Printf.sprintf "BinExpr(%s, %s, %s)"
        (string_of_expression l)
        (string_of_binop op)
        (string_of_expression r)
  | UnExpr (op, r) ->
      Printf.sprintf "UnExpr(%s, %s)"
        (string_of_unop op)
        (string_of_expression r)
  | BoolLiteral b -> 
      Printf.sprintf "BoolLiteral(%b)" b
  | IntLiteral i ->
      Printf.sprintf "IntLiteral(%d)" i
  | FloatLiteral f ->
      Printf.sprintf "FloatLiteral(%f)" f
  | Id id ->
      Printf.sprintf "Id(%s)" id

let string_of_constdecl {cd_name; cd_value} = 
  Printf.sprintf "ConstDecl{name=%s; value=%s}" cd_name (string_of_expression cd_value)

let string_of_typedecl {td_name; td_type} =
  Printf.sprintf "TypeDecl{name=%s; type=%s}" td_name (Type.string_of_type td_type)

let string_of_toplevel = function
  | ConstDecl cd -> string_of_constdecl cd
  | TypeDecl td -> string_of_typedecl td
  | PipelineDecl _ -> "TODO(PipelineDecl)"
  | RendererDecl _ -> "TODO(RendererDecl)"

let string_of_ast r =
  "{" ^ (String.concat ", " (List.map string_of_toplevel r)) ^ "}"
