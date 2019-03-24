(* Expressions *)

type unop =
  | UPlus | UMinus | LogicalNot | BitwiseComplement

type binop =
  | LogicalOr | LogicalXor | LogicalAnd
  | BitwiseOr | BitwiseXor | BitwiseAnd
  | Equal | NotEqual
  | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual
  | ShiftLeft | ShiftRight
  | Plus | Minus | Mult | Div | Mod

type assignop =
  | Assign | AssignPlus | AssignMinus | AssignMult | AssignDiv | AssignMod

type 'a located =
  { loc: Lexing.position * Lexing.position; value: 'a }

type expression =
  raw_expression located

and raw_expression =
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

(* Statements *)

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
  raw_stmt located

and raw_stmt =
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

val string_of_ast : root -> string
