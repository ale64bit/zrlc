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

(* Expressions *)

type unop = UPlus | UMinus | LogicalNot | BitwiseComplement
[@@deriving to_yojson]

type binop =
  | LogicalOr
  | LogicalXor
  | LogicalAnd
  | BitwiseOr
  | BitwiseXor
  | BitwiseAnd
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual
  | ShiftLeft
  | ShiftRight
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
[@@deriving to_yojson]

type assignop =
  | Assign
  | AssignPlus
  | AssignMinus
  | AssignMult
  | AssignDiv
  | AssignMod
[@@deriving to_yojson]

type expression = raw_expression Located.t [@@deriving to_yojson]

and raw_expression =
  | Access of expression * string
  | Index of expression * expression list
  | Call of expression * expression list
  | Cast of Type.t * expression
  | NamedArg of string * expression
  | BinExpr of expression * binop * expression
  | UnExpr of unop * expression
  | BoolLiteral of bool
  | IntLiteral of int
  | FloatLiteral of float
  | Id of string
[@@deriving to_yojson]

(* Statements *)

type binding = { bind_ids : string list; bind_values : expression list }
[@@deriving to_yojson]

and assignment = {
  asg_op : assignop;
  asg_lvalues : expression list;
  asg_rvalues : expression list;
}
[@@deriving to_yojson]

and if_stmt = {
  if_cond : expression;
  if_true : stmt list;
  if_false : stmt list;
}
[@@deriving to_yojson]

and for_iter_stmt = {
  foriter_id : string;
  foriter_it : expression;
  foriter_body : stmt list;
}
[@@deriving to_yojson]

and for_range_stmt = {
  forrange_id : string;
  forrange_from : expression;
  forrange_to : expression;
  forrange_body : stmt list;
}
[@@deriving to_yojson]

and stmt = raw_stmt Located.t [@@deriving to_yojson]

and raw_stmt =
  | CallExpr of string * expression list
  | Var of binding
  | Val of binding
  | Assignment of assignment
  | If of if_stmt
  | ForIter of for_iter_stmt
  | ForRange of for_range_stmt
  | Return of expression list
  | Discard
[@@deriving to_yojson]

type const_declaration = { cd_name : string; cd_value : expression }
[@@deriving to_yojson]

type type_declaration = { td_name : string; td_type : Type.t }
[@@deriving to_yojson]

type raw_function_declaration = {
  fd_name : string;
  fd_type : Type.t;
  fd_body : stmt list;
}
[@@deriving to_yojson]

type function_declaration = raw_function_declaration Located.t
[@@deriving to_yojson]

type pipeline_declaration = {
  pd_name : string;
  pd_type : Type.t;
  pd_functions : function_declaration list;
}
[@@deriving to_yojson]

type renderer_declaration = {
  rd_name : string;
  rd_type : Type.t;
  rd_functions : function_declaration list;
}
[@@deriving to_yojson]

type raw_toplevel_elem =
  | ConstDecl of const_declaration
  | TypeDecl of type_declaration
  | PipelineDecl of pipeline_declaration
  | RendererDecl of renderer_declaration
[@@deriving to_yojson]

type toplevel_elem = raw_toplevel_elem Located.t [@@deriving to_yojson]

type root = { module_name : string; elements : toplevel_elem list }
[@@deriving to_yojson]

let string_of_ast root = Yojson.Safe.pretty_to_string (root_to_yojson root)

let string_of_unop = function
  | UPlus -> "+"
  | UMinus -> "-"
  | LogicalNot -> "!"
  | BitwiseComplement -> "~"

let string_of_binop = function
  | LogicalOr -> "||"
  | LogicalXor -> "^^"
  | LogicalAnd -> "&&"
  | BitwiseOr -> "|"
  | BitwiseXor -> "^"
  | BitwiseAnd -> "&"
  | Equal -> "=="
  | NotEqual -> "!="
  | LessThan -> "<"
  | GreaterThan -> ">"
  | LessOrEqual -> "<="
  | GreaterOrEqual -> ">="
  | ShiftLeft -> "<<"
  | ShiftRight -> ">>"
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"

let string_of_assignop = function
  | Assign -> "="
  | AssignPlus -> "+="
  | AssignMinus -> "-="
  | AssignMult -> "*="
  | AssignDiv -> "/="
  | AssignMod -> "%="

let rec string_of_expression Located.{ value = e; _ } =
  match e with
  | Access (lhs, member) ->
      Printf.sprintf "%s.%s" (string_of_expression lhs) member
  | Index (lhs, indices) ->
      Printf.sprintf "%s[%s]" (string_of_expression lhs)
        (String.concat ", " (List.map string_of_expression indices))
  | Call (lhs, args) ->
      Printf.sprintf "%s(%s)" (string_of_expression lhs)
        (String.concat ", " (List.map string_of_expression args))
  | Cast (t, expr) ->
      Printf.sprintf "cast<%s>(%s)" (Type.string_of_type t)
        (string_of_expression expr)
  | NamedArg (id, expr) ->
      Printf.sprintf "%s = %s" id (string_of_expression expr)
  | BinExpr (lhs, op, rhs) ->
      Printf.sprintf "%s %s %s" (string_of_expression lhs) (string_of_binop op)
        (string_of_expression rhs)
  | UnExpr (op, rhs) ->
      Printf.sprintf "%s %s" (string_of_unop op) (string_of_expression rhs)
  | BoolLiteral b -> string_of_bool b
  | IntLiteral i -> string_of_int i
  | FloatLiteral f -> string_of_float f
  | Id id -> id
