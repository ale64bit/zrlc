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

type expression = Type.t list * Ast.expression [@@deriving to_yojson]

(* Statements *)

type binding = { bind_ids : string list; bind_values : expression list }
[@@deriving to_yojson]

and assignment = {
  asg_op : Ast.assignop;
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

and stmt = Env.t * raw_stmt Located.t [@@deriving to_yojson]

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

type function_declaration = {
  fd_env : Env.t;
  fd_name : string;
  fd_type : Type.t;
  fd_body : stmt list;
}
[@@deriving to_yojson]

type pipeline_declaration = {
  pd_env : Env.t;
  pd_name : string;
  pd_type : Type.t;
  pd_functions : function_declaration list;
}
[@@deriving to_yojson]

type renderer_declaration = {
  rd_env : Env.t;
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

type root = {
  root_env : Env.t;
  root_module : string;
  root_elems : toplevel_elem list;
}
[@@deriving to_yojson]

let string_of_ast root = Yojson.Safe.pretty_to_string (root_to_yojson root)
