(* Expressions *)

type expression = Type.t list * Ast.expression [@@deriving to_yojson]

(* Statements *)

type var_declaration = {var_ids: string list; var_values: expression list}
[@@deriving to_yojson]

and assignment =
  { asg_op: Ast.assignop
  ; asg_lvalues: expression list
  ; asg_rvalues: expression list }
[@@deriving to_yojson]

and if_stmt = {if_cond: expression; if_true: stmt list; if_false: stmt list}
[@@deriving to_yojson]

and for_iter_stmt =
  {foriter_id: string; foriter_it: expression; foriter_body: stmt list}
[@@deriving to_yojson]

and for_range_stmt =
  { forrange_id: string
  ; forrange_from: expression
  ; forrange_to: expression
  ; forrange_body: stmt list }
[@@deriving to_yojson]

and stmt = Env.t * raw_stmt Located.t [@@deriving to_yojson]

and raw_stmt =
  | Var of var_declaration
  | Assignment of assignment
  | If of if_stmt
  | ForIter of for_iter_stmt
  | ForRange of for_range_stmt
  | Return of expression list
[@@deriving to_yojson]

type const_declaration = {cd_name: string; cd_value: expression}
[@@deriving to_yojson]

type type_declaration = {td_name: string; td_type: Type.t}
[@@deriving to_yojson]

type function_declaration =
  {fd_env: Env.t; fd_name: string; fd_type: Type.t; fd_body: stmt list}
[@@deriving to_yojson]

type pipeline_declaration =
  { pd_env: Env.t
  ; pd_name: string
  ; pd_type: Type.t
  ; pd_functions: function_declaration list }
[@@deriving to_yojson]

type renderer_declaration =
  { rd_env: Env.t
  ; rd_name: string
  ; rd_type: Type.t
  ; rd_functions: function_declaration list }
[@@deriving to_yojson]

type toplevel_elem =
  | ConstDecl of const_declaration
  | TypeDecl of type_declaration
  | PipelineDecl of pipeline_declaration
  | RendererDecl of renderer_declaration
[@@deriving to_yojson]

type root =
  {root_env: Env.t; root_module: string; root_elems: toplevel_elem list}
[@@deriving to_yojson]

let string_of_ast root = Yojson.Safe.pretty_to_string (root_to_yojson root)
