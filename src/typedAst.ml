type expression = Type.t * Ast.expression [@@deriving to_yojson]

and stmt = Env.t * Ast.stmt [@@deriving to_yojson]

type function_declaration =
  {fd_name: string; fd_type: Type.t; fd_body: stmt list}
[@@deriving to_yojson]

type pipeline_declaration =
  { pd_name: string
  ; pd_type: Type.t
  ; pd_functions: (Env.t * function_declaration) list }
[@@deriving to_yojson]

type renderer_declaration =
  {rd_name: string; rd_type: Type.t; rd_body: stmt list}
[@@deriving to_yojson]

type toplevel_elem =
  | PipelineDecl of Env.t * pipeline_declaration
  | RendererDecl of Env.t * renderer_declaration
[@@deriving to_yojson]

type root = Env.t * toplevel_elem list [@@deriving to_yojson]

let string_of_ast root = Yojson.Safe.pretty_to_string (root_to_yojson root)
