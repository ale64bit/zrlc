type expression =
  Type.t * Ast.expression

and stmt =
  Env.t * Ast.stmt

type function_declaration = {
  fd_name: string; 
  fd_type: Type.t; 
  fd_body: stmt list
} 

type pipeline_declaration = {
  pd_name: string;
  pd_type: Type.t;
  pd_functions: (Env.t * function_declaration) list;
}

type renderer_declaration = {
  rd_name: string;
  rd_type: Type.t;
  rd_body: stmt list;
}

type toplevel_elem =
  | PipelineDecl of Env.t * pipeline_declaration
  | RendererDecl of Env.t * renderer_declaration

type root = Env.t * toplevel_elem list

val string_of_ast : root -> string
