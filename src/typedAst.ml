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
  pd_functions: function_declaration list;
}

type renderer_declaration = {
  rd_name: string;
  rd_type: Type.t;
  rd_body: stmt list;
}

type toplevel_elem =
  | PipelineDecl of pipeline_declaration
  | RendererDecl of renderer_declaration

type root = Env.t * toplevel_elem list
