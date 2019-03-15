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

let string_of_stmt (env, _) =
  Printf.sprintf "(%s, %s)"
    (Env.string_of_env env)
    ("TODO(stmt)")

let string_of_function_decl (env, {fd_name; fd_type; fd_body}) =
  Printf.sprintf "(%s, {fd_name=%s; fd_type=%s; fd_body=[%s]})"
    fd_name
    (Env.string_of_env env)
    (Type.string_of_type fd_type)
    (String.concat "; " (List.map string_of_stmt fd_body))

let string_of_pipeline_decl {pd_name; pd_type; pd_functions} =
  Printf.sprintf "{pd_name=%s; pd_type=%s; pd_functions=[%s]}"
    pd_name
    (Type.string_of_type pd_type)
    (String.concat "; " (List.map string_of_function_decl pd_functions))

let string_of_toplevel = function
  | PipelineDecl (env, pd) -> 
      Printf.sprintf "TypedAst.PipelineDecl (%s, %s)"
        (Env.string_of_env env)
        (string_of_pipeline_decl pd)
  | RendererDecl _ -> "TODO(RendererDecl)"

let string_of_ast (env, tls) =
  Printf.sprintf "TypedAst.root (%s, [%s])"
    (Env.string_of_env env)
    (String.concat "; " (List.map string_of_toplevel tls))

