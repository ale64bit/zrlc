open Monad

(*
(*
- identifier must be available
- rhs must be a literal expression
*)
let check_const_def {name; value} env = 
  match value with 
  | Ast.BoolLiteral _ -> Ok env
  | Ast.IntLiteral _ -> Ok env
  | Ast.FloatLiteral _ -> Ok env
  | _ -> Error (Printf.sprintf "constant '%s': expected boolean, integer or float literal initializer" name)

(*
- identifier must be available
- type must be valid
  - TypeRef must point to existing types
  - Record field types must be valid
  - Array type must be valid
  - Array dimensions must be integers
  - Function arg and ret ypes must be valid
*)
let check_type_def {name; typ} env = Ok (Env.put name typ env)

(* - identifier must be available *)
let check_func_def {name; typ=_; stmts=_} env =
  let env = Env.enter_scope ("func$" ^ name) env in
  let env = Env.exit_scope env in
  env

(* - identifier must be available *)
let check_pipeline_def {name=_; typ=_; funcs} env = 
  let m = fun env f -> may (check_func_def f) env in
  List.fold_left m (Ok env) funcs

(* - identifier must be available *)
let check_renderer_def {name=_; typ=_; stmts=_} env = Ok env

let check_root root env = match root with
  | Ast.ConstDef {name; value} as const_def ->
      check_const_def const_def env
  | Ast.TypeDef {name; typ} as type_def -> 
      check_type_def type_def env
  | Ast.PipelineDef {name; typ; funcs} as pipeline_def -> 
      let env = Env.enter_scope ("pipeline$" ^ id) env in
      let env = check_pipeline_def pipeline_def env in
      may Env.exit_scope env 
  | Ast.RendererDef {name; typ; stmts} as renderer_def -> 
      let env = Env.enter_scope ("renderer$" ^ name) env in
      let env = check_renderer_def renderer_def env in
      may Env.exit_scope env

let rec check_roots roots env = match roots with
  | [] -> Ok env
  | hd :: tl -> 
      match check_root hd env with
        | Ok env -> check_roots tl env
        | Error e ->  Error e

let pass ast = 
  let env = Env.enter_scope "global" Env.empty in
  match check_roots ast env with
    | Ok _ -> Ok ast
    | Error e -> Error e
    
*)

type error = [
  | `TypeError of string
  | `SemanticError of string
]

let check_const_decl env _ = Ok (env, [])
let check_type_decl env _ = Ok (env, [])
let check_pipeline_decl env _ = Ok (env, [])
let check_renderer_decl env _ = Ok (env, [])

let check_toplevel env = function
(*   | Ast.ConstDecl {cd_name; cd_value} as cd ->  *)
  | Ast.ConstDecl cd -> 
      check_const_decl env cd
(*   | Ast.TypeDecl {td_name; td_type=_} as td ->  *)
  | Ast.TypeDecl td -> 
      check_type_decl env td
(*   | Ast.PipelineDecl {pd_name; pd_type=_; pd_functions} as pd ->  *)
  | Ast.PipelineDecl pd -> 
      check_pipeline_decl env pd
(*   | Ast.RendererDecl {rd_name; rd_type=_; rd_body} as rd -> *)
  | Ast.RendererDecl rd ->
      check_renderer_decl env rd

let check ast = 
  List.fold_left
    (fun acc tl ->
      acc >>= fun (env, tls) ->
      check_toplevel env tl >>= fun (env, res) ->
      Ok (env, tls @ res))
    (Ok (Env.empty "global", []))
    ast

