open Monad

module SS = Set.Make(String)

type error = [
  | `Redefinition of string
  | `DuplicateMember of string * string
  | `Unimplemented of string
]

let check_const_decl env {Ast.cd_name; cd_value} = 
  if Env.constant_exists cd_name env then
    Error (`Redefinition cd_name)
  else match cd_value with
    | BoolLiteral b -> 
        Ok (Env.add_constant cd_name (Env.Bool b) env, [])
    | IntLiteral i -> 
        Ok (Env.add_constant cd_name (Env.Int i) env, [])
    | FloatLiteral f -> 
        Ok (Env.add_constant cd_name (Env.Float f) env, [])
    | _ -> 
        Error (`Unimplemented "constant initializer must be a boolean, integer or float literal")

let check_unique ids errfn = 
  List.fold_left 
    (fun seen id ->
      seen >>= fun seen ->
        if SS.mem id seen then Error (errfn id)
        else Ok (SS.add id seen))
  (Ok SS.empty)
  ids

let check_type_decl env {Ast.td_name; td_type} = 
  if Env.type_exists td_name env then
    Error (`Redefinition td_name)
  else match td_type with
    | Record fields ->
        let errfn = fun id -> `DuplicateMember (td_name, id) in
        let ids = List.map (fun f -> f.Type.name) fields in
        check_unique ids errfn >>= fun _ ->
        (* TODO: check field types are valid *)
        Ok (Env.add_type td_name td_type env, [])
    | TypeRef _ | Array _ | Function _ ->
        let msg = Printf.sprintf "type %s: type declarations should always be Record" td_name in
        failwith msg

let check_pipeline_decl env _ = Ok (env, [])
let check_renderer_decl env _ = Ok (env, [])

let check_toplevel env = function
  | Ast.ConstDecl cd -> check_const_decl env cd
  | Ast.TypeDecl td -> check_type_decl env td
  | Ast.PipelineDecl pd -> check_pipeline_decl env pd
  | Ast.RendererDecl rd -> check_renderer_decl env rd

let check ast = 
  List.fold_left
    (fun acc tl ->
      acc >>= fun (env, tls) ->
      check_toplevel env tl >>= fun (env, res) ->
      Ok (env, tls @ res))
    (Ok (Env.empty "global", []))
    ast

