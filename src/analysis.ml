open Monad
module SS = Set.Make (String)

type error =
  [ `Redefinition of string
  | `DuplicateMember of string * string
  | `DuplicateParameter of string
  | `Unimplemented of string
  | `UnknownTypeName of string
  | `NonIntegerArraySize
  | `UndeclaredIdentifier of string ]

let check_const_decl env {Ast.cd_name; cd_value} =
  if Env.constant_exists cd_name env then Error (`Redefinition cd_name)
  else
    match cd_value.value with
    | Ast.BoolLiteral b ->
        Ok
          ( Env.add_constant cd_name {loc= cd_value.loc; value= Env.Bool b} env
          , [] )
    | Ast.IntLiteral i ->
        Ok
          ( Env.add_constant cd_name {loc= cd_value.loc; value= Env.Int i} env
          , [] )
    | Ast.FloatLiteral f ->
        Ok
          ( Env.add_constant cd_name {loc= cd_value.loc; value= Env.Float f} env
          , [] )
    | _ ->
        Error
          (`Unimplemented
            "constant initializer must be a boolean, integer or float literal")

let check_unique ids errfn =
  List.fold_left
    (fun seen id ->
      seen
      >>= fun seen ->
      if SS.mem id seen then Error (errfn id) else Ok (SS.add id seen) )
    (Ok SS.empty) ids

let check_array_dim env = function
  | Type.OfInt _ ->
      Ok env
  | Type.OfName name -> (
    match Env.get_constant name env with
    | Some {value= Env.Int _; _} ->
        Ok env
    | Some {value= Env.Bool _; _} ->
        Error `NonIntegerArraySize
    | Some {value= Env.Float _; _} ->
        Error `NonIntegerArraySize
    | None ->
        Error (`UndeclaredIdentifier name) )
  | Type.OfFloat _ | Type.OfBool _ ->
      Error `NonIntegerArraySize

(* Checks the given type against the environment. For function types, it returns
 * an updated environment with a var for each function parameter. *)
let rec check_type ctx_loc env = function
  | Type.TypeRef name ->
      if Env.type_exists name env then Ok env
      else Error (`UnknownTypeName name)
  | Type.Record fields ->
      List.fold_left
        (fun env f -> env >>= fun env -> check_type ctx_loc env f.Type.t)
        (Ok env) fields
  | Type.Array (t, dims) ->
      check_type ctx_loc env t
      >>= fun env ->
      List.fold_left
        (fun env dim -> env >>= fun env -> check_array_dim env dim)
        (Ok env) dims
  | Type.Function (args, rets) ->
      let errfn id = `DuplicateParameter id in
      let ids = List.map (fun f -> f.Type.name) args in
      check_unique ids errfn
      >>= fun _ ->
      List.fold_left
        (fun env f ->
          env
          >>= fun env ->
          check_type ctx_loc env f.Type.t
          >>= fun env -> Ok (Env.add_var f.name {loc= ctx_loc; value= f.t} env)
          )
        (Ok env) args
      >>= fun env ->
      List.fold_left
        (fun env t -> env >>= fun env -> check_type ctx_loc env t)
        (Ok env) rets
  | Type.Primitive _ ->
      failwith "primitive types should not appear in analysis phase input"

let check_type_decl ctx_loc env {Ast.td_name; td_type} =
  if Env.type_exists td_name env then Error (`Redefinition td_name)
  else
    match td_type with
    | Record fields ->
        let errfn id = `DuplicateMember (td_name, id) in
        let ids = List.map (fun f -> f.Type.name) fields in
        check_unique ids errfn
        >>= fun _ ->
        check_type ctx_loc env td_type
        >>= fun _ ->
        Ok (Env.add_type td_name {loc= ctx_loc; value= td_type} env, [])
    | TypeRef _ | Array _ | Function _ | Primitive _ ->
        let msg =
          Printf.sprintf "type %s: type declarations should always be Record"
            td_name
        in
        failwith msg

(* TODO: implement *)
let check_function_decl env {Ast.fd_name; fd_type; _} =
  let tmp = {TypedAst.fd_name; fd_type; fd_body= []} in
  Ok (env, tmp)

let check_pipeline_functions env functions =
  let errfn id = `Redefinition id in
  let ids = List.map (fun {Located.value; _} -> value.Ast.fd_name) functions in
  check_unique ids errfn
  >>= fun _ ->
  List.fold_left
    (fun acc {Located.loc; value= fdecl} ->
      acc
      >>= fun (env, funcs) ->
      check_function_decl env fdecl
      >>= fun f ->
      Ok
        ( Env.add_function fdecl.fd_name {Located.loc; value= fdecl.fd_type} env
        , f :: funcs ) )
    (Ok (env, []))
    functions

let check_pipeline_decl ctx_loc env {Ast.pd_name; pd_type; pd_functions} =
  if Env.pipeline_exists pd_name env then Error (`Redefinition pd_name)
  else
    Ok (Env.add_pipeline pd_name {loc= ctx_loc; value= pd_type} env)
    >>= fun genv ->
    check_type ctx_loc env pd_type
    >>= fun env ->
    check_pipeline_functions env pd_functions
    >>= fun (penv, funcs) ->
    let pd = {TypedAst.pd_name; pd_type; pd_functions= List.rev funcs} in
    Ok
      ( genv
      , [TypedAst.PipelineDecl (Env.enter_pipeline_scope pd_name penv, pd)] )

(* TODO: implement *)
let check_renderer_decl env _ = Ok (env, [])

let check_toplevel env {Located.loc; value} =
  match value with
  | Ast.ConstDecl cd ->
      check_const_decl env cd
  | Ast.TypeDecl td ->
      check_type_decl loc env td
  | Ast.PipelineDecl pd ->
      check_pipeline_decl loc env pd
  | Ast.RendererDecl rd ->
      check_renderer_decl env rd

let check ast =
  List.fold_left
    (fun acc tl ->
      acc
      >>= fun (env, tls) ->
      check_toplevel env tl >>= fun (env, res) -> Ok (env, tls @ res) )
    (Ok (Env.global, []))
    ast
