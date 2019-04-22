open Monad
module SS = Set.Make (String)

type error =
  [ `Redefinition of string
  | `DuplicateMember of string
  | `DuplicateParameter of string
  | `Unimplemented of string
  | `UnknownTypeName of string
  | `NonIntegerArraySize
  | `UndeclaredIdentifier of string ]

let check_const_decl env loc Ast.{cd_name; cd_value} =
  if Env.constant_exists cd_name env then Error (`Redefinition cd_name)
  else
    match cd_value.value with
    | Ast.BoolLiteral b ->
        let value = Located.{loc; value= Env.Bool b} in
        let env = Env.add_constant cd_name value env in
        let t = Type.Primitive Bool in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= (t, cd_value)} in
        Ok (env, [cd])
    | Ast.IntLiteral i ->
        let value = Located.{loc; value= Env.Int i} in
        let env = Env.add_constant cd_name value env in
        let t = Type.Primitive Int in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= (t, cd_value)} in
        Ok (env, [cd])
    | Ast.FloatLiteral f ->
        let value = Located.{loc; value= Env.Float f} in
        let env = Env.add_constant cd_name value env in
        let t = Type.Primitive Float in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= (t, cd_value)} in
        Ok (env, [cd])
    | _ ->
        Error
          (`Unimplemented
            "constant initializer must be a boolean, integer or float literal")

let check_unique ids errfn =
  let check_dup seen id =
    seen
    >>= fun seen ->
    if SS.mem id seen then Error (errfn id) else Ok (SS.add id seen)
  in
  List.fold_left check_dup (Ok SS.empty) ids

(* Checks whether an array dimension is valid and returns the 
 * unfolded value in case it is a reference to a constant. *)
let check_array_dim env dim =
  match dim with
  | Type.OfInt _ ->
      Ok dim
  | Type.OfName name -> (
    match Env.get_constant name env with
    | Some {value= Env.Int i; _} ->
        Ok (Type.OfInt i)
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
let rec check_type env ctx_loc t =
  match t with
  | Type.TypeRef name ->
      if Env.type_exists name env then Ok (env, t)
      else Error (`UnknownTypeName name)
  | Type.Record fields ->
      let errfn id = `DuplicateMember id in
      let ids = List.map (fun f -> f.Type.name) fields in
      check_unique ids errfn
      >>= fun _ ->
      List.fold_left
        (fun acc field ->
          acc
          >>= fun new_fields ->
          check_type env ctx_loc field.Type.t
          >>= fun (_, nt) -> Ok ({field with t= nt} :: new_fields) )
        (Ok []) fields
      >>= fun new_fields -> Ok (env, Type.Record new_fields)
  | Type.Array (t, dims) ->
      check_type env ctx_loc t
      >>= fun (_, nt) ->
      List.fold_left
        (fun acc dim ->
          acc
          >>= fun new_dims ->
          check_array_dim env dim >>= fun ndim -> Ok (ndim :: new_dims) )
        (Ok []) dims
      >>= fun new_dims -> Ok (env, Type.Array (nt, new_dims))
  | Type.Function (args, rets) ->
      let errfn id = `DuplicateParameter id in
      let ids = List.map (fun f -> f.Type.name) args in
      check_unique ids errfn
      >>= fun _ ->
      List.fold_left
        (fun acc f ->
          acc
          >>= fun (new_env, new_args) ->
          check_type env ctx_loc f.Type.t
          >>= fun (_, nt) ->
          let new_arg = {f with t= nt} in
          Ok
            ( Env.add_var f.name {loc= ctx_loc; value= nt} new_env
            , new_arg :: new_args ) )
        (Ok (env, []))
        args
      >>= fun (new_env, new_args) ->
      List.fold_left
        (fun acc t ->
          acc
          >>= fun new_rets ->
          check_type env ctx_loc t >>= fun (_, nt) -> Ok (nt :: new_rets) )
        (Ok []) rets
      >>= fun new_rets -> Ok (new_env, Type.Function (new_args, new_rets))
  | Type.Primitive _ ->
      failwith "primitive types should not appear in analysis phase input"

let check_type_decl env loc td =
  let Ast.{td_name; td_type} = td in
  if Env.type_exists td_name env then Error (`Redefinition td_name)
  else
    match td_type with
    | Record _ ->
        check_type env loc td_type
        >>= fun (_, nt) ->
        let new_env = Env.add_type td_name {loc; value= nt} env in
        let new_td = TypedAst.TypeDecl {td_name; td_type= nt} in
        Ok (new_env, [new_td])
    | TypeRef _ | Array _ | Function _ | Primitive _ ->
        failwith
          (Printf.sprintf "type %s: type declarations should always be Record"
             td_name)

(* TODO: implement *)
let check_function_decl env Ast.{fd_name; fd_type; _} =
  Ok TypedAst.{fd_env= env; fd_name; fd_type; fd_body= []}

let check_pipeline_functions env functions =
  let errfn id = `Redefinition id in
  let ids = List.map (fun Located.{value; _} -> value.Ast.fd_name) functions in
  check_unique ids errfn
  >>= fun _ ->
  List.fold_left
    (fun acc Located.{loc; value= fdecl} ->
      acc
      >>= fun (env, funcs) ->
      check_function_decl env fdecl
      >>= fun f ->
      Ok
        ( Env.add_function fdecl.fd_name Located.{loc; value= fdecl.fd_type} env
        , f :: funcs ) )
    (Ok (env, []))
    functions

let check_pipeline_decl env loc pd =
  let Ast.{pd_name; pd_type; pd_functions} = pd in
  if Env.pipeline_exists pd_name env then Error (`Redefinition pd_name)
  else
    check_type env loc pd_type
    >>= fun (local_env, nt) ->
    let local_env = Env.enter_pipeline_scope pd_name local_env in
    check_pipeline_functions local_env pd_functions
    >>= fun (local_env, new_funcs) ->
    let pd =
      TypedAst.PipelineDecl
        {pd_env= local_env; pd_name; pd_type= nt; pd_functions= new_funcs}
    in
    let new_env = Env.add_pipeline pd_name {loc; value= pd_type} env in
    Ok (new_env, [pd])

(* TODO: implement *)
let check_renderer_decl env _ = Ok (env, [])

let check_toplevel env tl =
  let Located.{loc; value} = tl in
  match value with
  | Ast.ConstDecl cd ->
      check_const_decl env loc cd
  | Ast.TypeDecl td ->
      check_type_decl env loc td
  | Ast.PipelineDecl pd ->
      check_pipeline_decl env loc pd
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
  >>= fun (root_env, root_elems) -> Ok TypedAst.{root_env; root_elems}
