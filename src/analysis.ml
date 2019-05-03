open Monad
module SS = Set.Make (String)

type error =
  [ `Redefinition of string
  | `DuplicateMember of string
  | `DuplicateParameter of string
  | `Unimplemented of string
  | `UnknownTypeName of string
  | `NonIntegerArraySize
  | `UndeclaredIdentifier of string
  | `AssignmentMismatch of int * int ]

let check_unique idfn errfn elems =
  List.fold_left
    (fun acc elem ->
      acc >>= fun seen ->
      let id = idfn elem in
      if SS.mem id seen then Error (errfn elem) else Ok (SS.add id seen) )
    (Ok SS.empty) elems

(* Checks whether an array dimension is valid and returns the 
 * unfolded value in case it is a reference to a constant. *)
let check_array_dim env loc dim =
  match dim with
  | Type.OfInt _ ->
      Ok dim
  | Type.OfName name -> (
    match Env.get_constant name env with
    | Some {value= Env.Int i; _} ->
        Ok (Type.OfInt i)
    | Some {value= Env.Bool _; _} ->
        Error Located.{loc; value= `NonIntegerArraySize}
    | Some {value= Env.Float _; _} ->
        Error Located.{loc; value= `NonIntegerArraySize}
    | None ->
        Error Located.{loc; value= `UndeclaredIdentifier name} )
  | Type.OfFloat _ | Type.OfBool _ ->
      Error Located.{loc; value= `NonIntegerArraySize}

(* Checks the given type against the environment. For function types, it returns
 * an updated environment with a var for each function parameter. *)
let rec check_type env ctx_loc t =
  match t with
  | Type.TypeRef name ->
      if Env.type_exists name env then Ok (env, t)
      else Error Located.{loc= ctx_loc; value= `UnknownTypeName name}
  | Type.Record fields ->
      let idfn field = field.Type.name in
      let errfn field =
        Located.{loc= ctx_loc; value= `DuplicateMember (idfn field)}
      in
      check_unique idfn errfn fields >>= fun _ ->
      List.fold_left
        (fun acc field ->
          acc >>= fun new_fields ->
          check_type env ctx_loc field.Type.t >>= fun (_, nt) ->
          Ok (new_fields @ [{field with t= nt}]) )
        (Ok []) fields
      >>= fun new_fields -> Ok (env, Type.Record new_fields)
  | Type.Array (t, dims) ->
      check_type env ctx_loc t >>= fun (_, nt) ->
      List.fold_left
        (fun acc dim ->
          acc >>= fun new_dims ->
          check_array_dim env ctx_loc dim >>= fun ndim -> Ok (new_dims @ [ndim])
          )
        (Ok []) dims
      >>= fun new_dims -> Ok (env, Type.Array (nt, new_dims))
  | Type.Function (args, rets) ->
      let idfn arg = arg.Type.name in
      let errfn arg =
        Located.{loc= ctx_loc; value= `DuplicateParameter (idfn arg)}
      in
      check_unique idfn errfn args >>= fun _ ->
      List.fold_left
        (fun acc f ->
          acc >>= fun (new_env, new_args) ->
          check_type env ctx_loc f.Type.t >>= fun (_, nt) ->
          let new_arg = {f with t= nt} in
          Ok
            ( Env.add_var f.name {loc= ctx_loc; value= nt} new_env
            , new_args @ [new_arg] ) )
        (Ok (env, []))
        args
      >>= fun (new_env, new_args) ->
      List.fold_left
        (fun acc t ->
          acc >>= fun new_rets ->
          check_type env ctx_loc t >>= fun (_, nt) -> Ok (new_rets @ [nt]) )
        (Ok []) rets
      >>= fun new_rets -> Ok (new_env, Type.Function (new_args, new_rets))
  | Type.Primitive _ ->
      failwith "primitive types should not appear in analysis phase input"

let check_const_declaration env loc cd =
  let Ast.{cd_name; cd_value} = cd in
  if Env.constant_exists cd_name env then
    Error Located.{loc; value= `Redefinition cd_name}
  else
    match cd_value.value with
    | Ast.BoolLiteral b ->
        let value = Located.{loc; value= Env.Bool b} in
        let env = Env.add_constant cd_name value env in
        let t = Type.Primitive Bool in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= (t, cd_value)} in
        Ok (env, cd)
    | Ast.IntLiteral i ->
        let value = Located.{loc; value= Env.Int i} in
        let env = Env.add_constant cd_name value env in
        let t = Type.Primitive Int in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= (t, cd_value)} in
        Ok (env, cd)
    | Ast.FloatLiteral f ->
        let value = Located.{loc; value= Env.Float f} in
        let env = Env.add_constant cd_name value env in
        let t = Type.Primitive Float in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= (t, cd_value)} in
        Ok (env, cd)
    | _ ->
        Error
          Located.
            { loc
            ; value=
                `Unimplemented
                  "constant initializer must be a boolean, integer or float \
                   literal" }

let check_const_declarations env tls =
  List.fold_left
    (fun acc Located.{loc; value} ->
      match value with
      | Ast.ConstDecl cd ->
          acc >>= fun (env, typed_cds) ->
          check_const_declaration env loc cd >>= fun (env, typed_cd) ->
          Ok (env, typed_cds @ [typed_cd])
      | _ ->
          acc )
    (Ok (env, []))
    tls

let check_type_declaration env loc td =
  let Ast.{td_name; td_type} = td in
  if Env.type_exists td_name env then
    Error Located.{loc; value= `Redefinition td_name}
  else
    match td_type with
    | Record _ ->
        check_type env loc td_type >>= fun (_, clean_type) ->
        let env = Env.add_type td_name {loc; value= clean_type} env in
        let typed_td = TypedAst.TypeDecl {td_name; td_type= clean_type} in
        Ok (env, typed_td)
    | TypeRef _ | Array _ | Function _ | Primitive _ ->
        failwith
          (Printf.sprintf "type %s: type declarations should always be Record"
             td_name)

let check_type_declarations env tls =
  List.fold_left
    (fun acc Located.{loc; value} ->
      match value with
      | Ast.TypeDecl td ->
          acc >>= fun (env, typed_tds) ->
          check_type_declaration env loc td >>= fun (env, typed_td) ->
          Ok (env, typed_tds @ [typed_td])
      | _ ->
          acc )
    (Ok (env, []))
    tls

let check_pipeline_signatures env pd_functions =
  let idfn Located.{value; _} = value.Ast.fd_name in
  let errfn func = Located.{loc= func.loc; value= `Redefinition (idfn func)} in
  check_unique idfn errfn pd_functions >>= fun _ ->
  List.fold_left
    (fun acc Located.{loc; value} ->
      let Ast.{fd_name; fd_type; _} = value in
      acc >>= fun env ->
      check_type env loc fd_type >>= fun (_, clean_type) ->
      Ok Env.(env |> add_function fd_name Located.{loc; value= clean_type}) )
    (Ok env) pd_functions

let check_stmt _ loc = function
  | Ast.Var {var_ids; var_values} ->
      let num_vars, num_values = List.(length var_ids, length var_values) in
      if num_vars <> num_values then
        Error Located.{loc; value= `AssignmentMismatch (num_vars, num_values)}
      else (* TODO: implement *)
        Ok []
  | Ast.Assignment _ ->
      (* TODO: implement *)
      Ok []
  | Ast.If _ ->
      (* TODO: implement *)
      Ok []
  | Ast.ForIter _ ->
      (* TODO: implement *)
      Ok []
  | Ast.ForRange _ ->
      (* TODO: implement *)
      Ok []
  | Ast.Return _ ->
      (* TODO: implement *)
      Ok []

let check_function_declaration env loc fd =
  let Ast.{fd_name; fd_type; fd_body} = fd in
  let env = Env.enter_function_scope fd_name env in
  check_type env loc fd_type >>= fun (local_env, clean_type) ->
  List.fold_left
    (fun acc Located.{loc; value= stmt} ->
      acc >>= fun typed_stmts ->
      check_stmt local_env loc stmt >>= fun typed_stmt ->
      Ok (typed_stmts @ typed_stmt) )
    (Ok []) fd_body
  >>= fun typed_stmts ->
  Ok
    TypedAst.
      {fd_env= local_env; fd_name; fd_type= clean_type; fd_body= typed_stmts}

let check_pipeline_functions env pd_functions =
  List.fold_left
    (fun acc Located.{loc; value= fd} ->
      acc >>= fun typed_fds ->
      check_function_declaration env loc fd >>= fun typed_fd ->
      Ok (typed_fds @ [typed_fd]) )
    (Ok []) pd_functions

let check_pipeline_declaration env loc pd =
  let Ast.{pd_name; pd_type; pd_functions} = pd in
  if Env.pipeline_exists pd_name env then
    Error Located.{loc; value= `Redefinition pd_name}
  else
    let local_env = Env.enter_pipeline_scope pd_name env in
    check_type local_env loc pd_type >>= fun (local_env, clean_type) ->
    check_pipeline_signatures local_env pd_functions >>= fun local_env ->
    check_pipeline_functions local_env pd_functions >>= fun typed_functions ->
    let typed_pd =
      TypedAst.PipelineDecl
        { pd_env= local_env
        ; pd_name
        ; pd_type= clean_type
        ; pd_functions= typed_functions }
    in
    let env = Env.add_pipeline pd_name {loc; value= clean_type} env in
    Ok (env, typed_pd)

let check_pipeline_declarations env tls =
  List.fold_left
    (fun acc Located.{loc; value} ->
      match value with
      | Ast.PipelineDecl pd ->
          acc >>= fun (env, typed_pds) ->
          check_pipeline_declaration env loc pd >>= fun (env, typed_pd) ->
          Ok (env, typed_pds @ [typed_pd])
      | _ ->
          acc )
    (Ok (env, []))
    tls

let check_renderer_declarations env _ =
  (*   TODO: implement *)
  Ok (env, [])

let check ast =
  let env = Env.global in
  check_const_declarations env ast >>= fun (env, typed_cds) ->
  check_type_declarations env ast >>= fun (env, typed_tds) ->
  check_pipeline_declarations env ast >>= fun (env, typed_pds) ->
  check_renderer_declarations env ast >>= fun (env, typed_rds) ->
  Ok
    TypedAst.
      {root_env= env; root_elems= typed_cds @ typed_tds @ typed_pds @ typed_rds}
