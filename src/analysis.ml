open Monad
module SS = Set.Make (String)

type error =
  [ `Redefinition of string * (Lexing.position * Lexing.position)
  | `DuplicateMember of string
  | `DuplicateParameter of string
  | `Unimplemented of string
  | `UnknownTypeName of string
  | `NonIntegerArraySize
  | `UndeclaredIdentifier of string
  | `AssignmentMismatch of int * int
  | `InvalidUnaryOperation of Ast.unop * Type.t
  | `InvalidBinaryOperation of Type.t * Ast.binop * Type.t ]

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
    | Some _ ->
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
      Ok (env, t)

let check_const_declaration env loc cd =
  let Ast.{cd_name; cd_value} = cd in
  match Env.find_name cd_name env with
  | Some Located.{loc= prev_loc; _} ->
      Error Located.{loc; value= `Redefinition (cd_name, prev_loc)}
  | None -> (
    match cd_value.value with
    | Ast.BoolLiteral b ->
        let value = Located.{loc; value= Env.Bool b} in
        let env = Env.add_constant cd_name value env in
        let t = Type.TypeRef "bool" in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= (t, cd_value)} in
        Ok (env, cd)
    | Ast.IntLiteral i ->
        let value = Located.{loc; value= Env.Int i} in
        let env = Env.add_constant cd_name value env in
        let t = Type.TypeRef "int" in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= (t, cd_value)} in
        Ok (env, cd)
    | Ast.FloatLiteral f ->
        let value = Located.{loc; value= Env.Float f} in
        let env = Env.add_constant cd_name value env in
        let t = Type.TypeRef "float" in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= (t, cd_value)} in
        Ok (env, cd)
    | _ ->
        Error
          Located.
            { loc
            ; value=
                `Unimplemented
                  "constant initializer must be a boolean, integer or float \
                   literal" } )

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
  match Env.get_type td_name env with
  | Some Located.{loc= prev_loc; _} ->
      Error Located.{loc; value= `Redefinition (td_name, prev_loc)}
  | None -> (
    match td_type with
    | Record _ ->
        check_type env loc td_type >>= fun (_, clean_type) ->
        let env = Env.add_type td_name {loc; value= clean_type} env in
        let typed_td = TypedAst.TypeDecl {td_name; td_type= clean_type} in
        Ok (env, typed_td)
    | TypeRef _ | Array _ | Function _ | Primitive _ ->
        failwith
          (Printf.sprintf "type %s: type declarations should always be Record"
             td_name) )

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
  let find_first_loc name =
    List.find (fun Located.{value; _} -> value.Ast.fd_name = name) pd_functions
  in
  let idfn Located.{value; _} = value.Ast.fd_name in
  let errfn func =
    let prev_loc = (find_first_loc (idfn func)).loc in
    Located.{loc= func.loc; value= `Redefinition (idfn func, prev_loc)}
  in
  check_unique idfn errfn pd_functions >>= fun _ ->
  List.fold_left
    (fun acc Located.{loc; value} ->
      let Ast.{fd_name; fd_type; _} = value in
      acc >>= fun env ->
      check_type env loc fd_type >>= fun (_, clean_type) ->
      Ok Env.(env |> add_function fd_name Located.{loc; value= clean_type}) )
    (Ok env) pd_functions

let check_unop loc op typ =
  let err = Error Located.{loc; value= `InvalidUnaryOperation (op, typ)} in
  match (op, typ) with
  (* Unary Plus *)
  | Ast.UPlus, Type.Primitive Bool ->
      err
  | Ast.UPlus, Type.Primitive pt ->
      Ok (Type.Primitive pt)
  (* Unary Minus *)
  | Ast.UMinus, Type.Primitive Bool ->
      err
  | Ast.UMinus, Type.Primitive pt ->
      Ok (Type.Primitive pt)
  (* Logical Negation *)
  | Ast.LogicalNot, Type.Primitive Bool ->
      Ok (Type.Primitive Bool)
  (* Bitwise Complement *)
  | Ast.BitwiseComplement, Type.Primitive Int ->
      Ok (Type.Primitive Int)
  | Ast.BitwiseComplement, Type.Primitive UInt ->
      Ok (Type.Primitive UInt)
  (* Errors *)
  (* TODO: add cases for non-primitive builtin types *)
  | _, _ ->
      err

let check_binop loc ltyp op rtyp =
  match (ltyp, op, rtyp) with
  | _, _, _ ->
      Error Located.{loc; value= `InvalidBinaryOperation (ltyp, op, rtyp)}

let rec check_expr env expr =
  let Located.{loc; value} = expr in
  let err =
    Error
      Located.{loc; value= `Unimplemented "can't check this expression yet"}
  in
  match value with
  | Ast.Access _ ->
      (* TODO: implement *)
      err
  | Ast.Index _ ->
      (* TODO: implement *)
      err
  | Ast.Call _ ->
      (* TODO: implement *)
      err
  | Ast.NamedArg _ ->
      (* TODO: implement *)
      err
  | Ast.BundledArg _ ->
      (* TODO: implement *)
      err
  | Ast.BinExpr (lhs, op, rhs) ->
      check_expr env lhs >>= fun (ltyp, _) ->
      check_expr env rhs >>= fun (rtyp, _) ->
      check_binop loc ltyp op rtyp >>= fun typ -> Ok (typ, expr)
  | Ast.UnExpr (op, rhs) ->
      check_expr env rhs >>= fun (typ, _) ->
      check_unop loc op typ >>= fun typ -> Ok (typ, expr)
  | Ast.BoolLiteral _ ->
      Ok (Type.TypeRef "bool", expr)
  | Ast.IntLiteral _ ->
      Ok (Type.TypeRef "int", expr)
  | Ast.FloatLiteral _ ->
      Ok (Type.TypeRef "float", expr)
  | Ast.Id id -> (
    match Env.(get_constant id env, get_var id env) with
    | Some Located.{value= cv; _}, None -> (
      match cv with
      | Bool _ ->
          Ok (Type.TypeRef "bool", expr)
      | Int _ ->
          Ok (Type.TypeRef "int", expr)
      | Float _ ->
          Ok (Type.TypeRef "float", expr) )
    | None, Some Located.{value= typ; _} ->
        Ok (typ, expr)
    | None, None ->
        Error Located.{loc; value= `UndeclaredIdentifier id}
    | Some _, Some _ ->
        failwith (Printf.sprintf "both constant and var found with id '%s'" id)
    )

let check_var_declaration env loc Ast.{var_ids; var_values} =
  let num_vars, num_values = List.(length var_ids, length var_values) in
  if num_vars <> num_values then
    Error Located.{loc; value= `AssignmentMismatch (num_vars, num_values)}
  else
    let vvs = List.combine var_ids var_values in
    List.fold_left
      (fun acc (id, rhs) ->
        match Env.find_name id env with
        | Some Located.{loc= prev_loc; _} ->
            Error Located.{loc; value= `Redefinition (id, prev_loc)}
        | None ->
            acc >>= fun (env, typed_stmts) ->
            check_expr env rhs >>= fun (typ, expr) ->
            let new_env = Env.(env |> add_var id Located.{loc; value= typ}) in
            let stmt = TypedAst.Var {var_id= id; var_value= (typ, expr)} in
            Ok (new_env, typed_stmts @ [(new_env, Located.{loc; value= stmt})])
        )
      (Ok (env, []))
      vvs

let check_stmt env loc = function
  | Ast.Var vd ->
      check_var_declaration env loc vd
  | Ast.Assignment _ ->
      (* TODO: implement *)
      Ok (env, [])
  | Ast.If _ ->
      (* TODO: implement *)
      Ok (env, [])
  | Ast.ForIter _ ->
      (* TODO: implement *)
      Ok (env, [])
  | Ast.ForRange _ ->
      (* TODO: implement *)
      Ok (env, [])
  | Ast.Return _ ->
      (* TODO: implement *)
      Ok (env, [])

let check_function_declaration env loc fd =
  let Ast.{fd_name; fd_type; fd_body} = fd in
  let env = Env.enter_function_scope fd_name env in
  check_type env loc fd_type >>= fun (local_env, clean_type) ->
  List.fold_left
    (fun acc Located.{loc; value= stmt} ->
      acc >>= fun (env, typed_stmts) ->
      check_stmt env loc stmt >>= fun (new_env, typed_stmt) ->
      Ok (new_env, typed_stmts @ typed_stmt) )
    (Ok (local_env, []))
    fd_body
  >>= fun (env, typed_stmts) ->
  Ok TypedAst.{fd_env= env; fd_name; fd_type= clean_type; fd_body= typed_stmts}

let check_pipeline_functions env pd_functions =
  List.fold_left
    (fun acc Located.{loc; value= fd} ->
      acc >>= fun typed_fds ->
      check_function_declaration env loc fd >>= fun typed_fd ->
      Ok (typed_fds @ [typed_fd]) )
    (Ok []) pd_functions

let check_pipeline_declaration env loc pd =
  let Ast.{pd_name; pd_type; pd_functions} = pd in
  match Env.find_name pd_name env with
  | Some Located.{loc= prev_loc; _} ->
      Error Located.{loc; value= `Redefinition (pd_name, prev_loc)}
  | None ->
      let local_env = Env.enter_pipeline_scope pd_name env in
      check_type local_env loc pd_type >>= fun (local_env, clean_type) ->
      check_pipeline_signatures local_env pd_functions >>= fun local_env ->
      check_pipeline_functions local_env pd_functions
      >>= fun typed_functions ->
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
