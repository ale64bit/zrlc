open Monad.Result
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
    match Env.find_constant ~local:false name env with
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
      if Env.type_exists name env then Ok t
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
          check_type env ctx_loc field.Type.t >>= fun nt ->
          Ok (new_fields @ [{field with t= nt}]) )
        (Ok []) fields
      >>= fun new_fields -> Ok (Type.Record new_fields)
  | Type.Array (t, dims) ->
      check_type env ctx_loc t >>= fun nt ->
      List.fold_left
        (fun acc dim ->
          acc >>= fun new_dims ->
          check_array_dim env ctx_loc dim >>= fun ndim -> Ok (new_dims @ [ndim])
          )
        (Ok []) dims
      >>= fun new_dims -> Ok (Type.Array (nt, new_dims))
  | Type.Function (args, rets) ->
      let idfn arg = arg.Type.name in
      let errfn arg =
        Located.{loc= ctx_loc; value= `DuplicateParameter (idfn arg)}
      in
      check_unique idfn errfn args >>= fun _ ->
      List.fold_left
        (fun acc f ->
          acc >>= fun new_args ->
          check_type env ctx_loc f.Type.t >>= fun nt ->
          let new_arg = {f with t= nt} in
          Ok (new_args @ [new_arg]) )
        (Ok []) args
      >>= fun new_args ->
      List.fold_left
        (fun acc t ->
          acc >>= fun new_rets ->
          check_type env ctx_loc t >>= fun nt -> Ok (new_rets @ [nt]) )
        (Ok []) rets
      >>= fun new_rets -> Ok (Type.Function (new_args, new_rets))
  | Type.Primitive _ ->
      Ok t

let build_function_environment env loc typ =
  match typ with
  | Type.Function (args, _) ->
      List.fold_left
        (fun env Type.{name; t} -> Env.add_var name Located.{loc; value= t} env)
        env args
  | _ ->
      failwith "Cannot build function environment from non-function type"

let check_const_declaration env loc cd =
  let Ast.{cd_name; cd_value} = cd in
  match Env.find_name ~local:true cd_name env with
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

let check_type_declaration env loc td =
  let Ast.{td_name; td_type} = td in
  match Env.find_name ~local:true td_name env with
  | Some Located.{loc= prev_loc; _} ->
      Error Located.{loc; value= `Redefinition (td_name, prev_loc)}
  | None -> (
    match td_type with
    | Record _ ->
        check_type env loc td_type >>= fun clean_type ->
        let env = Env.add_type td_name {loc; value= clean_type} env in
        let typed_td = TypedAst.TypeDecl {td_name; td_type= clean_type} in
        Ok (env, typed_td)
    | TypeRef _ | Array _ | Function _ | Primitive _ ->
        failwith
          (Printf.sprintf
             "type %s: type declarations should always be Type.Record" td_name)
    )

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
    match Env.find_rvalue id env with
    | Some Located.{value= typ; _} ->
        Ok (typ, expr)
    | None ->
        Error Located.{loc; value= `UndeclaredIdentifier id} )

let check_var_declaration env loc Ast.{var_ids; var_values} =
  let num_vars, num_values = List.(length var_ids, length var_values) in
  if num_vars <> num_values then
    Error Located.{loc; value= `AssignmentMismatch (num_vars, num_values)}
  else
    let vvs = List.combine var_ids var_values in
    List.fold_left
      (fun acc (id, rhs) ->
        match Env.find_name ~local:true id env with
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
  check_type env loc fd_type >>= fun clean_type ->
  let env = Env.enter_function_scope fd_name env in
  let env = build_function_environment env loc clean_type in
  List.fold_left
    (fun acc Located.{loc; value= stmt} ->
      acc >>= fun (env, typed_stmts) ->
      check_stmt env loc stmt >>= fun (env, typed_stmt) ->
      Ok (env, typed_stmts @ typed_stmt) )
    (Ok (env, []))
    fd_body
  >>= fun (env, typed_stmts) ->
  Ok TypedAst.{fd_env= env; fd_name; fd_type= clean_type; fd_body= typed_stmts}

let check_pipeline_declaration_sig env loc pd =
  let Ast.{pd_name; pd_type; _} = pd in
  match Env.find_name ~local:true pd_name env with
  | Some Located.{loc= prev_loc; _} ->
      Error Located.{loc; value= `Redefinition (pd_name, prev_loc)}
  | None ->
      check_type env loc pd_type >>= fun clean_type ->
      Ok (Env.add_pipeline pd_name {loc; value= clean_type} env)

let check_function_sig env loc fd =
  let Ast.{fd_name; fd_type; _} = fd in
  match Env.find_name ~local:true fd_name env with
  | Some Located.{loc= prev_loc; _} ->
      Error Located.{loc; value= `Redefinition (fd_name, prev_loc)}
  | None ->
      check_type env loc fd_type >>= fun clean_type ->
      Ok (Env.add_function fd_name {loc; value= clean_type} env)

let check_pipeline_declaration_body env loc pd =
  let Ast.{pd_name; pd_type; pd_functions} = pd in
  check_type env loc pd_type >>= fun clean_type ->
  let env = Env.enter_pipeline_scope pd_name env in
  let env = build_function_environment env loc clean_type in
  List.fold_left
    (fun acc Located.{loc; value} ->
      acc >>= fun env -> check_function_sig env loc value )
    (Ok env) pd_functions
  >>= fun env ->
  List.fold_left
    (fun acc Located.{loc; value} ->
      acc >>= fun typed_functions ->
      check_function_declaration env loc value >>= fun typed_fd ->
      Ok (typed_functions @ [typed_fd]) )
    (Ok []) pd_functions
  >>= fun typed_functions ->
  Ok
    (TypedAst.PipelineDecl
       { pd_env= env
       ; pd_name
       ; pd_type= clean_type
       ; pd_functions= typed_functions })

(* TODO: implement *)
let check_renderer_declaration_sig env _ _ = Ok env

(* TODO: implement *)
let check_renderer_declaration_body env _ rd =
  let Ast.{rd_name; rd_type; _} = rd in
  Ok (TypedAst.RendererDecl {rd_env= env; rd_name; rd_type; rd_body= []})

let check Ast.{module_name; elements} =
  let env = Env.(global |> enter_module_scope module_name) in
  (* First check all signatures and populate the module scope *)
  List.fold_left
    (fun acc Located.{loc; value} ->
      acc >>= fun (env, typed_root_elems) ->
      match value with
      | Ast.ConstDecl cd ->
          check_const_declaration env loc cd >>= fun (env, typed_cd) ->
          Ok (env, typed_root_elems @ [typed_cd])
      | Ast.TypeDecl td ->
          check_type_declaration env loc td >>= fun (env, typed_td) ->
          Ok (env, typed_root_elems @ [typed_td])
      | Ast.PipelineDecl pd ->
          check_pipeline_declaration_sig env loc pd >>= fun env ->
          Ok (env, typed_root_elems)
      | Ast.RendererDecl rd ->
          check_renderer_declaration_sig env loc rd >>= fun env ->
          Ok (env, typed_root_elems) )
    (Ok (env, []))
    elements
  >>= fun (env, typed_root_elems) ->
  (* Then check the actual bodies of pipelines and renderers *)
  List.fold_left
    (fun acc Located.{loc; value} ->
      acc >>= fun typed_root_elems ->
      match value with
      | Ast.ConstDecl _ ->
          acc
      | Ast.TypeDecl _ ->
          acc
      | Ast.PipelineDecl pd ->
          check_pipeline_declaration_body env loc pd >>= fun typed_pd ->
          Ok (typed_root_elems @ [typed_pd])
      | Ast.RendererDecl rd ->
          check_renderer_declaration_body env loc rd >>= fun typed_rd ->
          Ok (typed_root_elems @ [typed_rd]) )
    (Ok typed_root_elems) elements
  >>= fun typed_root_elems ->
  Ok
    TypedAst.
      {root_env= env; root_module= module_name; root_elems= typed_root_elems}
