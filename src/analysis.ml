open Monad.Result
open Extensions
module SS = Set.Make (String)

type error =
  [ `Redefinition of string * (Lexing.position * Lexing.position)
  | `DuplicateMember of string
  | `DuplicateParameter of string
  | `Unimplemented of string
  | `UnknownTypeName of string
  | `NonIntegerArraySize
  | `NonIntegerArrayIndex of Ast.expression
  | `UndeclaredIdentifier of string
  | `AssignmentMismatch of int * int
  | `InvalidUnaryOperation of Ast.unop * Type.t
  | `InvalidBinaryOperation of Ast.expression * Type.t * Type.t
  | `InvalidIndexOperation of Ast.expression * Type.t
  | `InvalidCallOperation of Ast.expression * Type.t
  | `NotAnExpression of string
  | `NoSuchMember of Type.t * string
  | `NotEnoughArguments of Ast.expression * Type.t list * Type.t list
  | `TooManyArguments of Ast.expression * Type.t list * Type.t list
  | `NotEnoughReturnArguments of Type.t list * Type.t list
  | `TooManyReturnArguments of Type.t list * Type.t list
  | `NotEnoughIndices of Ast.expression * int * int
  | `TooManyIndices of Ast.expression * int * int
  | `MultipleValueInSingleValueContext of Ast.expression
  | `MixedArgumentStyle of Ast.expression
  | `InvalidArgument of Ast.expression * Type.t * Type.t * string
  | `InvalidReturnArgument of Ast.expression * Type.t * Type.t
  | `MissingNamedArgument of string * string
  | `UnexpectedNamedArgument of string * string
  | `UnitUsedAsValue of Ast.expression ]

(* Helpers *)

let error loc e = Error Located.{loc; value= e}

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

(* Checks the given type against the environment *)
let rec check_type env ctx_loc t =
  match t with
  | Type.TypeRef name ->
      if Env.type_exists name env then Ok t
      else Error Located.{loc= ctx_loc; value= `UnknownTypeName name}
  | Type.Record fields ->
      let idfn (name, _) = name in
      let errfn field =
        Located.{loc= ctx_loc; value= `DuplicateMember (idfn field)}
      in
      check_unique idfn errfn fields >>= fun _ ->
      List.fold_results
        (fun acc (name, t) ->
          acc >>= fun new_fields ->
          check_type env ctx_loc t >>= fun nt -> Ok ((name, nt) :: new_fields)
          )
        (Ok []) fields
      >>= fun new_fields -> Ok (Type.Record new_fields)
  | Type.Array (t, dims) ->
      check_type env ctx_loc t >>= fun nt ->
      List.fold_results
        (fun acc dim ->
          acc >>= fun new_dims ->
          check_array_dim env ctx_loc dim >>= fun ndim -> Ok (ndim :: new_dims)
          )
        (Ok []) dims
      >>= fun new_dims -> Ok (Type.Array (nt, new_dims))
  | Type.Function (args, rets) ->
      let idfn (name, _) = name in
      let errfn arg =
        Located.{loc= ctx_loc; value= `DuplicateParameter (idfn arg)}
      in
      check_unique idfn errfn args >>= fun _ ->
      List.fold_results
        (fun acc (name, t) ->
          acc >>= fun new_args ->
          check_type env ctx_loc t >>= fun nt -> Ok ((name, nt) :: new_args) )
        (Ok []) args
      >>= fun new_args ->
      List.fold_results
        (fun acc t ->
          acc >>= fun new_rets ->
          check_type env ctx_loc t >>= fun nt -> Ok (nt :: new_rets) )
        (Ok []) rets
      >>= fun new_rets -> Ok (Type.Function (new_args, new_rets))
  | Type.Primitive _ ->
      Ok t

let build_function_environment env loc typ =
  match typ with
  | Type.Function (args, _) ->
      List.fold_left
        (fun env (name, t) -> Env.add_var name Located.{loc; value= t} env)
        env args
  | _ ->
      failwith "cannot build function environment from non-function type"

let check_const_declaration env loc cd =
  let Ast.{cd_name; cd_value} = cd in
  match Env.find_name ~local:true cd_name env with
  | Some Located.{loc= prev_loc; _} ->
      error loc (`Redefinition (cd_name, prev_loc))
  | None -> (
    match cd_value.value with
    | Ast.BoolLiteral b ->
        let value = Located.{loc; value= Env.Bool b} in
        let env = Env.add_constant cd_name value env in
        let t = Type.TypeRef "bool" in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= ([t], cd_value)} in
        Ok (env, cd)
    | Ast.IntLiteral i ->
        let value = Located.{loc; value= Env.Int i} in
        let env = Env.add_constant cd_name value env in
        let t = Type.TypeRef "int" in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= ([t], cd_value)} in
        Ok (env, cd)
    | Ast.FloatLiteral f ->
        let value = Located.{loc; value= Env.Float f} in
        let env = Env.add_constant cd_name value env in
        let t = Type.TypeRef "float" in
        let cd = TypedAst.ConstDecl {cd_name; cd_value= ([t], cd_value)} in
        Ok (env, cd)
    | _ ->
        error loc
          (`Unimplemented
            "constant initializer must be a boolean, integer or float literal")
    )

let check_type_declaration env loc td =
  let Ast.{td_name; td_type} = td in
  match Env.find_name ~local:true td_name env with
  | Some Located.{loc= prev_loc; _} ->
      error loc (`Redefinition (td_name, prev_loc))
  | None -> (
    match td_type with
    | Record _ ->
        check_type env loc td_type >>= fun clean_type ->
        let env = Env.add_type td_name {loc; value= clean_type} env in
        let typed_td = TypedAst.TypeDecl {td_name; td_type= clean_type} in
        Ok (env, typed_td)
    | _ ->
        failwith
          (Printf.sprintf
             "type %s: type declarations should always be Type.Record" td_name)
    )

(* Expressions *)

let check_unop loc op typ =
  let open Ast in
  let open Type in
  match (op, typ) with
  (* Unary Plus and Minus *)
  | (UPlus | UMinus), TypeRef "int"
  | (UPlus | UMinus), TypeRef "uint"
  | (UPlus | UMinus), TypeRef "float"
  | (UPlus | UMinus), TypeRef "double"
  | (UPlus | UMinus), Primitive Int
  | (UPlus | UMinus), Primitive UInt
  | (UPlus | UMinus), Primitive Float
  | (UPlus | UMinus), Primitive Double ->
      Ok typ
  (* Logical NOT *)
  | LogicalNot, TypeRef "bool" | LogicalNot, Primitive Bool ->
      Ok typ
  (* Bitwise Complement *)
  | BitwiseComplement, TypeRef "int"
  | BitwiseComplement, TypeRef "uint"
  | BitwiseComplement, Primitive Int
  | BitwiseComplement, Primitive UInt ->
      Ok typ
  (* TODO: add cases for non-primitive builtin types *)
  | _, _ ->
      error loc (`InvalidUnaryOperation (op, typ))

let check_binop expr ltyp op rtyp =
  let Located.{loc; _} = expr in
  let open Ast in
  let open Type in
  match (ltyp, op, rtyp) with
  (* Logical *)
  | ( (Primitive Bool | TypeRef "bool")
    , (LogicalOr | LogicalXor | LogicalAnd)
    , (Primitive Bool | TypeRef "bool") ) ->
      Ok (TypeRef "bool")
  (* Bitwise *)
  | ( (Primitive Int | TypeRef "int")
    , (BitwiseOr | BitwiseXor | BitwiseAnd | ShiftLeft | ShiftRight)
    , (Primitive Int | TypeRef "int") ) ->
      Ok (TypeRef "int")
  (* Comparison *)
  | ( (Primitive Bool | TypeRef "bool")
    , (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual)
    , (Primitive Bool | TypeRef "bool") )
  | ( (Primitive Int | TypeRef "int")
    , (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual)
    , (Primitive Int | TypeRef "int") )
  | ( (Primitive UInt | TypeRef "uint")
    , (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual)
    , (Primitive UInt | TypeRef "uint") )
  | ( (Primitive Float | TypeRef "float")
    , (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual)
    , (Primitive Float | TypeRef "float") )
  | ( (Primitive Double | TypeRef "double")
    , (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual)
    , (Primitive Double | TypeRef "double") ) ->
      Ok (TypeRef "bool")
  (* Arithmetic *)
  | ( (Primitive Int | TypeRef "int")
    , (Plus | Minus | Mult | Div | Mod)
    , (Primitive Int | TypeRef "int") ) ->
      Ok (TypeRef "int")
  | ( (Primitive UInt | TypeRef "uint")
    , (Plus | Minus | Mult | Div | Mod)
    , (Primitive UInt | TypeRef "uint") ) ->
      Ok (TypeRef "uint")
  | ( (Primitive Float | TypeRef "float")
    , (Plus | Minus | Mult | Div)
    , (Primitive Float | TypeRef "float") ) ->
      Ok (TypeRef "float")
  | ( (Primitive Double | TypeRef "double")
    , (Plus | Minus | Mult | Div)
    , (Primitive Double | TypeRef "double") ) ->
      Ok (TypeRef "double")
  (* TODO: add cases for non-primitive builtin types *)
  | _, _, _ ->
      error loc (`InvalidBinaryOperation (expr, ltyp, rtyp))

let rec check_access env loc expr id =
  check_single_value_expr env expr >>= fun typ ->
  let err = error loc (`NoSuchMember (typ, id)) in
  match typ with
  | Type.TypeRef name -> (
    match Env.find_type ~local:false name env with
    | Some Located.{value= Type.Record fields; _} -> (
      match List.find_opt (fun (name, _) -> name = id) fields with
      | Some (_, t) ->
          Ok [t]
      | None ->
          err )
    | _ ->
        failwith "name cannot exist in environment without a known type" )
  | _ ->
      err

and check_index env loc expr index_exprs =
  let open Type in
  check_single_value_expr env expr >>= fun expr_type ->
  List.fold_results
    (fun acc index_expr ->
      acc >>= fun index_types ->
      check_single_value_expr env index_expr >>= fun index_type ->
      Ok (index_type :: index_types) )
    (Ok []) index_exprs
  >>= fun index_types ->
  match expr_type with
  | Array (t, dims) ->
      let have = List.length index_types in
      let want = List.length dims in
      if have < want then error loc (`NotEnoughIndices (expr, have, want))
      else if have > want then error loc (`TooManyIndices (expr, have, want))
      else
        List.fold_left
          (fun acc (index_expr, index_type) ->
            acc >>= fun _ ->
            match index_type with
            | Primitive Int | Primitive UInt | TypeRef "int" | TypeRef "uint"
              ->
                acc
            | _ ->
                let Located.{loc; _} = index_expr in
                error loc (`NonIntegerArrayIndex index_expr) )
          (Ok ())
          (List.combine index_exprs index_types)
        >>= fun () -> Ok [t]
  | _ ->
      let expr = Located.{loc; value= Ast.Index (expr, index_exprs)} in
      error loc (`InvalidIndexOperation (expr, expr_type))

and check_call env loc f_expr arg_exprs =
  check_single_value_expr env f_expr >>= fun f_type ->
  List.fold_results
    (fun acc arg_expr ->
      acc >>= fun arg_types ->
      check_single_value_expr env arg_expr >>= fun arg_type ->
      Ok (arg_type :: arg_types) )
    (Ok []) arg_exprs
  >>= fun arg_types ->
  match f_type with
  | Type.Function (args, ret) -> (
      let have = List.length arg_types in
      let want = List.length args in
      let want_types = List.map (fun (_, t) -> t) args in
      if have < want then
        error loc (`NotEnoughArguments (f_expr, arg_types, want_types))
      else if have > want then
        error loc (`TooManyArguments (f_expr, arg_types, want_types))
      else
        match f_expr with
        | Located.{value= Ast.Id name; _} -> (
            let is_function = Env.function_exists name env in
            let is_pipeline = Env.pipeline_exists name env in
            let is_named = function
              | Located.{value= Ast.NamedArg _; _} ->
                  true
              | _ ->
                  false
            in
            let all_named = List.for_all is_named arg_exprs in
            let all_unnamed =
              List.for_all (fun x -> not (is_named x)) arg_exprs
            in
            match (is_function, is_pipeline, all_named, all_unnamed) with
            (* Case #0: function + no arguments *)
            | true, false, true, true ->
                let () = assert (List.length arg_exprs = 0) in
                check_call_args name arg_exprs arg_types want_types
                >>= fun _ -> Ok ret
            (* Case #1: function + unnamed arguments *)
            | true, false, false, true ->
                check_call_args name arg_exprs arg_types want_types
                >>= fun _ -> Ok ret
            (* Case #2: function + named arguments *)
            | true, false, true, false ->
                check_call_named_args loc name f_type arg_exprs arg_types
                  want_types
                >>= fun _ -> Ok ret
            | _, _, false, false ->
                let expr =
                  Located.{loc; value= Ast.Call (f_expr, arg_exprs)}
                in
                error loc (`MixedArgumentStyle expr)
            | _ ->
                error loc (`Unimplemented "not implemented")
            (* 
                 * TODO: implement 
                 * Case #3: pipeline + unnamed arguments 
                 * Case #4: pipeline + named arguments
                 * *)
            )
        | _ ->
            failwith
              "function types should only be called from their IDs since they \
               cannot be built anonymously" )
  | _ ->
      error loc (`InvalidCallOperation (f_expr, f_type))

and check_call_args f_name arg_exprs arg_types want_types =
  List.fold_left
    (fun acc ((arg_expr, arg_type), want_type) ->
      acc >>= fun _ ->
      if arg_type <> want_type then
        let Located.{loc; _} = arg_expr in
        error loc (`InvalidArgument (arg_expr, arg_type, want_type, f_name))
      else Ok () )
    (Ok ())
    (List.combine (List.combine arg_exprs arg_types) want_types)

and check_call_named_args loc f_name f_type arg_exprs arg_types want_types =
  match f_type with
  | Type.Function (params, _) ->
      let args = List.combine arg_exprs arg_types in
      (* Check that there are no unexpected named arguments *)
      List.fold_left
        (fun acc Located.{value; _} ->
          acc >>= fun _ ->
          match value with
          | Ast.NamedArg (arg_name, _) ->
              if List.exists (fun (name, _) -> name = arg_name) params then
                Ok ()
              else error loc (`UnexpectedNamedArgument (arg_name, f_name))
          | _ ->
              failwith "expected all arguments to be named" )
        (Ok ()) arg_exprs
      >>= fun () ->
      (* Check that all required named arguments are provided *)
      List.fold_left
        (fun acc (name, _) ->
          acc >>= fun (new_arg_exprs, new_arg_types) ->
          let arg =
            List.find_opt
              (fun (arg_expr, _) ->
                match arg_expr with
                | Located.{value= Ast.NamedArg (arg_name, _); _} ->
                    name = arg_name
                | _ ->
                    failwith "expected all arguments to be named" )
              args
          in
          match arg with
          | Some (arg_expr, arg_type) ->
              Ok (new_arg_exprs @ [arg_expr], new_arg_types @ [arg_type])
          | None ->
              error loc (`MissingNamedArgument (name, f_name)) )
        (Ok ([], []))
        params
      >>= fun (new_arg_exprs, new_arg_types) ->
      check_call_args f_name new_arg_exprs new_arg_types want_types
  | _ ->
      failwith "f_type must be a Function type"

and check_bundled_arg env exprs =
  List.fold_results
    (fun acc expr ->
      acc >>= fun expr_types ->
      check_single_value_expr env expr >>= fun expr_type ->
      Ok (expr_type :: expr_types) )
    (Ok []) exprs

and check_id env loc id =
  match Env.find_rvalue id env with
  | Some Located.{value= typ; _} ->
      Ok [typ]
  | None -> (
    match Env.find_name ~local:false id env with
    | Some _ ->
        error loc (`NotAnExpression id)
    | None ->
        error loc (`UndeclaredIdentifier id) )

and check_single_value_expr env expr =
  let Located.{loc; _} = expr in
  check_expr env expr >>= function
  | [] ->
      error loc (`UnitUsedAsValue expr)
  | [typ] ->
      Ok typ
  | _ ->
      error loc (`MultipleValueInSingleValueContext expr)

and check_expr env expr =
  let open Ast in
  let Located.{loc; value} = expr in
  match value with
  | Access (expr, id) ->
      check_access env loc expr id
  | Index (expr, index_exprs) ->
      check_index env loc expr index_exprs
  | Call (f_expr, arg_exprs) ->
      check_call env loc f_expr arg_exprs
  | NamedArg (_, expr) ->
      check_single_value_expr env expr >>= fun typ -> Ok [typ]
  | BundledArg exprs ->
      check_bundled_arg env exprs
  | BinExpr (lhs, op, rhs) ->
      check_single_value_expr env lhs >>= fun ltyp ->
      check_single_value_expr env rhs >>= fun rtyp ->
      check_binop expr ltyp op rtyp >>= fun typ -> Ok [typ]
  | UnExpr (op, rhs) ->
      check_single_value_expr env rhs >>= fun typ ->
      check_unop loc op typ >>= fun typ -> Ok [typ]
  | BoolLiteral _ ->
      Ok [Type.TypeRef "bool"]
  | IntLiteral _ ->
      Ok [Type.TypeRef "int"]
  | FloatLiteral _ ->
      Ok [Type.TypeRef "float"]
  | Id id ->
      check_id env loc id

and check_expr_list env have_exprs want_types less_errfn more_errfn errfn =
  let num_have = List.length have_exprs in
  let num_want = List.length want_types in
  if num_have = 1 then
    let have_expr = List.hd have_exprs in
    check_expr env have_expr >>= fun have_types ->
    let num_have = List.length have_types in
    if num_have < num_want then less_errfn have_types
    else if num_have > num_want then more_errfn have_types
    else
      List.fold_left
        (fun acc (have_type, want_type) ->
          acc >>= fun _ ->
          if have_type = want_type then Ok ()
          else errfn have_expr have_type want_type )
        (Ok ())
        (List.combine have_types want_types)
  else if num_have <> num_want then
    List.fold_left
      (fun acc have_expr ->
        acc >>= fun have_types ->
        check_single_value_expr env have_expr >>= fun have_type ->
        Ok (have_type :: have_types) )
      (Ok []) have_exprs
    >>= fun have_types ->
    if num_have < num_want then less_errfn have_types
    else more_errfn have_types
  else
    List.fold_left
      (fun acc (have_expr, want_type) ->
        acc >>= fun _ ->
        check_single_value_expr env have_expr >>= fun have_type ->
        if have_type = want_type then Ok ()
        else errfn have_expr have_type want_type )
      (Ok ())
      (List.combine have_exprs want_types)

(* Statements *)

let check_var_declaration env loc ids exprs =
  let declare_local_vars types =
    List.fold_left
      (fun acc (id, t) ->
        acc >>= fun env ->
        match Env.find_name ~local:true id env with
        | Some Located.{loc= prev_loc; _} ->
            error loc (`Redefinition (id, prev_loc))
        | None ->
            Ok Env.(env |> add_var id Located.{loc; value= t}) )
      (Ok env) (List.combine ids types)
  in
  match exprs with
  | [expr] ->
      check_expr env expr >>= fun expr_types ->
      let num_vars, num_values = List.(length ids, length expr_types) in
      if num_values = 0 then error loc (`UnitUsedAsValue expr)
      else if num_vars <> num_values then
        error loc (`AssignmentMismatch (num_vars, num_values))
      else
        declare_local_vars expr_types >>= fun new_env ->
        let typed_stmt =
          TypedAst.Var {var_ids= ids; var_values= [(expr_types, expr)]}
        in
        Ok (new_env, [(env, Located.{loc; value= typed_stmt})])
  | _ ->
      List.fold_results
        (fun acc expr ->
          acc >>= fun expr_types ->
          check_single_value_expr env expr >>= fun expr_type ->
          Ok (expr_type :: expr_types) )
        (Ok []) exprs
      >>= fun expr_types ->
      let num_vars, num_values = List.(length ids, length exprs) in
      if num_vars <> num_values then
        error loc (`AssignmentMismatch (num_vars, num_values))
      else
        declare_local_vars expr_types >>= fun new_env ->
        let typed_stmt =
          TypedAst.Var
            { var_ids= ids
            ; var_values=
                List.map
                  (fun (t, e) -> ([t], e))
                  (List.combine expr_types exprs) }
        in
        Ok (new_env, [(env, Located.{loc; value= typed_stmt})])

(* TODO: check_assignment
let check_assignment env loc Ast.{asg_op; asg_lvalues; asg_rvalues} =
  let num_lhs = List.length asg_lvalues in
  let num_rhs = List.length asg_rvalues in
  if num_lhs <> num_rhs then error loc (`AssignmentMismatch (num_lhs, num_rhs))
  else
    List.fold_left
    (fun acc (lhs, rhs) ->
      acc >>= fun  (typed_lvals, typed_rvals) ->
      check_single_value_expr 
    )
    (Ok ([], []))
    (List.combine asg_lvalues asg_rvalues)
*)

(* TODO: check_if *)
(* TODO: check_for_iter *)
(* TODO: check_for_range *)

let check_return env loc exprs =
  match Env.scope_summary env with
  | Env.Function (Type.Function (_, ret_types)) ->
      let less_errfn have_types =
        error loc (`NotEnoughReturnArguments (have_types, ret_types))
      in
      let more_errfn have_types =
        error loc (`TooManyReturnArguments (have_types, ret_types))
      in
      let errfn have_expr have_type want_type =
        let Located.{loc; _} = have_expr in
        error loc (`InvalidReturnArgument (have_expr, have_type, want_type))
      in
      check_expr_list env exprs ret_types less_errfn more_errfn errfn
      >>= fun () ->
      let stmt =
        TypedAst.Return
          (List.map (fun (t, e) -> ([t], e)) (List.combine ret_types exprs))
      in
      Ok (env, [(env, Located.{loc; value= stmt})])
  | _ ->
      failwith "return can only appear in Function scopes"

let check_stmt env loc =
  let open Ast in
  function
  | Var {var_ids; var_values} ->
      check_var_declaration env loc var_ids var_values
  | Assignment _ ->
      (* TODO: implement *)
      Ok (env, [])
  | If _ ->
      (* TODO: implement *)
      Ok (env, [])
  | ForIter _ ->
      (* TODO: implement *)
      Ok (env, [])
  | ForRange _ ->
      (* TODO: implement *)
      Ok (env, [])
  | Return exprs ->
      check_return env loc exprs

(* Top-Level Elements *)

let check_function_declaration env loc fd =
  let Ast.{fd_name; fd_type; fd_body} = fd in
  check_type env loc fd_type >>= fun clean_type ->
  let env = Env.enter_function_scope fd_name clean_type env in
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
      error loc (`Redefinition (pd_name, prev_loc))
  | None ->
      check_type env loc pd_type >>= fun clean_type ->
      Ok (Env.add_pipeline pd_name {loc; value= clean_type} env)

let check_function_sig env loc fd =
  let Ast.{fd_name; fd_type; _} = fd in
  match Env.find_name ~local:true fd_name env with
  | Some Located.{loc= prev_loc; _} ->
      error loc (`Redefinition (fd_name, prev_loc))
  | None ->
      check_type env loc fd_type >>= fun clean_type ->
      Ok (Env.add_function fd_name {loc; value= clean_type} env)

let check_pipeline_declaration_body env loc pd =
  let Ast.{pd_name; pd_type; pd_functions} = pd in
  check_type env loc pd_type >>= fun clean_type ->
  let env = Env.enter_pipeline_scope pd_name clean_type env in
  let env = build_function_environment env loc clean_type in
  List.fold_left
    (fun acc Located.{loc; value} ->
      acc >>= fun env -> check_function_sig env loc value )
    (Ok env) pd_functions
  >>= fun env ->
  List.fold_results
    (fun acc Located.{loc; value} ->
      acc >>= fun typed_functions ->
      check_function_declaration env loc value >>= fun typed_fd ->
      Ok (typed_fd :: typed_functions) )
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
