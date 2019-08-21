open Monad.Result
open Extensions
module SS = Set.Make (String)
module L = Located

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
  | `InvalidCast of Type.t * Type.t
  | `NotAnExpression of string
  | `NoSuchMember of Type.t * string
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
  | `UnitUsedAsValue of Ast.expression
  | `NotAnLValue of Ast.expression
  | `InvalidSingleAssignment of Ast.expression * Type.t * Type.t
  | `InvalidMultipleAssignment of Type.t * Ast.expression * Type.t
  | `NonBoolIfCondition of Ast.expression * Type.t
  | `CannotRangeOver of Ast.expression * Type.t
  | `NonIntegerRangeExpression of Ast.expression * Type.t
  | `MissingReturn of string
  | `NoMatchingFunction of string * Type.t list ]

(* Helpers *)

let error loc e = Error L.{ loc; value = e }

let valid_swizzle_masks =
  let gen_swizzles s src =
    let n = String.length src in
    let rec aux k mask =
      if k = 0 then SS.empty
      else
        List.fold_left
          (fun s i ->
            let new_mask = Printf.sprintf "%s%c" mask src.[i] in
            let more = aux (k - 1) new_mask in
            SS.(s |> add new_mask |> add_seq (to_seq more)))
          SS.empty
          (List.init n (fun i -> i))
    in
    SS.add_seq (SS.to_seq (aux n "")) s
  in
  let s2 = List.fold_left gen_swizzles SS.empty [ "rg"; "xy"; "st" ] in
  let s3 = List.fold_left gen_swizzles SS.empty [ "rgb"; "xyz"; "stp" ] in
  let s4 = List.fold_left gen_swizzles SS.empty [ "rgba"; "xyzw"; "stpq" ] in
  (s2, s3, s4)

let check_unique idfn errfn elems =
  List.fold_left
    (fun acc elem ->
      acc >>= fun seen ->
      let id = idfn elem in
      if SS.mem id seen then errfn elem else Ok (SS.add id seen))
    (Ok SS.empty) elems

let check_valid_identifier loc id =
  if id = "builtin" then
    error loc (`Unimplemented "'builtin' is not a valid identifier")
  else if id <> "_" && (id.[0] = '_' || id.[String.length id - 1] = '_') then
    error loc
      (`Unimplemented "identifiers beginning or ending with _ are reserved")
  else Ok ()

(* Checks whether an array dimension is valid and returns the 
 * unfolded value in case it is a reference to a constant. *)
let check_array_dim env loc dim =
  match dim with
  | Type.OfInt _ -> Ok dim
  | Type.OfName name -> (
      match Env.find_constant ~local:false name env with
      | Some { value = Env.Int i; _ } -> Ok (Type.OfInt i)
      | Some _ -> Error L.{ loc; value = `NonIntegerArraySize }
      | None -> Error L.{ loc; value = `UndeclaredIdentifier name } )
  | Type.OfFloat _ | Type.OfBool _ ->
      Error L.{ loc; value = `NonIntegerArraySize }

(* Checks the given type against the environment and returns the normalized
 * version. *)
let rec check_type env ctx_loc t =
  let open Type in
  match t with
  | Unit -> Ok Unit
  | TypeRef name -> (
      match Env.find_type ~local:false name env with
      | Some L.{ value = Record _; _ } -> Ok (TypeRef name)
      | Some L.{ value = _ as t; _ } -> Ok t
      | None -> error ctx_loc (`UnknownTypeName name) )
  | Record fields ->
      let idfn (name, _) = name in
      let errfn field = error ctx_loc (`DuplicateMember (idfn field)) in
      check_unique idfn errfn fields >>= fun _ ->
      List.fold_results
        (fun acc (name, t) ->
          acc >>= fun new_fields ->
          check_valid_identifier ctx_loc name >>= fun () ->
          check_type env ctx_loc t >>= fun nt -> Ok ((name, nt) :: new_fields))
        (Ok []) fields
      >>= fun new_fields -> Ok (Record new_fields)
  | Array (t, dims) ->
      check_type env ctx_loc t >>= fun nt ->
      List.fold_results
        (fun acc dim ->
          acc >>= fun new_dims ->
          check_array_dim env ctx_loc dim >>= fun ndim -> Ok (ndim :: new_dims))
        (Ok []) dims
      >>= fun new_dims -> Ok (Array (nt, new_dims))
  | Function (args, rets) ->
      let idfn (name, _) = name in
      let errfn arg = error ctx_loc (`DuplicateParameter (idfn arg)) in
      check_unique idfn errfn args >>= fun _ ->
      List.fold_results
        (fun acc (name, t) ->
          acc >>= fun new_args ->
          check_type env ctx_loc t >>= fun nt -> Ok ((name, nt) :: new_args))
        (Ok []) args
      >>= fun new_args ->
      List.fold_results
        (fun acc t ->
          acc >>= fun new_rets ->
          check_type env ctx_loc t >>= fun nt -> Ok (nt :: new_rets))
        (Ok []) rets
      >>= fun new_rets -> Ok (Function (new_args, new_rets))
  | Primitive _ -> Ok t
  | Vector _ -> Ok t
  | Matrix _ -> Ok t
  | Sampler _ -> Ok t
  | SamplerCube -> Ok t
  | Texture _ -> Ok t
  | RenderTarget _ -> Ok t
  | Atom _ -> Ok t

let build_function_environment ~mutable_args env loc name typ =
  match typ with
  | Type.Function (args, _) ->
      let add_fn = if mutable_args then Env.add_var else Env.add_val in
      let env =
        List.fold_left
          (fun env (name, t) -> add_fn name L.{ loc; value = t } env)
          env args
      in
      Env.add_builtin name env
  | _ -> failwith "cannot build function environment from non-function type"

let check_const_declaration env loc cd =
  let Ast.{ cd_name; cd_value } = cd in
  check_valid_identifier loc cd_name >>= fun () ->
  match Env.find_name ~local:true cd_name env with
  | Some L.{ loc = prev_loc; _ } ->
      error loc (`Redefinition (cd_name, prev_loc))
  | None -> (
      match cd_value.value with
      | Ast.BoolLiteral b ->
          let value = L.{ loc; value = Env.Bool b } in
          let env = Env.add_constant cd_name value env in
          let t = Type.Primitive Bool in
          let cd =
            TypedAst.ConstDecl { cd_name; cd_value = ([ t ], cd_value) }
          in
          Ok (env, L.{ loc; value = cd })
      | Ast.IntLiteral i ->
          let value = L.{ loc; value = Env.Int i } in
          let env = Env.add_constant cd_name value env in
          let t = Type.Primitive Int in
          let cd =
            TypedAst.ConstDecl { cd_name; cd_value = ([ t ], cd_value) }
          in
          Ok (env, L.{ loc; value = cd })
      | Ast.FloatLiteral f ->
          let value = L.{ loc; value = Env.Float f } in
          let env = Env.add_constant cd_name value env in
          let t = Type.Primitive Float in
          let cd =
            TypedAst.ConstDecl { cd_name; cd_value = ([ t ], cd_value) }
          in
          Ok (env, L.{ loc; value = cd })
      | _ ->
          error loc
            (`Unimplemented
              "constant initializer must be a boolean, integer or float literal")
      )

let check_type_declaration env loc td =
  let Ast.{ td_name; td_type } = td in
  match Env.find_name ~local:false td_name env with
  | Some L.{ loc = prev_loc; _ } ->
      error loc (`Redefinition (td_name, prev_loc))
  | None -> (
      match td_type with
      | Record fields ->
          List.fold_results
            (fun acc (name, t) ->
              acc >>= fun fields ->
              check_type env loc t >>= fun clean_type ->
              Ok ((name, clean_type) :: fields))
            (Ok []) fields
          >>= fun fields ->
          check_valid_identifier loc td_name >>= fun () ->
          check_type env loc td_type >>= fun clean_type ->
          let ctor = Type.Function (fields, [ Type.TypeRef td_name ]) in
          let env = Env.add_type td_name { loc; value = clean_type } env in
          let env = Env.add_function td_name { loc; value = ctor } env in
          let typed_td = TypedAst.TypeDecl { td_name; td_type = clean_type } in
          Ok (env, L.{ loc; value = typed_td })
      | _ ->
          failwith
            (Printf.sprintf
               "type %s: type declarations should always be Type.Record"
               td_name) )

(* Expressions *)

let check_unop loc op typ =
  let open Ast in
  let open Type in
  match (op, typ) with
  (* Unary Plus and Minus *)
  | (UPlus | UMinus), Primitive (Int | UInt | Float | Double) -> Ok typ
  | (UPlus | UMinus), Vector ((Int | UInt | Float | Double), _) -> Ok typ
  | (UPlus | UMinus), Matrix ((Int | UInt | Float | Double), _, _) -> Ok typ
  (* Logical NOT *)
  | LogicalNot, Primitive Bool -> Ok typ
  (* Bitwise Complement *)
  | BitwiseComplement, Primitive (Int | UInt) -> Ok typ
  | _, _ -> error loc (`InvalidUnaryOperation (op, typ))

let check_binop expr ltyp op rtyp =
  let open Ast in
  let open Type in
  let L.{ loc; _ } = expr in
  match (ltyp, op, rtyp) with
  (* Logical *)
  | Primitive Bool, (LogicalOr | LogicalXor | LogicalAnd), Primitive Bool ->
      Ok (Primitive Bool)
  (* Bitwise *)
  | ( Primitive Int,
      (BitwiseOr | BitwiseXor | BitwiseAnd | ShiftLeft | ShiftRight),
      Primitive Int ) ->
      Ok (Primitive Int)
  | ( Primitive UInt,
      (BitwiseOr | BitwiseXor | BitwiseAnd | ShiftLeft | ShiftRight),
      Primitive UInt ) ->
      Ok (Primitive UInt)
  (* Primitive Comparison *)
  | Primitive Bool, (Equal | NotEqual), Primitive Bool
  | ( Primitive Int,
      (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual),
      Primitive Int )
  | ( Primitive UInt,
      (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual),
      Primitive UInt )
  | ( Primitive Float,
      (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual),
      Primitive Float )
  | ( Primitive Double,
      (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual),
      Primitive Double ) ->
      Ok (Primitive Bool)
  (* Vector Comparison *)
  | Vector (Bool, m), (Equal | NotEqual), Vector (Bool, n)
  | Vector (Int, m), (Equal | NotEqual), Vector (Int, n)
  | Vector (UInt, m), (Equal | NotEqual), Vector (UInt, n)
  | Vector (Float, m), (Equal | NotEqual), Vector (Float, n)
  | Vector (Double, m), (Equal | NotEqual), Vector (Double, n)
    when m = n ->
      Ok (Primitive Bool)
  (* Primitive Arithmetic *)
  | Primitive Int, (Plus | Minus | Mult | Div | Mod), Primitive Int ->
      Ok (Primitive Int)
  | Primitive UInt, (Plus | Minus | Mult | Div | Mod), Primitive UInt ->
      Ok (Primitive UInt)
  | Primitive Float, (Plus | Minus | Mult | Div), Primitive Float ->
      Ok (Primitive Float)
  | Primitive Double, (Plus | Minus | Mult | Div), Primitive Double ->
      Ok (Primitive Double)
  (* Vector-Scalar Arithmetic *)
  | Vector (Int, m), (Plus | Minus | Mult | Div), Primitive Int ->
      Ok (Vector (Int, m))
  | Vector (UInt, m), (Plus | Minus | Mult | Div), Primitive UInt ->
      Ok (Vector (UInt, m))
  | Vector (Float, m), (Plus | Minus | Mult | Div), Primitive Float ->
      Ok (Vector (Float, m))
  | Vector (Double, m), (Plus | Minus | Mult | Div), Primitive Double ->
      Ok (Vector (Double, m))
  | Primitive Int, (Plus | Minus | Mult), Vector (Int, m) ->
      Ok (Vector (Int, m))
  | Primitive UInt, (Plus | Minus | Mult), Vector (UInt, m) ->
      Ok (Vector (UInt, m))
  | Primitive Float, (Plus | Minus | Mult), Vector (Float, m) ->
      Ok (Vector (Float, m))
  | Primitive Double, (Plus | Minus | Mult), Vector (Double, m) ->
      Ok (Vector (Double, m))
  (* Vector-Vector Arithmetic - Element-wise *)
  | Vector (Int, m), (Plus | Minus | Mult | Div | Mod), Vector (Int, n)
    when m = n ->
      Ok (Vector (Int, m))
  | Vector (UInt, m), (Plus | Minus | Mult | Div | Mod), Vector (UInt, n)
    when m = n ->
      Ok (Vector (UInt, m))
  | Vector (Float, m), (Plus | Minus | Mult | Div), Vector (Float, n)
    when m = n ->
      Ok (Vector (Float, m))
  | Vector (Double, m), (Plus | Minus | Mult | Div), Vector (Double, n)
    when m = n ->
      Ok (Vector (Double, m))
  (* Matrix-Scalar Arithmetic *)
  | Matrix (Int, n, m), (Plus | Minus | Mult | Div | Mod), Primitive Int ->
      Ok (Matrix (Int, n, m))
  | Matrix (UInt, n, m), (Plus | Minus | Mult | Div | Mod), Primitive UInt ->
      Ok (Matrix (UInt, n, m))
  | Matrix (Float, n, m), (Plus | Minus | Mult | Div), Primitive Float ->
      Ok (Matrix (Float, n, m))
  | Matrix (Double, n, m), (Plus | Minus | Mult | Div), Primitive Double ->
      Ok (Matrix (Double, n, m))
  | Primitive Int, (Plus | Minus | Mult), Matrix (Int, n, m) ->
      Ok (Matrix (Int, n, m))
  | Primitive UInt, (Plus | Minus | Mult), Matrix (UInt, n, m) ->
      Ok (Matrix (UInt, n, m))
  | Primitive Float, (Plus | Minus | Mult), Matrix (Float, n, m) ->
      Ok (Matrix (Float, n, m))
  | Primitive Double, (Plus | Minus | Mult), Matrix (Double, n, m) ->
      Ok (Matrix (Double, n, m))
  (* Matrix-Vector Arithmetic *)
  | Matrix (Int, n, m), Mult, Vector (Int, p) when n = p ->
      Ok (Vector (Int, m))
  | Matrix (UInt, n, m), Mult, Vector (UInt, p) when n = p ->
      Ok (Vector (UInt, m))
  | Matrix (Float, n, m), Mult, Vector (Float, p) when n = p ->
      Ok (Vector (Float, m))
  | Matrix (Double, n, m), Mult, Vector (Double, p) when n = p ->
      Ok (Vector (Double, m))
  (* Matrix-Matrix Arithmetic *)
  | Matrix (Int, n, m), Mult, Matrix (Int, q, p) when n = p ->
      Ok (Matrix (Int, m, q))
  | Matrix (UInt, n, m), Mult, Matrix (UInt, q, p) when n = p ->
      Ok (Matrix (UInt, m, q))
  | Matrix (Float, n, m), Mult, Matrix (Float, q, p) when n = p ->
      Ok (Matrix (Float, m, q))
  | Matrix (Double, n, m), Mult, Matrix (Double, q, p) when n = p ->
      Ok (Matrix (Double, m, q))
  | _ -> error loc (`InvalidBinaryOperation (expr, ltyp, rtyp))

let check_assignop ltyp op rtyp err =
  let open Ast in
  let open Type in
  match (ltyp, op, rtyp) with
  (* Primitive *)
  | Primitive Bool, Assign, Primitive Bool -> Ok ()
  | Primitive Int, Assign, Primitive Int -> Ok ()
  | Primitive UInt, Assign, Primitive UInt -> Ok ()
  | ( Primitive Float,
      (Assign | AssignPlus | AssignMinus | AssignMult | AssignDiv),
      Primitive Float ) ->
      Ok ()
  | ( Primitive Double,
      (Assign | AssignPlus | AssignMinus | AssignMult | AssignDiv),
      Primitive Double ) ->
      Ok ()
  | Vector (Bool, m), Assign, Vector (Bool, n) when m = n -> Ok ()
  (* Vector *)
  | ( Vector (Int, m),
      (Assign | AssignPlus | AssignMinus | AssignMult),
      Vector (Int, n) )
  | ( Vector (UInt, m),
      (Assign | AssignPlus | AssignMinus | AssignMult),
      Vector (UInt, n) )
  | ( Vector (Float, m),
      (Assign | AssignPlus | AssignMinus | AssignMult),
      Vector (Float, n) )
  | ( Vector (Double, m),
      (Assign | AssignPlus | AssignMinus | AssignMult),
      Vector (Double, n) )
    when m = n ->
      Ok ()
  (* Vector-Primitive *)
  | Vector (Int, _), (AssignPlus | AssignMinus | AssignMult), Primitive Int
  | Vector (UInt, _), (AssignPlus | AssignMinus | AssignMult), Primitive UInt
  | Vector (Float, _), (AssignPlus | AssignMinus | AssignMult), Primitive Float
  | ( Vector (Double, _),
      (AssignPlus | AssignMinus | AssignMult),
      Primitive Double ) ->
      Ok ()
  (* Matrix *)
  | Matrix (Int, n, m), (Assign | AssignPlus | AssignMinus), Matrix (Int, q, p)
  | ( Matrix (UInt, n, m),
      (Assign | AssignPlus | AssignMinus),
      Matrix (UInt, q, p) )
  | ( Matrix (Float, n, m),
      (Assign | AssignPlus | AssignMinus),
      Matrix (Float, q, p) )
  | ( Matrix (Double, n, m),
      (Assign | AssignPlus | AssignMinus),
      Matrix (Double, q, p) )
    when m = p && n = q ->
      Ok ()
  (* Render Targets *)
  | RenderTarget RGB, (Assign | AssignPlus), Vector (Float, 3) -> Ok ()
  | RenderTarget RGBA, (Assign | AssignPlus), Vector (Float, 4) -> Ok ()
  | RenderTarget DS, (Assign | AssignPlus), Primitive Float -> Ok ()
  | _ -> err

let check_vector_access loc ptype dim id =
  let open Type in
  let m2, m3, m4 = valid_swizzle_masks in
  match (dim, String.length id) with
  | 2, 1 when SS.mem id m2 -> Ok (Primitive ptype)
  | 2, 2 when SS.mem id m2 -> Ok (Vector (ptype, 2))
  | 3, 1 when SS.mem id m3 -> Ok (Primitive ptype)
  | 3, 2 when SS.mem id m3 -> Ok (Vector (ptype, 2))
  | 3, 3 when SS.mem id m3 -> Ok (Vector (ptype, 3))
  | 4, 1 when SS.mem id m4 -> Ok (Primitive ptype)
  | 4, 2 when SS.mem id m4 -> Ok (Vector (ptype, 2))
  | 4, 3 when SS.mem id m4 -> Ok (Vector (ptype, 3))
  | 4, 4 when SS.mem id m4 -> Ok (Vector (ptype, 4))
  | _ -> error loc (`NoSuchMember (Vector (ptype, dim), id))

let rec check_access env loc expr id =
  let check_field_exists id fields err =
    match List.find_opt (fun (name, _) -> name = id) fields with
    | Some (_, t) -> Ok t
    | None -> err
  in
  check_single_value_expr env expr >>= fun typ ->
  let err = error loc (`NoSuchMember (typ, id)) in
  match typ with
  | Type.Vector (ptype, dim) -> check_vector_access loc ptype dim id
  | Type.TypeRef name -> (
      match Env.find_type ~local:false name env with
      | Some L.{ value = Type.Record fields; _ } ->
          check_field_exists id fields err
      | _ ->
          failwith
            ("name cannot exist in environment without a Record type: " ^ name)
      )
  | Type.Record fields -> check_field_exists id fields err
  | _ -> err

and check_index env loc expr index_exprs =
  let open Type in
  check_single_value_expr env expr >>= fun expr_type ->
  List.fold_results
    (fun acc index_expr ->
      acc >>= fun index_types ->
      check_single_value_expr env index_expr >>= fun index_type ->
      Ok (index_type :: index_types))
    (Ok []) index_exprs
  >>= fun index_types ->
  let check_index_exprs =
    List.fold_left
      (fun acc (index_expr, index_type) ->
        acc >>= fun _ ->
        match index_type with
        | Primitive (Int | UInt) -> acc
        | _ ->
            let L.{ loc; _ } = index_expr in
            error loc (`NonIntegerArrayIndex index_expr))
      (Ok ())
      (List.combine index_exprs index_types)
  in
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
            | Primitive (Int | UInt) -> acc
            | _ ->
                let L.{ loc; _ } = index_expr in
                error loc (`NonIntegerArrayIndex index_expr))
          (Ok ())
          (List.combine index_exprs index_types)
        >>= fun () -> Ok t
  | Matrix (pt, _, n) -> (
      let have = List.length index_types in
      match have with
      | 1 -> check_index_exprs >>= fun () -> Ok (Vector (pt, n))
      | 2 -> check_index_exprs >>= fun () -> Ok (Primitive pt)
      | _ -> error loc (`TooManyIndices (expr, have, 2)) )
  | _ ->
      let expr = L.{ loc; value = Ast.Index (expr, index_exprs) } in
      error loc (`InvalidIndexOperation (expr, expr_type))

and find_matching_overload env loc name arg_types =
  match Env.find_function ~local:false name arg_types env with
  | Some L.{ value = Type.Function (args, ret); _ } -> Ok (args, ret)
  | _ ->
      let candidates = Env.find_all_functions ~local:false name env in
      error loc (`NoMatchingFunction (name, candidates))

and check_call env loc f_expr arg_exprs =
  List.fold_results
    (fun acc arg_expr ->
      acc >>= fun arg_types ->
      check_single_value_expr env arg_expr >>= fun arg_type ->
      Ok (arg_type :: arg_types))
    (Ok []) arg_exprs
  >>= fun arg_types ->
  check_single_value_expr env f_expr >>= fun f_type ->
  match f_type with
  | Type.Function (args, ret) -> (
      let is_anonymous, name =
        match f_expr with
        | L.{ value = Ast.Id name; _ } -> (false, name)
        | _ -> (true, "")
      in
      let is_pipeline = (not is_anonymous) && Env.pipeline_exists name env in
      let is_function =
        is_anonymous || Env.function_exists name arg_types env
      in
      (* Replace args and ret with a matching overload if the function call
       * is not a pipeline call. *)
      ( if not is_pipeline then find_matching_overload env loc name arg_types
      else Ok (args, ret) )
      >>= fun (args, ret) ->
      let is_named = function
        | L.{ value = Ast.NamedArg _; _ } -> true
        | _ -> false
      in
      let all_named = List.for_all is_named arg_exprs in
      let all_unnamed = List.for_all (fun x -> not (is_named x)) arg_exprs in
      let want_types = List.map (fun (_, t) -> t) args in
      let name = Ast.string_of_expression f_expr in
      match (is_function, is_pipeline, all_named, all_unnamed) with
      (* Case #0: function + no arguments *)
      | true, false, true, true ->
          let () = assert (List.length arg_exprs = 0) in
          check_call_args name arg_exprs arg_types want_types >>= fun () ->
          Ok ret
      (* Case #1: function + unnamed arguments *)
      | true, false, false, true ->
          check_call_args name arg_exprs arg_types want_types >>= fun () ->
          Ok ret
      (* Case #2: function + named arguments *)
      | true, false, true, false ->
          check_call_named_args ~strict:true loc name args arg_exprs arg_types
          >>= fun () -> Ok ret
      (* Case #3: pipeline + unnamed arguments *)
      | false, true, false, true ->
          (* TODO: create custom error *)
          error loc
            (`Unimplemented
              "pipelines can be called only with named parameters")
      (* Case #4: pipeline + named arguments *)
      | false, true, true, _ ->
          check_pipeline_call env loc name args arg_exprs arg_types
          >>= fun () -> Ok ret
      | _, _, false, false ->
          let expr = L.{ loc; value = Ast.Call (f_expr, arg_exprs) } in
          error loc (`MixedArgumentStyle expr)
      | _ -> error loc (`Unimplemented "NOT IMPLEMENTED: weird call...") )
  | _ -> error loc (`InvalidCallOperation (f_expr, f_type))

and valid_arg arg_type want_type =
  let open Type in
  match (arg_type, want_type) with
  | Atom Singleton, _ -> true
  | RenderTarget RGBA, Sampler 2 -> true
  | _ -> arg_type = want_type

and check_call_args f_name arg_exprs arg_types want_types =
  List.fold_left
    (fun acc ((arg_expr, arg_type), want_type) ->
      acc >>= fun _ ->
      if not (valid_arg arg_type want_type) then
        let L.{ loc; _ } = arg_expr in
        error loc (`InvalidArgument (arg_expr, arg_type, want_type, f_name))
      else Ok ())
    (Ok ())
    (List.combine (List.combine arg_exprs arg_types) want_types)

and check_call_named_args ~strict loc f_name params arg_exprs arg_types =
  let want_types = List.map (fun (_, t) -> t) params in
  let args = List.combine arg_exprs arg_types in
  (* Check that there are no unexpected named arguments if strict mode is enabled *)
  ( if strict then
    List.fold_left
      (fun acc L.{ value; _ } ->
        acc >>= fun _ ->
        match value with
        | Ast.NamedArg (arg_name, _) ->
            if List.exists (fun (name, _) -> name = arg_name) params then Ok ()
            else error loc (`UnexpectedNamedArgument (arg_name, f_name))
        | _ -> failwith "expected all arguments to be named")
      (Ok ()) arg_exprs
  else Ok () )
  >>= fun () ->
  (* Check that all required named arguments are provided *)
  List.fold_left
    (fun acc (name, _) ->
      acc >>= fun (new_arg_exprs, new_arg_types) ->
      let arg =
        List.find_opt
          (fun (arg_expr, _) ->
            match arg_expr with
            | L.{ value = Ast.NamedArg (arg_name, _); _ } -> name = arg_name
            | _ -> failwith "expected all arguments to be named")
          args
      in
      match arg with
      | Some (arg_expr, arg_type) ->
          Ok (new_arg_exprs @ [ arg_expr ], new_arg_types @ [ arg_type ])
      | None -> error loc (`MissingNamedArgument (name, f_name)))
    (Ok ([], []))
    params
  >>= fun (new_arg_exprs, new_arg_types) ->
  check_call_args f_name new_arg_exprs new_arg_types want_types

and check_pipeline_call env loc f_name params arg_exprs arg_types =
  if Env.is_renderer_scope env then
    check_call_named_args ~strict:false loc f_name params arg_exprs arg_types
  else
    error loc
      (`Unimplemented "pipelines can be called only from renderer scope")

and check_id env loc id =
  match Env.find_rvalue id env with
  | Some L.{ value = typ; _ } -> Ok [ typ ]
  | None -> (
      match Env.find_name ~local:false id env with
      | Some _ -> error loc (`NotAnExpression id)
      | None -> error loc (`UndeclaredIdentifier id) )

and check_cast env loc t expr =
  let open Type in
  check_type env loc t >>= fun to_type ->
  check_single_value_expr env expr >>= fun from_type ->
  match (from_type, to_type) with
  | Primitive Int, Primitive Bool
  | Primitive Int, Primitive UInt
  | Primitive Int, Primitive Float
  | Primitive Int, Primitive Double
  | Primitive UInt, Primitive Bool
  | Primitive UInt, Primitive Int
  | Primitive UInt, Primitive Float
  | Primitive UInt, Primitive Double
  | Primitive Float, Primitive Int
  | Primitive Float, Primitive UInt
  | Primitive Float, Primitive Double ->
      Ok to_type
  | _ -> error loc (`InvalidCast (from_type, to_type))

and check_single_value_expr env expr =
  let L.{ loc; _ } = expr in
  check_expr env expr >>= function
  | [] -> error loc (`UnitUsedAsValue expr)
  | [ typ ] -> check_type env loc typ
  | _ -> error loc (`MultipleValueInSingleValueContext expr)

and check_expr env expr =
  let open Ast in
  let L.{ loc; value } = expr in
  match value with
  | Access (expr, id) -> check_access env loc expr id >>= fun typ -> Ok [ typ ]
  | Index (expr, index_exprs) ->
      check_index env loc expr index_exprs >>= fun typ -> Ok [ typ ]
  | Call (f_expr, arg_exprs) -> check_call env loc f_expr arg_exprs
  | Cast (t, expr) -> check_cast env loc t expr >>= fun typ -> Ok [ typ ]
  | NamedArg (_, expr) ->
      check_single_value_expr env expr >>= fun typ -> Ok [ typ ]
  | BinExpr (lhs, op, rhs) ->
      check_single_value_expr env lhs >>= fun ltyp ->
      check_single_value_expr env rhs >>= fun rtyp ->
      check_binop expr ltyp op rtyp >>= fun typ -> Ok [ typ ]
  | UnExpr (op, rhs) ->
      check_single_value_expr env rhs >>= fun typ ->
      check_unop loc op typ >>= fun typ -> Ok [ typ ]
  | BoolLiteral _ -> Ok [ Primitive Bool ]
  | IntLiteral _ -> Ok [ Primitive Int ]
  | FloatLiteral _ -> Ok [ Primitive Float ]
  | Id id -> check_id env loc id

and check_expr_list env have_exprs want_types less_errfn more_errfn errfn =
  let num_have = List.length have_exprs in
  let num_want = List.length want_types in
  if num_want > 1 && num_have = 1 then
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
          else errfn have_expr have_type want_type)
        (Ok ())
        (List.combine have_types want_types)
  else if num_have <> num_want then
    List.fold_left
      (fun acc have_expr ->
        acc >>= fun have_types ->
        check_single_value_expr env have_expr >>= fun have_type ->
        Ok (have_type :: have_types))
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
        else errfn have_expr have_type want_type)
      (Ok ())
      (List.combine have_exprs want_types)

(* Statements *)

let rec check_binding env loc ids exprs env_add_fn typed_ctor =
  let add_bindings_to_env types =
    List.fold_left
      (fun acc (id, t) ->
        acc >>= fun env ->
        match Env.find_name ~local:true id env with
        | Some L.{ loc = prev_loc; _ } ->
            error loc (`Redefinition (id, prev_loc))
        | None ->
            check_valid_identifier loc id >>= fun () ->
            (* Ignore variables named _ *)
            if id = "_" then Ok env
            else Ok (env_add_fn id L.{ loc; value = t } env))
      (Ok env) (List.combine ids types)
  in
  match exprs with
  | [ expr ] when List.length ids > 1 ->
      check_expr env expr >>= fun expr_types ->
      let num_vars, num_values = List.(length ids, length expr_types) in
      if num_values = 0 then error loc (`UnitUsedAsValue expr)
      else if num_vars <> num_values then
        error loc (`AssignmentMismatch (num_vars, num_values))
      else
        add_bindings_to_env expr_types >>= fun new_env ->
        let typed_stmt =
          typed_ctor
            TypedAst.{ bind_ids = ids; bind_values = [ (expr_types, expr) ] }
        in
        Ok (new_env, [ (env, L.{ loc; value = typed_stmt }) ])
  | _ ->
      List.fold_results
        (fun acc expr ->
          acc >>= fun expr_types ->
          check_single_value_expr env expr >>= fun expr_type ->
          Ok (expr_type :: expr_types))
        (Ok []) exprs
      >>= fun expr_types ->
      let num_vars, num_values = List.(length ids, length exprs) in
      if num_vars <> num_values then
        error loc (`AssignmentMismatch (num_vars, num_values))
      else
        add_bindings_to_env expr_types >>= fun new_env ->
        let typed_stmt =
          typed_ctor
            TypedAst.
              {
                bind_ids = ids;
                bind_values =
                  List.map
                    (fun (t, e) -> ([ t ], e))
                    (List.combine expr_types exprs);
              }
        in
        Ok (new_env, [ (env, L.{ loc; value = typed_stmt }) ])

and check_lvalue env expr =
  let open Ast in
  let L.{ loc; _ } = expr in
  match expr.value with
  | Id id -> (
      match Env.find_lvalue id env with
      | Some L.{ value = expr_type; _ } -> Ok expr_type
      | None ->
          if Env.name_exists id env then error loc (`NotAnLValue expr)
          else error loc (`UndeclaredIdentifier id) )
  | Access (lhs, id) -> check_access env loc lhs id
  | Index (lhs, rhs) -> check_index env loc lhs rhs
  | _ -> error loc (`NotAnLValue expr)

and check_lvalues env exprs =
  List.fold_results
    (fun acc expr ->
      acc >>= fun expr_types ->
      check_lvalue env expr >>= fun expr_type -> Ok (expr_type :: expr_types))
    (Ok []) exprs

and check_single_assignment loc op lhs_types rhs_exprs rhs_types =
  let () = assert (List.(length lhs_types = length rhs_types)) in
  List.fold_left
    (fun acc (lhs_type, (rhs_expr, rhs_type)) ->
      acc >>= fun () ->
      let err =
        error loc (`InvalidSingleAssignment (rhs_expr, rhs_type, lhs_type))
      in
      check_assignop lhs_type op rhs_type err)
    (Ok ())
    List.(combine lhs_types (combine rhs_exprs rhs_types))

and check_multiple_assignment loc op lhs_exprs lhs_types rhs_types =
  let () = assert (List.(length lhs_types = length rhs_types)) in
  List.fold_left
    (fun acc ((lhs_expr, lhs_type), rhs_type) ->
      acc >>= fun () ->
      let err =
        error loc (`InvalidMultipleAssignment (rhs_type, lhs_expr, lhs_type))
      in
      check_assignop lhs_type op rhs_type err)
    (Ok ())
    List.(combine (combine lhs_exprs lhs_types) rhs_types)

and check_assignment env loc op lhs rhs =
  let () = assert (List.length lhs > 0) in
  let () = assert (List.length rhs > 0) in
  match rhs with
  | [ expr ] when List.length lhs > 1 ->
      check_lvalues env lhs >>= fun lhs_types ->
      check_expr env expr >>= fun rhs_types ->
      let num_lhs, num_rhs = List.(length lhs_types, length rhs_types) in
      if num_rhs = 0 then error loc (`UnitUsedAsValue expr)
      else if num_lhs <> num_rhs then
        error loc (`AssignmentMismatch (num_lhs, num_rhs))
      else
        check_multiple_assignment loc op lhs lhs_types rhs_types >>= fun () ->
        let typed_stmt =
          TypedAst.Assignment
            {
              asg_op = op;
              asg_lvalues = List.(combine (map (fun t -> [ t ]) lhs_types) lhs);
              asg_rvalues = [ (rhs_types, expr) ];
            }
        in
        Ok (env, [ (env, L.{ loc; value = typed_stmt }) ])
  | _ ->
      check_lvalues env lhs >>= fun lhs_types ->
      List.fold_results
        (fun acc expr ->
          acc >>= fun expr_types ->
          check_single_value_expr env expr >>= fun expr_type ->
          Ok (expr_type :: expr_types))
        (Ok []) rhs
      >>= fun rhs_types ->
      let num_lhs, num_rhs = List.(length lhs_types, length rhs_types) in
      if num_lhs <> num_rhs then
        error loc (`AssignmentMismatch (num_lhs, num_rhs))
      else
        check_single_assignment loc op lhs_types rhs rhs_types >>= fun () ->
        let typed_stmt =
          TypedAst.Assignment
            {
              asg_op = op;
              asg_lvalues = List.(combine (map (fun t -> [ t ]) lhs_types) lhs);
              asg_rvalues = List.(combine (map (fun t -> [ t ]) rhs_types) rhs);
            }
        in
        Ok (env, [ (env, L.{ loc; value = typed_stmt }) ])

and check_if env loc if_cond if_true if_false =
  check_single_value_expr env if_cond >>= function
  | Type.Primitive Bool ->
      let true_env = Env.enter_block_scope "if_true" loc env in
      let false_env = Env.enter_block_scope "if_false" loc env in
      check_stmt_list true_env if_true >>= fun (_, typed_if_true) ->
      check_stmt_list false_env if_false >>= fun (_, typed_if_false) ->
      let typed_stmt =
        TypedAst.If
          {
            if_cond = ([ Type.Primitive Bool ], if_cond);
            if_true = typed_if_true;
            if_false = typed_if_false;
          }
      in
      Ok (env, [ (env, L.{ loc; value = typed_stmt }) ])
  | _ as t ->
      let L.{ loc; _ } = if_cond in
      error loc (`NonBoolIfCondition (if_cond, t))

and check_iterable_type env expr =
  let open Type in
  let L.{ loc; _ } = expr in
  check_single_value_expr env expr >>= fun expr_typ ->
  match expr_typ with
  | Array (t, [ _ ]) -> Ok (expr_typ, t)
  | Atom (List | Set) -> Ok (expr_typ, Atom Singleton)
  | _ as t -> error loc (`CannotRangeOver (expr, t))

and check_foriter env loc it_var it_expr body =
  check_valid_identifier loc it_var >>= fun () ->
  check_iterable_type env it_expr >>= fun (expr_typ, it_typ) ->
  let stmt_env =
    Env.(
      env
      |> enter_block_scope "foriter" loc
      |> add_var it_var L.{ loc; value = it_typ })
  in
  check_stmt_list stmt_env body >>= fun (_, typed_body) ->
  let typed_stmt =
    TypedAst.ForIter
      {
        foriter_id = it_var;
        foriter_it = ([ expr_typ ], it_expr);
        foriter_body = typed_body;
      }
  in
  Ok (env, [ (stmt_env, L.{ loc; value = typed_stmt }) ])

and check_range_expr env expr =
  let open Type in
  let L.{ loc; _ } = expr in
  check_single_value_expr env expr >>= fun expr_typ ->
  match expr_typ with
  | Primitive Int -> Ok ()
  | _ -> error loc (`NonIntegerRangeExpression (expr, expr_typ))

and check_forrange env loc it_var from_expr to_expr body =
  check_range_expr env from_expr >>= fun () ->
  check_range_expr env to_expr >>= fun () ->
  let int_typ = Type.Primitive Int in
  let stmt_env =
    Env.(
      env
      |> enter_block_scope "forrange" loc
      |> add_var it_var L.{ loc; value = int_typ })
  in
  check_stmt_list stmt_env body >>= fun (_, typed_body) ->
  let typed_stmt =
    TypedAst.ForRange
      {
        forrange_id = it_var;
        forrange_from = ([ int_typ ], from_expr);
        forrange_to = ([ int_typ ], to_expr);
        forrange_body = typed_body;
      }
  in
  Ok (env, [ (stmt_env, L.{ loc; value = typed_stmt }) ])

and check_return env loc exprs =
  match Env.match_function_scope env with
  | Some (Env.Function (_, Type.Function (_, ret_types))) ->
      let less_errfn have_types =
        error loc (`NotEnoughReturnArguments (have_types, ret_types))
      in
      let more_errfn have_types =
        error loc (`TooManyReturnArguments (have_types, ret_types))
      in
      let errfn have_expr have_type want_type =
        let L.{ loc; _ } = have_expr in
        error loc (`InvalidReturnArgument (have_expr, have_type, want_type))
      in
      check_expr_list env exprs ret_types less_errfn more_errfn errfn
      >>= fun () ->
      let stmt =
        TypedAst.Return
          (List.map (fun (t, e) -> ([ t ], e)) (List.combine ret_types exprs))
      in
      Ok (env, [ (env, L.{ loc; value = stmt }) ])
  | _ -> failwith "return can only appear in Function scopes"

and check_discard env loc =
  if Env.is_function_scope env then
    Ok (env, [ (env, L.{ loc; value = TypedAst.Discard }) ])
  else error loc (`Unimplemented "discard can only appear in Function scopes")

and check_stmt_list env stmts =
  List.fold_left
    (fun acc L.{ loc; value = stmt } ->
      acc >>= fun (env, typed_stmts) ->
      check_stmt env loc stmt >>= fun (env, typed_stmt) ->
      Ok (env, typed_stmts @ typed_stmt))
    (Ok (env, []))
    stmts

and check_stmt env loc =
  let open Ast in
  function
  | CallExpr (id, arg_exprs) ->
      check_call env loc L.{ value = Ast.Id id; loc } arg_exprs >>= fun _ ->
      List.fold_results
        (fun acc expr ->
          acc >>= fun arg_exprs ->
          check_expr env expr >>= fun expr_types ->
          Ok ((expr_types, expr) :: arg_exprs))
        (Ok []) arg_exprs
      >>= fun arg_exprs ->
      Ok (env, [ (env, L.{ loc; value = TypedAst.CallExpr (id, arg_exprs) }) ])
  | Var { bind_ids; bind_values } ->
      check_binding env loc bind_ids bind_values Env.add_var (fun b ->
          TypedAst.Var b)
  | Val { bind_ids; bind_values } ->
      check_binding env loc bind_ids bind_values Env.add_val (fun b ->
          TypedAst.Val b)
  | Assignment { asg_op; asg_lvalues; asg_rvalues } ->
      check_assignment env loc asg_op asg_lvalues asg_rvalues
  | If { if_cond; if_true; if_false } ->
      check_if env loc if_cond if_true if_false
  | ForIter { foriter_id; foriter_it; foriter_body } ->
      check_foriter env loc foriter_id foriter_it foriter_body
  | ForRange { forrange_id; forrange_from; forrange_to; forrange_body } ->
      check_forrange env loc forrange_id forrange_from forrange_to
        forrange_body
  | Return exprs -> check_return env loc exprs
  | Discard -> check_discard env loc

(* Top-Level Elements *)

let rec exists_nested_stmt stmt_match stmts =
  let open Ast in
  List.exists
    (fun L.{ value = stmt; _ } ->
      stmt_match stmt
      ||
      match stmt with
      | If { if_true; if_false; _ } ->
          exists_nested_stmt stmt_match if_true
          || exists_nested_stmt stmt_match if_false
      | ForIter { foriter_body; _ } ->
          exists_nested_stmt stmt_match foriter_body
      | ForRange { forrange_body; _ } ->
          exists_nested_stmt stmt_match forrange_body
      | other -> stmt_match other)
    stmts

let check_missing_return loc fd =
  let Ast.{ fd_name; fd_type; fd_body } = fd in
  match fd_type with
  | Type.Function (_, _ :: _) ->
      let stmt_match = function Ast.Return _ -> true | _ -> false in
      if exists_nested_stmt stmt_match fd_body then Ok ()
      else error loc (`MissingReturn fd_name)
  | _ -> Ok ()

let check_function_declaration env loc fd =
  let Ast.{ fd_name; fd_type; fd_body } = fd in
  check_valid_identifier loc fd_name >>= fun () ->
  check_type env loc fd_type >>= fun clean_type ->
  let env = Env.enter_function_scope fd_name clean_type env in
  let env =
    build_function_environment ~mutable_args:true env loc fd_name clean_type
  in
  check_stmt_list env fd_body >>= fun (env, typed_stmts) ->
  check_missing_return loc fd >>= fun () ->
  Ok
    TypedAst.
      { fd_env = env; fd_name; fd_type = clean_type; fd_body = typed_stmts }

let check_function_sig env loc fd =
  let Ast.{ fd_name; fd_type; _ } = fd in
  match Env.find_name ~local:true fd_name env with
  | Some L.{ loc = prev_loc; _ } ->
      error loc (`Redefinition (fd_name, prev_loc))
  | None ->
      check_type env loc fd_type >>= fun clean_type ->
      Ok (Env.add_function fd_name { loc; value = clean_type } env)

let check_function_group env functions =
  List.fold_left
    (fun acc L.{ loc; value } ->
      acc >>= fun env -> check_function_sig env loc value)
    (Ok env) functions
  >>= fun env ->
  List.fold_results
    (fun acc L.{ loc; value } ->
      acc >>= fun typed_functions ->
      check_function_declaration env loc value >>= fun typed_fd ->
      Ok (typed_fd :: typed_functions))
    (Ok []) functions

let check_pipeline_declaration_sig env loc pd =
  let Ast.{ pd_name; pd_type; _ } = pd in
  match Env.find_name ~local:true pd_name env with
  | Some L.{ loc = prev_loc; _ } ->
      error loc (`Redefinition (pd_name, prev_loc))
  | None ->
      check_valid_identifier loc pd_name >>= fun () ->
      check_type env loc pd_type >>= fun clean_type ->
      Ok (Env.add_pipeline pd_name { loc; value = clean_type } env)

let check_pipeline_declaration_body env loc pd =
  let Ast.{ pd_name; pd_type; pd_functions } = pd in
  check_type env loc pd_type >>= fun clean_type ->
  let env = Env.enter_pipeline_scope pd_name clean_type env in
  let env =
    build_function_environment ~mutable_args:false env loc "" clean_type
  in
  check_function_group env pd_functions >>= fun typed_functions ->
  Ok
    L.
      {
        loc;
        value =
          TypedAst.PipelineDecl
            {
              pd_env = env;
              pd_name;
              pd_type = clean_type;
              pd_functions = typed_functions;
            };
      }

let check_renderer_declaration_sig env loc rd =
  let Ast.{ rd_name; rd_type; _ } = rd in
  match Env.find_name ~local:true rd_name env with
  | Some L.{ loc = prev_loc; _ } ->
      error loc (`Redefinition (rd_name, prev_loc))
  | None ->
      check_valid_identifier loc rd_name >>= fun () ->
      check_type env loc rd_type >>= fun clean_type ->
      Ok (Env.add_renderer rd_name { loc; value = clean_type } env)

let check_renderer_declaration_body env loc rd =
  let Ast.{ rd_name; rd_type; rd_functions } = rd in
  check_type env loc rd_type >>= fun clean_type ->
  let env = Env.enter_renderer_scope rd_name clean_type env in
  let env =
    build_function_environment ~mutable_args:true env loc "" clean_type
  in
  check_function_group env rd_functions >>= fun typed_functions ->
  Ok
    L.
      {
        loc;
        value =
          TypedAst.RendererDecl
            {
              rd_env = env;
              rd_name;
              rd_type = clean_type;
              rd_functions = typed_functions;
            };
      }

let check Ast.{ module_name; elements } =
  let env = Env.(global |> enter_module_scope module_name) in
  (* First check all signatures and populate the module scope *)
  List.fold_left
    (fun acc L.{ loc; value } ->
      acc >>= fun (env, typed_root_elems) ->
      match value with
      | Ast.ConstDecl cd ->
          check_const_declaration env loc cd >>= fun (env, typed_cd) ->
          Ok (env, typed_root_elems @ [ typed_cd ])
      | Ast.TypeDecl td ->
          check_type_declaration env loc td >>= fun (env, typed_td) ->
          Ok (env, typed_root_elems @ [ typed_td ])
      | Ast.PipelineDecl pd ->
          check_pipeline_declaration_sig env loc pd >>= fun env ->
          Ok (env, typed_root_elems)
      | Ast.RendererDecl rd ->
          check_renderer_declaration_sig env loc rd >>= fun env ->
          Ok (env, typed_root_elems))
    (Ok (env, []))
    elements
  >>= fun (env, typed_root_elems) ->
  (* Then check the actual bodies of pipelines and renderers *)
  List.fold_left
    (fun acc L.{ loc; value } ->
      acc >>= fun typed_root_elems ->
      match value with
      | Ast.ConstDecl _ -> acc
      | Ast.TypeDecl _ -> acc
      | Ast.PipelineDecl pd ->
          check_pipeline_declaration_body env loc pd >>= fun typed_pd ->
          Ok (typed_root_elems @ [ typed_pd ])
      | Ast.RendererDecl rd ->
          check_renderer_declaration_body env loc rd >>= fun typed_rd ->
          Ok (typed_root_elems @ [ typed_rd ]))
    (Ok typed_root_elems) elements
  >>= fun typed_root_elems ->
  Ok
    TypedAst.
      {
        root_env = env;
        root_module = module_name;
        root_elems = typed_root_elems;
      }
