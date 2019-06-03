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
  | `UnitUsedAsValue of Ast.expression
  | `NotAnLValue of Ast.expression
  | `InvalidSingleAssignment of Ast.expression * Type.t * Type.t
  | `InvalidMultipleAssignment of Type.t * Ast.expression * Type.t
  | `NonBoolIfCondition of Ast.expression * Type.t
  | `CannotRangeOver of Ast.expression * Type.t
  | `NonIntegerRangeExpression of Ast.expression * Type.t ]

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
  | Type.RenderTarget _ ->
      Ok t
  | Type.Primitive _ ->
      Ok t

let build_function_environment env loc name typ =
  match typ with
  | Type.Function (args, _) ->
      let env =
        List.fold_left
          (fun env (name, t) -> Env.add_var name Located.{loc; value= t} env)
          env args
      in
      Env.add_builtin name env
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
  let () = assert (is_ref typ) in
  match (op, typ) with
  (* Unary Plus and Minus *)
  | (UPlus | UMinus), TypeRef "int"
  | (UPlus | UMinus), TypeRef "uint"
  | (UPlus | UMinus), TypeRef "float"
  | (UPlus | UMinus), TypeRef "double"
  | (UPlus | UMinus), TypeRef "ivec2"
  | (UPlus | UMinus), TypeRef "ivec3"
  | (UPlus | UMinus), TypeRef "ivec4"
  | (UPlus | UMinus), TypeRef "uvec2"
  | (UPlus | UMinus), TypeRef "uvec3"
  | (UPlus | UMinus), TypeRef "uvec4"
  | (UPlus | UMinus), TypeRef "fvec2"
  | (UPlus | UMinus), TypeRef "fvec3"
  | (UPlus | UMinus), TypeRef "fvec4"
  | (UPlus | UMinus), TypeRef "dvec2"
  | (UPlus | UMinus), TypeRef "dvec3"
  | (UPlus | UMinus), TypeRef "dvec4"
  (* Logical NOT *)
  | LogicalNot, TypeRef "bool"
  (* Bitwise Complement *)
  | BitwiseComplement, TypeRef "int"
  | BitwiseComplement, TypeRef "uint" ->
      Ok typ
  | _, _ ->
      error loc (`InvalidUnaryOperation (op, typ))

let check_binop expr ltyp op rtyp =
  let open Ast in
  let open Type in
  let () = assert (is_ref ltyp) in
  let () = assert (is_ref rtyp) in
  let Located.{loc; _} = expr in
  match (ltyp, op, rtyp) with
  (* Logical *)
  | TypeRef "bool", (LogicalOr | LogicalXor | LogicalAnd), TypeRef "bool" ->
      Ok (TypeRef "bool")
  (* Bitwise *)
  | ( TypeRef "int"
    , (BitwiseOr | BitwiseXor | BitwiseAnd | ShiftLeft | ShiftRight)
    , TypeRef "int" ) ->
      Ok (TypeRef "int")
  (* Comparison *)
  | ( TypeRef "bool"
    , (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual)
    , TypeRef "bool" )
  | ( TypeRef "int"
    , (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual)
    , TypeRef "int" )
  | ( TypeRef "uint"
    , (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual)
    , TypeRef "uint" )
  | ( TypeRef "float"
    , (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual)
    , TypeRef "float" )
  | ( TypeRef "double"
    , (Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual)
    , TypeRef "double" ) ->
      Ok (TypeRef "bool")
  (* Arithmetic *)
  | TypeRef "int", (Plus | Minus | Mult | Div | Mod), TypeRef "int" ->
      Ok (TypeRef "int")
  | TypeRef "uint", (Plus | Minus | Mult | Div | Mod), TypeRef "uint" ->
      Ok (TypeRef "uint")
  | TypeRef "float", (Plus | Minus | Mult | Div), TypeRef "float" ->
      Ok (TypeRef "float")
  | TypeRef "double", (Plus | Minus | Mult | Div), TypeRef "double" ->
      Ok (TypeRef "double")
  | TypeRef "ivec2", (Plus | Minus), TypeRef "ivec2" ->
      Ok (TypeRef "ivec2")
  | TypeRef "ivec3", (Plus | Minus), TypeRef "ivec3" ->
      Ok (TypeRef "ivec3")
  | TypeRef "ivec4", (Plus | Minus), TypeRef "ivec4" ->
      Ok (TypeRef "ivec4")
  | TypeRef "uvec2", (Plus | Minus), TypeRef "uvec2" ->
      Ok (TypeRef "uvec2")
  | TypeRef "uvec3", (Plus | Minus), TypeRef "uvec3" ->
      Ok (TypeRef "uvec3")
  | TypeRef "uvec4", (Plus | Minus), TypeRef "uvec4" ->
      Ok (TypeRef "uvec4")
  | TypeRef "fvec2", (Plus | Minus), TypeRef "fvec2" ->
      Ok (TypeRef "fvec2")
  | TypeRef "fvec3", (Plus | Minus), TypeRef "fvec3" ->
      Ok (TypeRef "fvec3")
  | TypeRef "fvec4", (Plus | Minus), TypeRef "fvec4" ->
      Ok (TypeRef "fvec4")
  | TypeRef "dvec2", (Plus | Minus), TypeRef "dvec2" ->
      Ok (TypeRef "dvec2")
  | TypeRef "dvec3", (Plus | Minus), TypeRef "dvec3" ->
      Ok (TypeRef "dvec3")
  | TypeRef "dvec4", (Plus | Minus), TypeRef "dvec4" ->
      Ok (TypeRef "dvec4")
  (* Matrix Multiplication *)
  | TypeRef "fmat2x2", Mult, TypeRef "fvec2" ->
      Ok (TypeRef "fvec2")
  | TypeRef "fmat2", Mult, TypeRef "fvec2" ->
      Ok (TypeRef "fvec2")
  | TypeRef "fmat2x2", Mult, TypeRef "fmat2x2" ->
      Ok (TypeRef "fmat2x2")
  | TypeRef "fmat2x2", Mult, TypeRef "fmat2" ->
      Ok (TypeRef "fmat2x2")
  | TypeRef "fmat2", Mult, TypeRef "fmat2x2" ->
      Ok (TypeRef "fmat2x2")
  | TypeRef "fmat2", Mult, TypeRef "fmat2" ->
      Ok (TypeRef "fmat2x2")
  | TypeRef "fmat2x2", Mult, TypeRef "fmat2x3" ->
      Ok (TypeRef "fmat2x3")
  | TypeRef "fmat2", Mult, TypeRef "fmat2x3" ->
      Ok (TypeRef "fmat2x3")
  | TypeRef "fmat2x2", Mult, TypeRef "fmat2x4" ->
      Ok (TypeRef "fmat2x4")
  | TypeRef "fmat2", Mult, TypeRef "fmat2x4" ->
      Ok (TypeRef "fmat2x4")
  | TypeRef "fmat2x3", Mult, TypeRef "fvec3" ->
      Ok (TypeRef "fvec2")
  | TypeRef "fmat2x3", Mult, TypeRef "fmat3x2" ->
      Ok (TypeRef "fmat2x2")
  | TypeRef "fmat2x3", Mult, TypeRef "fmat3x3" ->
      Ok (TypeRef "fmat2x3")
  | TypeRef "fmat2x3", Mult, TypeRef "fmat3" ->
      Ok (TypeRef "fmat2x3")
  | TypeRef "fmat2x3", Mult, TypeRef "fmat3x4" ->
      Ok (TypeRef "fmat2x4")
  | TypeRef "fmat2x4", Mult, TypeRef "fvec4" ->
      Ok (TypeRef "fvec2")
  | TypeRef "fmat2x4", Mult, TypeRef "fmat4x2" ->
      Ok (TypeRef "fmat2x2")
  | TypeRef "fmat2x4", Mult, TypeRef "fmat4x3" ->
      Ok (TypeRef "fmat2x3")
  | TypeRef "fmat2x4", Mult, TypeRef "fmat4x4" ->
      Ok (TypeRef "fmat2x4")
  | TypeRef "fmat2x4", Mult, TypeRef "fmat4" ->
      Ok (TypeRef "fmat2x4")
  | TypeRef "fmat3x2", Mult, TypeRef "fvec2" ->
      Ok (TypeRef "fvec3")
  | TypeRef "fmat3x2", Mult, TypeRef "fmat2x2" ->
      Ok (TypeRef "fmat3x2")
  | TypeRef "fmat3x2", Mult, TypeRef "fmat2" ->
      Ok (TypeRef "fmat3x2")
  | TypeRef "fmat3x2", Mult, TypeRef "fmat2x3" ->
      Ok (TypeRef "fmat3x3")
  | TypeRef "fmat3x2", Mult, TypeRef "fmat2x4" ->
      Ok (TypeRef "fmat3x4")
  | TypeRef "fmat3x3", Mult, TypeRef "fvec3" ->
      Ok (TypeRef "fvec3")
  | TypeRef "fmat3", Mult, TypeRef "fvec3" ->
      Ok (TypeRef "fvec3")
  | TypeRef "fmat3x3", Mult, TypeRef "fmat3x2" ->
      Ok (TypeRef "fmat3x2")
  | TypeRef "fmat3", Mult, TypeRef "fmat3x2" ->
      Ok (TypeRef "fmat3x2")
  | TypeRef "fmat3x3", Mult, TypeRef "fmat3x3" ->
      Ok (TypeRef "fmat3x3")
  | TypeRef "fmat3x3", Mult, TypeRef "fmat3" ->
      Ok (TypeRef "fmat3x3")
  | TypeRef "fmat3", Mult, TypeRef "fmat3x3" ->
      Ok (TypeRef "fmat3x3")
  | TypeRef "fmat3", Mult, TypeRef "fmat3" ->
      Ok (TypeRef "fmat3x3")
  | TypeRef "fmat3x3", Mult, TypeRef "fmat3x4" ->
      Ok (TypeRef "fmat3x4")
  | TypeRef "fmat3", Mult, TypeRef "fmat3x4" ->
      Ok (TypeRef "fmat3x4")
  | TypeRef "fmat3x4", Mult, TypeRef "fvec4" ->
      Ok (TypeRef "fvec3")
  | TypeRef "fmat3x4", Mult, TypeRef "fmat4x2" ->
      Ok (TypeRef "fmat3x2")
  | TypeRef "fmat3x4", Mult, TypeRef "fmat4x3" ->
      Ok (TypeRef "fmat3x3")
  | TypeRef "fmat3x4", Mult, TypeRef "fmat4x4" ->
      Ok (TypeRef "fmat3x4")
  | TypeRef "fmat3x4", Mult, TypeRef "fmat4" ->
      Ok (TypeRef "fmat3x4")
  | TypeRef "fmat4x2", Mult, TypeRef "fvec2" ->
      Ok (TypeRef "fvec4")
  | TypeRef "fmat4x2", Mult, TypeRef "fmat2x2" ->
      Ok (TypeRef "fmat4x2")
  | TypeRef "fmat4x2", Mult, TypeRef "fmat2" ->
      Ok (TypeRef "fmat4x2")
  | TypeRef "fmat4x2", Mult, TypeRef "fmat2x3" ->
      Ok (TypeRef "fmat4x3")
  | TypeRef "fmat4x2", Mult, TypeRef "fmat2x4" ->
      Ok (TypeRef "fmat4x4")
  | TypeRef "fmat4x3", Mult, TypeRef "fvec3" ->
      Ok (TypeRef "fvec4")
  | TypeRef "fmat4x3", Mult, TypeRef "fmat3x2" ->
      Ok (TypeRef "fmat4x2")
  | TypeRef "fmat4x3", Mult, TypeRef "fmat3x3" ->
      Ok (TypeRef "fmat4x3")
  | TypeRef "fmat4x3", Mult, TypeRef "fmat3" ->
      Ok (TypeRef "fmat4x3")
  | TypeRef "fmat4x3", Mult, TypeRef "fmat3x4" ->
      Ok (TypeRef "fmat4x4")
  | TypeRef "fmat4x4", Mult, TypeRef "fvec4" ->
      Ok (TypeRef "fvec4")
  | TypeRef "fmat4", Mult, TypeRef "fvec4" ->
      Ok (TypeRef "fvec4")
  | TypeRef "fmat4x4", Mult, TypeRef "fmat4x2" ->
      Ok (TypeRef "fmat4x2")
  | TypeRef "fmat4", Mult, TypeRef "fmat4x2" ->
      Ok (TypeRef "fmat4x2")
  | TypeRef "fmat4x4", Mult, TypeRef "fmat4x3" ->
      Ok (TypeRef "fmat4x3")
  | TypeRef "fmat4", Mult, TypeRef "fmat4x3" ->
      Ok (TypeRef "fmat4x3")
  | TypeRef "fmat4x4", Mult, TypeRef "fmat4x4" ->
      Ok (TypeRef "fmat4x4")
  | TypeRef "fmat4x4", Mult, TypeRef "fmat4" ->
      Ok (TypeRef "fmat4x4")
  | TypeRef "fmat4", Mult, TypeRef "fmat4x4" ->
      Ok (TypeRef "fmat4x4")
  | TypeRef "fmat4", Mult, TypeRef "fmat4" ->
      Ok (TypeRef "fmat4x4")
  | _ ->
      error loc (`InvalidBinaryOperation (expr, ltyp, rtyp))

let check_assignop ltyp op rtyp err =
  let open Ast in
  let open Type in
  let () = assert (is_ref ltyp) in
  let () = assert (is_ref rtyp) in
  match (ltyp, op, rtyp) with
  | TypeRef "bool", Assign, TypeRef "bool" ->
      Ok ()
  | TypeRef "int", _, TypeRef "int" ->
      Ok ()
  | TypeRef "uint", _, TypeRef "uint" ->
      Ok ()
  | ( TypeRef "float"
    , (Assign | AssignPlus | AssignMinus | AssignMult | AssignDiv)
    , TypeRef "float" ) ->
      Ok ()
  | ( TypeRef "double"
    , (Assign | AssignPlus | AssignMinus | AssignMult | AssignDiv)
    , TypeRef "double" ) ->
      Ok ()
  | TypeRef "ivec2", (Assign | AssignPlus | AssignMinus), TypeRef "ivec2" ->
      Ok ()
  | TypeRef "ivec3", (Assign | AssignPlus | AssignMinus), TypeRef "ivec3" ->
      Ok ()
  | TypeRef "ivec4", (Assign | AssignPlus | AssignMinus), TypeRef "ivec4" ->
      Ok ()
  | TypeRef "uvec2", (Assign | AssignPlus | AssignMinus), TypeRef "uvec2" ->
      Ok ()
  | TypeRef "uvec3", (Assign | AssignPlus | AssignMinus), TypeRef "uvec3" ->
      Ok ()
  | TypeRef "uvec4", (Assign | AssignPlus | AssignMinus), TypeRef "uvec4" ->
      Ok ()
  | TypeRef "fvec2", (Assign | AssignPlus | AssignMinus), TypeRef "fvec2" ->
      Ok ()
  | TypeRef "fvec3", (Assign | AssignPlus | AssignMinus), TypeRef "fvec3" ->
      Ok ()
  | TypeRef "fvec4", (Assign | AssignPlus | AssignMinus), TypeRef "fvec4" ->
      Ok ()
  | TypeRef "dvec2", (Assign | AssignPlus | AssignMinus), TypeRef "dvec2" ->
      Ok ()
  | TypeRef "dvec3", (Assign | AssignPlus | AssignMinus), TypeRef "dvec3" ->
      Ok ()
  | TypeRef "dvec4", (Assign | AssignPlus | AssignMinus), TypeRef "dvec4" ->
      Ok ()
  | ( TypeRef "fmat2"
    , (Assign | AssignPlus | AssignMinus | AssignMult)
    , TypeRef "fmat2" ) ->
      Ok ()
  | ( TypeRef "fmat3"
    , (Assign | AssignPlus | AssignMinus | AssignMult)
    , TypeRef "fmat3" ) ->
      Ok ()
  | ( TypeRef "fmat4"
    , (Assign | AssignPlus | AssignMinus | AssignMult)
    , TypeRef "fmat4" ) ->
      Ok ()
  | TypeRef "fmat2x2", (Assign | AssignPlus | AssignMinus), TypeRef "fmat2x2"
    ->
      Ok ()
  | TypeRef "fmat2x3", (Assign | AssignPlus | AssignMinus), TypeRef "fmat2x3"
    ->
      Ok ()
  | TypeRef "fmat2x4", (Assign | AssignPlus | AssignMinus), TypeRef "fmat2x4"
    ->
      Ok ()
  | TypeRef "fmat3x2", (Assign | AssignPlus | AssignMinus), TypeRef "fmat3x2"
    ->
      Ok ()
  | TypeRef "fmat3x3", (Assign | AssignPlus | AssignMinus), TypeRef "fmat3x3"
    ->
      Ok ()
  | TypeRef "fmat3x4", (Assign | AssignPlus | AssignMinus), TypeRef "fmat3x4"
    ->
      Ok ()
  | TypeRef "fmat4x2", (Assign | AssignPlus | AssignMinus), TypeRef "fmat4x2"
    ->
      Ok ()
  | TypeRef "fmat4x3", (Assign | AssignPlus | AssignMinus), TypeRef "fmat4x3"
    ->
      Ok ()
  | TypeRef "fmat4x4", (Assign | AssignPlus | AssignMinus), TypeRef "fmat4x4"
    ->
      Ok ()
  | ( TypeRef "dmat2"
    , (Assign | AssignPlus | AssignMinus | AssignMult)
    , TypeRef "dmat2" ) ->
      Ok ()
  | ( TypeRef "dmat3"
    , (Assign | AssignPlus | AssignMinus | AssignMult)
    , TypeRef "dmat3" ) ->
      Ok ()
  | ( TypeRef "dmat4"
    , (Assign | AssignPlus | AssignMinus | AssignMult)
    , TypeRef "dmat4" ) ->
      Ok ()
  | TypeRef "dmat2x2", (Assign | AssignPlus | AssignMinus), TypeRef "dmat2x2"
    ->
      Ok ()
  | TypeRef "dmat2x3", (Assign | AssignPlus | AssignMinus), TypeRef "dmat2x3"
    ->
      Ok ()
  | TypeRef "dmat2x4", (Assign | AssignPlus | AssignMinus), TypeRef "dmat2x4"
    ->
      Ok ()
  | TypeRef "dmat3x2", (Assign | AssignPlus | AssignMinus), TypeRef "dmat3x2"
    ->
      Ok ()
  | TypeRef "dmat3x3", (Assign | AssignPlus | AssignMinus), TypeRef "dmat3x3"
    ->
      Ok ()
  | TypeRef "dmat3x4", (Assign | AssignPlus | AssignMinus), TypeRef "dmat3x4"
    ->
      Ok ()
  | TypeRef "dmat4x2", (Assign | AssignPlus | AssignMinus), TypeRef "dmat4x2"
    ->
      Ok ()
  | TypeRef "dmat4x3", (Assign | AssignPlus | AssignMinus), TypeRef "dmat4x3"
    ->
      Ok ()
  | TypeRef "dmat4x4", (Assign | AssignPlus | AssignMinus), TypeRef "dmat4x4"
    ->
      Ok ()
  | ( (TypeRef "rt_rgb" | RenderTarget RGB)
    , (Assign | AssignPlus)
    , TypeRef "fvec3" ) ->
      Ok ()
  | ( (TypeRef "rt_rgba" | RenderTarget RGBA)
    , (Assign | AssignPlus)
    , TypeRef "fvec4" ) ->
      Ok ()
  | (TypeRef "rt_ds" | RenderTarget DS), (Assign | AssignPlus), TypeRef "float"
    ->
      Ok ()
  | _ ->
      err

let rec check_access env loc expr id =
  let check_field_exists id fields err =
    match List.find_opt (fun (name, _) -> name = id) fields with
    | Some (_, t) ->
        Ok t
    | None ->
        err
  in
  check_single_value_expr env expr >>= fun typ ->
  let err = error loc (`NoSuchMember (typ, id)) in
  match typ with
  | Type.Record fields ->
      check_field_exists id fields err
  | Type.TypeRef name -> (
    match Env.find_type ~local:false name env with
    | Some Located.{value= Type.Record fields; _} ->
        check_field_exists id fields err
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
            | TypeRef "int" | TypeRef "uint" ->
                acc
            | _ ->
                let Located.{loc; _} = index_expr in
                error loc (`NonIntegerArrayIndex index_expr) )
          (Ok ())
          (List.combine index_exprs index_types)
        >>= fun () -> Ok t
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
      let is_anonymous, name =
        match f_expr with
        | Located.{value= Ast.Id name; _} ->
            (false, name)
        | _ ->
            (true, "")
      in
      let is_pipeline = (not is_anonymous) && Env.pipeline_exists name env in
      let is_function = is_anonymous || Env.function_exists name env in
      let is_named = function
        | Located.{value= Ast.NamedArg _; _} ->
            true
        | _ ->
            false
      in
      let all_named = List.for_all is_named arg_exprs in
      let all_unnamed = List.for_all (fun x -> not (is_named x)) arg_exprs in
      let have = List.length arg_types in
      let want = List.length args in
      let want_types = List.map (fun (_, t) -> t) args in
      if all_unnamed && have < want then
        error loc (`NotEnoughArguments (f_expr, arg_types, want_types))
      else if all_unnamed && have > want then
        error loc (`TooManyArguments (f_expr, arg_types, want_types))
      else
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
            check_call_named_args loc name args arg_exprs arg_types
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
            let expr = Located.{loc; value= Ast.Call (f_expr, arg_exprs)} in
            error loc (`MixedArgumentStyle expr)
        | _ ->
            error loc (`Unimplemented "not implemented: weird call") )
  | _ ->
      error loc (`InvalidCallOperation (f_expr, f_type))

and valid_arg arg_type want_type =
  let open Type in
  match (arg_type, want_type) with
  | TypeRef "atom", _ ->
      true
  | TypeRef "rt_ds", TypeRef "depthBuffer" ->
      true
  | TypeRef "rt_rgb", TypeRef "texture2D" ->
      true
  | TypeRef "rt_rgba", TypeRef "texture2D" ->
      true
  | _ ->
      arg_type = want_type

and check_call_args f_name arg_exprs arg_types want_types =
  List.fold_left
    (fun acc ((arg_expr, arg_type), want_type) ->
      acc >>= fun _ ->
      if not (valid_arg arg_type want_type) then
        let Located.{loc; _} = arg_expr in
        error loc (`InvalidArgument (arg_expr, arg_type, want_type, f_name))
      else Ok () )
    (Ok ())
    (List.combine (List.combine arg_exprs arg_types) want_types)

and check_call_named_args loc f_name params arg_exprs arg_types =
  let want_types = List.map (fun (_, t) -> t) params in
  let args = List.combine arg_exprs arg_types in
  (* Check that there are no unexpected named arguments *)
  List.fold_left
    (fun acc Located.{value; _} ->
      acc >>= fun _ ->
      match value with
      | Ast.NamedArg (arg_name, _) ->
          if List.exists (fun (name, _) -> name = arg_name) params then Ok ()
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

and check_pipeline_call env loc f_name params arg_exprs arg_types =
  if Env.is_renderer_scope env then
    (* TODO: modify params to include parameters to first stage *)
    check_call_named_args loc f_name params arg_exprs arg_types
  else
    error loc
      (`Unimplemented "pipelines can be called only from renderer scope")

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
      check_access env loc expr id >>= fun typ -> Ok [typ]
  | Index (expr, index_exprs) ->
      check_index env loc expr index_exprs >>= fun typ -> Ok [typ]
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

let rec check_var_declaration env loc ids exprs =
  let declare_local_vars types =
    List.fold_left
      (fun acc (id, t) ->
        acc >>= fun env ->
        match Env.find_name ~local:true id env with
        | Some Located.{loc= prev_loc; _} ->
            error loc (`Redefinition (id, prev_loc))
        | None ->
            (* Ignore variables named _ *)
            if id = "_" then Ok env
            else Ok Env.(env |> add_var id Located.{loc; value= t}) )
      (Ok env) (List.combine ids types)
  in
  match exprs with
  | [expr] when List.length ids > 1 ->
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

and check_lvalue env expr =
  let open Ast in
  let Located.{loc; _} = expr in
  match expr.value with
  | Id id -> (
    match Env.find_lvalue id env with
    | Some Located.{value= expr_type; _} ->
        Ok expr_type
    | None ->
        if Env.name_exists id env then error loc (`NotAnLValue expr)
        else error loc (`UndeclaredIdentifier id) )
  | Access (lhs, id) ->
      check_access env loc lhs id
  | Index (lhs, rhs) ->
      check_index env loc lhs rhs
  | _ ->
      error loc (`NotAnLValue expr)

and check_lvalues env exprs =
  List.fold_results
    (fun acc expr ->
      acc >>= fun expr_types ->
      check_lvalue env expr >>= fun expr_type -> Ok (expr_type :: expr_types)
      )
    (Ok []) exprs

and check_single_assignment loc op lhs_types rhs_exprs rhs_types =
  let () = assert (List.(length lhs_types = length rhs_types)) in
  List.fold_left
    (fun acc (lhs_type, (rhs_expr, rhs_type)) ->
      acc >>= fun () ->
      let err =
        error loc (`InvalidSingleAssignment (rhs_expr, rhs_type, lhs_type))
      in
      check_assignop lhs_type op rhs_type err )
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
      check_assignop lhs_type op rhs_type err )
    (Ok ())
    List.(combine (combine lhs_exprs lhs_types) rhs_types)

and check_assignment env loc op lhs rhs =
  let () = assert (List.length lhs > 0) in
  let () = assert (List.length rhs > 0) in
  match rhs with
  | [expr] when List.length lhs > 1 ->
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
            { asg_op= op
            ; asg_lvalues= List.(combine (map (fun t -> [t]) lhs_types) lhs)
            ; asg_rvalues= [(rhs_types, expr)] }
        in
        Ok (env, [(env, Located.{loc; value= typed_stmt})])
  | _ ->
      check_lvalues env lhs >>= fun lhs_types ->
      List.fold_results
        (fun acc expr ->
          acc >>= fun expr_types ->
          check_single_value_expr env expr >>= fun expr_type ->
          Ok (expr_type :: expr_types) )
        (Ok []) rhs
      >>= fun rhs_types ->
      let num_lhs, num_rhs = List.(length lhs_types, length rhs_types) in
      if num_lhs <> num_rhs then
        error loc (`AssignmentMismatch (num_lhs, num_rhs))
      else
        check_single_assignment loc op lhs_types rhs rhs_types >>= fun () ->
        let typed_stmt =
          TypedAst.Assignment
            { asg_op= op
            ; asg_lvalues= List.(combine (map (fun t -> [t]) lhs_types) lhs)
            ; asg_rvalues= List.(combine (map (fun t -> [t]) rhs_types) rhs) }
        in
        Ok (env, [(env, Located.{loc; value= typed_stmt})])

and check_if env loc if_cond if_true if_false =
  check_single_value_expr env if_cond >>= function
  | Type.TypeRef "bool" ->
      let true_env = Env.enter_block_scope "if_true" loc env in
      let false_env = Env.enter_block_scope "if_false" loc env in
      check_stmt_list true_env if_true >>= fun (_, typed_if_true) ->
      check_stmt_list false_env if_false >>= fun (_, typed_if_false) ->
      let typed_stmt =
        TypedAst.If
          { if_cond= ([Type.TypeRef "bool"], if_cond)
          ; if_true= typed_if_true
          ; if_false= typed_if_false }
      in
      Ok (env, [(env, Located.{loc; value= typed_stmt})])
  | _ as t ->
      let Located.{loc; _} = if_cond in
      error loc (`NonBoolIfCondition (if_cond, t))

and check_iterable_type env expr =
  let open Type in
  let Located.{loc; _} = expr in
  check_single_value_expr env expr >>= fun expr_typ ->
  match expr_typ with
  | Array (t, [_]) ->
      Ok (expr_typ, t)
  | TypeRef "atomset" ->
      Ok (expr_typ, TypeRef "atom")
  | TypeRef "atomlist" ->
      Ok (expr_typ, TypeRef "atom")
  | _ as t ->
      error loc (`CannotRangeOver (expr, t))

and check_foriter env loc it_var it_expr body =
  check_iterable_type env it_expr >>= fun (expr_typ, it_typ) ->
  let stmt_env =
    Env.(
      env
      |> enter_block_scope "foriter" loc
      |> add_var it_var Located.{loc; value= it_typ})
  in
  check_stmt_list stmt_env body >>= fun (_, typed_body) ->
  let typed_stmt =
    TypedAst.ForIter
      { foriter_id= it_var
      ; foriter_it= ([expr_typ], it_expr)
      ; foriter_body= typed_body }
  in
  Ok (env, [(stmt_env, Located.{loc; value= typed_stmt})])

and check_range_expr env expr =
  let open Type in
  let Located.{loc; _} = expr in
  check_single_value_expr env expr >>= fun expr_typ ->
  match expr_typ with
  | TypeRef "int" ->
      Ok ()
  | _ ->
      error loc (`NonIntegerRangeExpression (expr, expr_typ))

and check_forrange env loc it_var from_expr to_expr body =
  check_range_expr env from_expr >>= fun () ->
  check_range_expr env to_expr >>= fun () ->
  let int_typ = Type.TypeRef "int" in
  let stmt_env =
    Env.(
      env
      |> enter_block_scope "forrange" loc
      |> add_var it_var Located.{loc; value= int_typ})
  in
  check_stmt_list stmt_env body >>= fun (_, typed_body) ->
  let typed_stmt =
    TypedAst.ForRange
      { forrange_id= it_var
      ; forrange_from= ([int_typ], from_expr)
      ; forrange_to= ([int_typ], to_expr)
      ; forrange_body= typed_body }
  in
  Ok (env, [(stmt_env, Located.{loc; value= typed_stmt})])

and check_return env loc exprs =
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

and check_discard env loc =
  if Env.is_function_scope env then
    Ok (env, [(env, Located.{loc; value= TypedAst.Discard})])
  else error loc (`Unimplemented "discard can only appear in Function scopes")

and check_stmt_list env stmts =
  List.fold_left
    (fun acc Located.{loc; value= stmt} ->
      acc >>= fun (env, typed_stmts) ->
      check_stmt env loc stmt >>= fun (env, typed_stmt) ->
      Ok (env, typed_stmts @ typed_stmt) )
    (Ok (env, []))
    stmts

and check_stmt env loc =
  let open Ast in
  function
  | Var {var_ids; var_values} ->
      check_var_declaration env loc var_ids var_values
  | Assignment {asg_op; asg_lvalues; asg_rvalues} ->
      check_assignment env loc asg_op asg_lvalues asg_rvalues
  | If {if_cond; if_true; if_false} ->
      check_if env loc if_cond if_true if_false
  | ForIter {foriter_id; foriter_it; foriter_body} ->
      check_foriter env loc foriter_id foriter_it foriter_body
  | ForRange {forrange_id; forrange_from; forrange_to; forrange_body} ->
      check_forrange env loc forrange_id forrange_from forrange_to
        forrange_body
  | Return exprs ->
      check_return env loc exprs
  | Discard ->
      check_discard env loc

(* Top-Level Elements *)

let check_function_declaration env loc fd =
  let Ast.{fd_name; fd_type; fd_body} = fd in
  check_type env loc fd_type >>= fun clean_type ->
  let env = Env.enter_function_scope fd_name clean_type env in
  let env = build_function_environment env loc fd_name clean_type in
  check_stmt_list env fd_body >>= fun (env, typed_stmts) ->
  Ok TypedAst.{fd_env= env; fd_name; fd_type= clean_type; fd_body= typed_stmts}

let check_function_sig env loc fd =
  let Ast.{fd_name; fd_type; _} = fd in
  match Env.find_name ~local:true fd_name env with
  | Some Located.{loc= prev_loc; _} ->
      error loc (`Redefinition (fd_name, prev_loc))
  | None ->
      check_type env loc fd_type >>= fun clean_type ->
      Ok (Env.add_function fd_name {loc; value= clean_type} env)

let check_function_group env functions =
  List.fold_left
    (fun acc Located.{loc; value} ->
      acc >>= fun env -> check_function_sig env loc value )
    (Ok env) functions
  >>= fun env ->
  List.fold_results
    (fun acc Located.{loc; value} ->
      acc >>= fun typed_functions ->
      check_function_declaration env loc value >>= fun typed_fd ->
      Ok (typed_fd :: typed_functions) )
    (Ok []) functions

let expand_pipeline_type env loc t functions =
  (* TODO: add strict check for pipeline stage invariants *)
  match t with
  | Type.Function (p_params, p_ret) -> (
      List.fold_left
        (fun acc Located.{loc; value} ->
          acc >>= fun env -> check_function_sig env loc value )
        (Ok env) functions
      >>= fun env ->
      let vertex = Env.find_function ~local:true "vertex" env in
      let compute = Env.find_function ~local:true "compute" env in
      match (vertex, compute) with
      | Some _, Some _ ->
          error loc
            (`Unimplemented
              "ambiguous pipeline entry stage: cannot have both vertex and \
               compute functions")
      | Some Located.{value= Type.Function (f_params, _); _}, None ->
          Ok (Type.Function (p_params @ f_params, p_ret))
      | None, Some _ ->
          Ok t
      | None, None ->
          Ok t
      | _ ->
          failwith "functions should have Function type" )
  | _ ->
      failwith "pipelines should have Function type"

let check_pipeline_declaration_sig env loc pd =
  let Ast.{pd_name; pd_type; pd_functions} = pd in
  match Env.find_name ~local:true pd_name env with
  | Some Located.{loc= prev_loc; _} ->
      error loc (`Redefinition (pd_name, prev_loc))
  | None ->
      check_type env loc pd_type >>= fun clean_type ->
      expand_pipeline_type env loc clean_type pd_functions
      >>= fun clean_type ->
      Ok (Env.add_pipeline pd_name {loc; value= clean_type} env)

let check_pipeline_declaration_body env loc pd =
  let Ast.{pd_name; pd_type; pd_functions} = pd in
  check_type env loc pd_type >>= fun clean_type ->
  let env = Env.enter_pipeline_scope pd_name clean_type env in
  let env = build_function_environment env loc "" clean_type in
  check_function_group env pd_functions >>= fun typed_functions ->
  (* TODO: check pipeline preconditions? *)
  Ok
    (TypedAst.PipelineDecl
       { pd_env= env
       ; pd_name
       ; pd_type= clean_type
       ; pd_functions= typed_functions })

let check_renderer_declaration_sig env loc rd =
  let Ast.{rd_name; rd_type; _} = rd in
  match Env.find_name ~local:true rd_name env with
  | Some Located.{loc= prev_loc; _} ->
      error loc (`Redefinition (rd_name, prev_loc))
  | None ->
      check_type env loc rd_type >>= fun clean_type ->
      Ok (Env.add_renderer rd_name {loc; value= clean_type} env)

let check_renderer_declaration_body env loc rd =
  let Ast.{rd_name; rd_type; rd_functions} = rd in
  check_type env loc rd_type >>= fun clean_type ->
  let env = Env.enter_renderer_scope rd_name clean_type env in
  let env = build_function_environment env loc "" clean_type in
  check_function_group env rd_functions >>= fun typed_functions ->
  Ok
    (TypedAst.RendererDecl
       { rd_env= env
       ; rd_name
       ; rd_type= clean_type
       ; rd_functions= typed_functions })

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
