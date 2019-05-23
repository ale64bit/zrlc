open OUnit2
open Zrl
open Zrl.Monad.Result

(* Helpers *)

let typed_ast_printer = function
  | Ok ast ->
      "Ok (" ^ TypedAst.string_of_ast ast ^ ")"
  | Error e ->
      "Error (" ^ Error.debug_string_of_error e ^ ")"

let analysis_test src want =
  let lexbuf = Lexing.from_string src in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= "test"} ;
  let got = Parsing.parse lexbuf >>= fun ast -> Analysis.check ast in
  assert_equal ~printer:typed_ast_printer want got

let analysis_ok_test src root_env root_elems _ =
  analysis_test src (Ok {root_env; root_module= "test"; root_elems})

let analysis_error_test src want _ = analysis_test src (Error want)

let loc lnum bol cnum =
  Lexing.{pos_fname= "test"; pos_lnum= lnum; pos_bol= bol; pos_cnum= cnum}

let loc_of ?(n = 0) s p =
  let rec aux s from last (lnum, bol) =
    match String.index_from_opt s from '\n' with
    | Some i ->
        if i >= last then (lnum, bol)
        else aux s (i + 1) last (lnum + 1, bol + i - from + 1)
    | None ->
        (lnum, bol)
  in
  let rex = Pcre.regexp ~flags:[`DOTALL; `MULTILINE] p in
  let subs = Pcre.exec_all ~rex s in
  if n >= Array.length subs then raise Not_found
  else
    let first_cnum, last_cnum = Pcre.get_substring_ofs subs.(n) 0 in
    let first_lnum, first_bol = aux s 0 first_cnum (1, 0) in
    let last_lnum, last_bol = aux s 0 last_cnum (1, 0) in
    (loc first_lnum first_bol first_cnum, loc last_lnum last_bol last_cnum)

(* Tests *)

let test_empty =
  let src = "module test" in
  let want_env = Env.(global |> enter_module_scope "test") in
  let want_ast = [] in
  "test_empty" >:: analysis_ok_test src want_env want_ast

let test_const_bool_true =
  let src = {|
    module test
    const b = true
    |} in
  let stmt_loc = loc_of src (Pcre.quote "const b = true") in
  let val_loc = loc_of src (Pcre.quote "true") in
  let const_expr_true = Located.{loc= val_loc; value= Ast.BoolLiteral true} in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "b"; cd_value= ([Type.TypeRef "bool"], const_expr_true)}
  in
  let want_env =
    Env.(
      global |> enter_module_scope "test"
      |> add_constant "b" {loc= stmt_loc; value= Env.Bool true})
  in
  let want_ast = [cd] in
  "test_const_bool_true" >:: analysis_ok_test src want_env want_ast

let test_const_bool_false =
  let src = {|
    module test
    const b = false
    |} in
  let stmt_loc = loc_of src (Pcre.quote "const b = false") in
  let val_loc = loc_of src (Pcre.quote "false") in
  let const_expr_false =
    Located.{loc= val_loc; value= Ast.BoolLiteral false}
  in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "b"; cd_value= ([Type.TypeRef "bool"], const_expr_false)}
  in
  let want_env =
    Env.(
      global |> enter_module_scope "test"
      |> add_constant "b" {loc= stmt_loc; value= Env.Bool false})
  in
  let want_ast = [cd] in
  "test_const_bool_false" >:: analysis_ok_test src want_env want_ast

let test_const_int =
  let src = {|
    module test
    const i = 42
    |} in
  let stmt_loc = loc_of src (Pcre.quote "const i = 42") in
  let val_loc = loc_of src (Pcre.quote "42") in
  let const_expr_42 = Located.{loc= val_loc; value= Ast.IntLiteral 42} in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "i"; cd_value= ([Type.TypeRef "int"], const_expr_42)}
  in
  let want_env =
    Env.(
      global |> enter_module_scope "test"
      |> add_constant "i" {loc= stmt_loc; value= Env.Int 42})
  in
  let want_ast = [cd] in
  "test_const_int" >:: analysis_ok_test src want_env want_ast

let test_const_float =
  let src = {|
    module test
    const pi = 3.14
    |} in
  let stmt_loc = loc_of src (Pcre.quote "const pi = 3.14") in
  let val_loc = loc_of src (Pcre.quote "3.14") in
  let const_expr_pi = Located.{loc= val_loc; value= Ast.FloatLiteral 3.14} in
  let cd =
    TypedAst.ConstDecl
      {cd_name= "pi"; cd_value= ([Type.TypeRef "float"], const_expr_pi)}
  in
  let want_env =
    Env.(
      global |> enter_module_scope "test"
      |> add_constant "pi" {loc= stmt_loc; value= Env.Float 3.14})
  in
  let want_ast = [cd] in
  "test_const_float" >:: analysis_ok_test src want_env want_ast

let test_empty_pipeline =
  let src = {|
    module test
    pipeline P(x: int): float {}
    |} in
  let stmt_loc = loc_of src (Pcre.quote "pipeline P(x: int): float {}") in
  let args = [("x", Type.TypeRef "int")] in
  let rets = [Type.TypeRef "float"] in
  let pipeline_type = Type.Function (args, rets) in
  let want_env =
    Env.(
      global |> enter_module_scope "test"
      |> add_pipeline "P" {loc= stmt_loc; value= pipeline_type})
  in
  let pipeline_env =
    Env.(
      want_env
      |> enter_pipeline_scope "P" pipeline_type
      |> add_var "x" {loc= stmt_loc; value= Type.TypeRef "int"})
  in
  let pd =
    TypedAst.PipelineDecl
      { pd_env= pipeline_env
      ; pd_name= "P"
      ; pd_type= pipeline_type
      ; pd_functions= [] }
  in
  let want_ast = [pd] in
  "test_empty_pipeline" >:: analysis_ok_test src want_env want_ast

let test_type_declaration =
  let src =
    {|
    module test
    type T {
      x: int
      a: [64,132]vec4
    }
    |}
  in
  let typ =
    Type.(
      Record
        [ ("x", TypeRef "int")
        ; ("a", Array (TypeRef "vec4", [OfInt 64; OfInt 132])) ])
  in
  let typ_loc = loc_of src "type T {.*}" in
  let td = TypedAst.TypeDecl {td_name= "T"; td_type= typ} in
  let want_env =
    Env.(
      global |> enter_module_scope "test"
      |> add_type "T" Located.{loc= typ_loc; value= typ})
  in
  let want_ast = [td] in
  "test_type_declaration" >:: analysis_ok_test src want_env want_ast

let test_const_redefined =
  let src = {|
    module test
    const i = 1
    const i = 2
    |} in
  let prev_loc = loc_of src (Pcre.quote "const i = 1") in
  let want_loc = loc_of src (Pcre.quote "const i = 2") in
  let want_err =
    Located.{loc= want_loc; value= `Redefinition ("i", prev_loc)}
  in
  "test_const_redefined" >:: analysis_error_test src want_err

let test_duplicate_member =
  let src =
    {|
    module test
    type T {
      f: float
      f: int
    }
    |}
  in
  let want_loc = loc_of src "type T {.*}" in
  let want_err = Located.{loc= want_loc; value= `DuplicateMember "f"} in
  "test_duplicate_member" >:: analysis_error_test src want_err

let test_duplicate_parameter =
  let src =
    {|
    module test
    pipeline P() {
      def f(x: int, x: float) {}
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "def f(x: int, x: float) {}") in
  let want_err = Located.{loc= want_loc; value= `DuplicateParameter "x"} in
  "test_duplicate_parameter" >:: analysis_error_test src want_err

let test_unknown_type_name =
  let src = {|
    module test
    type T {
      x: X
    }
    |} in
  let want_loc = loc_of src "type T {.*}" in
  let want_err = Located.{loc= want_loc; value= `UnknownTypeName "X"} in
  "test_unknown_type_name" >:: analysis_error_test src want_err

let test_pipeline_redefined =
  let src =
    {|
    module test
    pipeline P() {}
    pipeline P() {}
    |}
  in
  let prev_loc = loc_of ~n:0 src (Pcre.quote "pipeline P() {}") in
  let want_loc = loc_of ~n:1 src (Pcre.quote "pipeline P() {}") in
  let want_err =
    Located.{loc= want_loc; value= `Redefinition ("P", prev_loc)}
  in
  "test_pipeline_redefined" >:: analysis_error_test src want_err

let test_pipeline_param_redefined =
  let src = {|
    module test
    pipeline P(x: int, x: float) {}
    |} in
  let want_loc = loc_of src (Pcre.quote "pipeline P(x: int, x: float) {}") in
  let want_err = Located.{loc= want_loc; value= `DuplicateParameter "x"} in
  "test_pipeline_param_redefined" >:: analysis_error_test src want_err

let test_pipeline_function_redefined =
  let src =
    {|
    module test
    pipeline P() {
      def f() {}
      def f() {}
    }
    |}
  in
  let prev_loc = loc_of src ~n:0 (Pcre.quote "def f() {}") in
  let want_loc = loc_of src ~n:1 (Pcre.quote "def f() {}") in
  let want_err =
    Located.{loc= want_loc; value= `Redefinition ("f", prev_loc)}
  in
  "test_pipeline_function_redefined" >:: analysis_error_test src want_err

let test_assignment_mismatch =
  let src =
    {|
    module test
    pipeline P() {
      def f() {
        var x, y = 1, 2, 3
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "var x, y = 1, 2, 3") in
  let want_err = Located.{loc= want_loc; value= `AssignmentMismatch (2, 3)} in
  "test_assignment_mismatch" >:: analysis_error_test src want_err

let test_non_integer_array_index =
  let src =
    {|
    module test
    pipeline P(xs: [2,4,8]int) {
      def f() {
        var x = xs[0,false,3]
      }
    }
    |}
  in
  let want_loc = loc_of src "false" in
  let expr = Located.{loc= want_loc; value= Ast.BoolLiteral false} in
  let want_err = Located.{loc= want_loc; value= `NonIntegerArrayIndex expr} in
  "test_non_integer_array_index" >:: analysis_error_test src want_err

let test_invalid_unary_operation =
  let src =
    {|
    module test
    pipeline P() {
      def f() {
        var x = -true
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "-true") in
  let want_err =
    Located.
      { loc= want_loc
      ; value= `InvalidUnaryOperation (Ast.UMinus, Type.TypeRef "bool") }
  in
  "test_invalid_unary_operation" >:: analysis_error_test src want_err

let test_invalid_binary_operation =
  let src =
    {|
    module test
    pipeline P() {
      def f() {
        var x = 1 + 2.0
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "1 + 2.0") in
  let expr =
    Located.
      { loc= want_loc
      ; value=
          Ast.BinExpr
            ( Located.{loc= loc_of src "1"; value= Ast.IntLiteral 1}
            , Ast.Plus
            , Located.
                { loc= loc_of src (Pcre.quote "2.0")
                ; value= Ast.FloatLiteral 2.0 } ) }
  in
  let want_err =
    Located.
      { loc= want_loc
      ; value=
          `InvalidBinaryOperation
            (expr, Type.TypeRef "int", Type.TypeRef "float") }
  in
  "test_invalid_binary_operation" >:: analysis_error_test src want_err

let test_invalid_index_operation =
  let src =
    {|
    module test
    pipeline P() {
      def F() {
        var x = F[6, 2]
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "F[6, 2]") in
  let expr =
    Located.
      { loc= want_loc
      ; value=
          Ast.Index
            ( Located.{loc= loc_of ~n:1 src "F"; value= Ast.Id "F"}
            , [ Located.{loc= loc_of src "6"; value= Ast.IntLiteral 6}
              ; Located.{loc= loc_of src "2"; value= Ast.IntLiteral 2} ] ) }
  in
  let want_err =
    Located.
      { loc= want_loc
      ; value= `InvalidIndexOperation (expr, Type.Function ([], [])) }
  in
  "test_invalid_index_operation" >:: analysis_error_test src want_err

let test_invalid_call_operation =
  let src =
    {|
    module test
    pipeline P() {
      def f() {
        var x = 42(false)
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "42(false)") in
  let expr = Located.{loc= loc_of src "42"; value= Ast.IntLiteral 42} in
  let want_err =
    Located.
      {loc= want_loc; value= `InvalidCallOperation (expr, Type.TypeRef "int")}
  in
  "test_invalid_call_operation" >:: analysis_error_test src want_err

let test_not_an_expression =
  let src =
    {|
    module test
    type T {
      x: int
    }
    pipeline P() {
      def f() {
        var x = 1+T
      }
    }
    |}
  in
  let want_loc = loc_of ~n:1 src (Pcre.quote "T") in
  let want_err = Located.{loc= want_loc; value= `NotAnExpression "T"} in
  "test_not_an_expression" >:: analysis_error_test src want_err

let test_no_such_member =
  let src =
    {|
    module test
    type T {
      x: int
    }
    pipeline P() {
      def f(t: T) {
        var x = t.y
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "t.y") in
  let want_err =
    Located.{loc= want_loc; value= `NoSuchMember (Type.TypeRef "T", "y")}
  in
  "test_no_such_member" >:: analysis_error_test src want_err

let test_not_enough_arguments =
  let src =
    {|
    module test
    pipeline P() {
      def f1(x, y: int) {}
      def f2() {
        return f1(1)
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "f1(1)") in
  let expr = Located.{loc= loc_of ~n:1 src "f1"; value= Ast.Id "f1"} in
  let have_types = List.init 1 (fun _ -> Type.TypeRef "int") in
  let want_types = List.init 2 (fun _ -> Type.TypeRef "int") in
  let want_err =
    Located.
      {loc= want_loc; value= `NotEnoughArguments (expr, have_types, want_types)}
  in
  "test_not_enough_arguments" >:: analysis_error_test src want_err

let test_too_many_arguments =
  let src =
    {|
    module test
    pipeline P() {
      def f1(x, y: int) {}
      def f2() {
        return f1(1, 2, 3)
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "f1(1, 2, 3)") in
  let expr = Located.{loc= loc_of ~n:1 src "f1"; value= Ast.Id "f1"} in
  let have_types = List.init 3 (fun _ -> Type.TypeRef "int") in
  let want_types = List.init 2 (fun _ -> Type.TypeRef "int") in
  let want_err =
    Located.
      {loc= want_loc; value= `TooManyArguments (expr, have_types, want_types)}
  in
  "test_too_many_arguments" >:: analysis_error_test src want_err

let test_not_enough_return_arguments =
  let src =
    {|
    module test
    pipeline P() {
      def f1(): (int, int) {
        return 1
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "return 1") in
  let have_types = List.init 1 (fun _ -> Type.TypeRef "int") in
  let want_types = List.init 2 (fun _ -> Type.TypeRef "int") in
  let want_err =
    Located.
      {loc= want_loc; value= `NotEnoughReturnArguments (have_types, want_types)}
  in
  "test_not_enough_return_arguments" >:: analysis_error_test src want_err

let test_too_many_return_arguments =
  let src =
    {|
    module test
    pipeline P() {
      def f1(): (int, int) {
        return 1, 2, 3
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "return 1, 2, 3") in
  let have_types = List.init 3 (fun _ -> Type.TypeRef "int") in
  let want_types = List.init 2 (fun _ -> Type.TypeRef "int") in
  let want_err =
    Located.
      {loc= want_loc; value= `TooManyReturnArguments (have_types, want_types)}
  in
  "test_too_many_return_arguments" >:: analysis_error_test src want_err

let test_not_enough_indices =
  let src =
    {|
    module test
    pipeline P() {
      def f(array: [8,8,8]float) {
        var x = array[1,2]
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "array[1,2]") in
  let expr = Located.{loc= loc_of ~n:1 src "array"; value= Ast.Id "array"} in
  let want_err =
    Located.{loc= want_loc; value= `NotEnoughIndices (expr, 2, 3)}
  in
  "test_not_enough_indices" >:: analysis_error_test src want_err

let test_too_many_indices =
  let src =
    {|
    module test
    pipeline P() {
      def f(array: [8,8,8]float) {
        var x = array[1,2,3,4]
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "array[1,2,3,4]") in
  let expr = Located.{loc= loc_of ~n:1 src "array"; value= Ast.Id "array"} in
  let want_err =
    Located.{loc= want_loc; value= `TooManyIndices (expr, 4, 3)}
  in
  "test_too_many_indices" >:: analysis_error_test src want_err

let test_multiple_value_in_single_value_context =
  let src =
    {|
    module test
    pipeline P() {
      def f1(): (int, int) { return 1, 2 }
      def f2() {
        var x, y = f1(), 2
      }
    }
    |}
  in
  let want_loc = loc_of ~n:1 src (Pcre.quote "f1()") in
  let expr =
    Located.
      { loc= loc_of ~n:1 src (Pcre.quote "f1()")
      ; value=
          Ast.Call (Located.{loc= loc_of ~n:1 src "f1"; value= Ast.Id "f1"}, [])
      }
  in
  let want_err =
    Located.{loc= want_loc; value= `MultipleValueInSingleValueContext expr}
  in
  "test_multiple_value_in_single_value_context"
  >:: analysis_error_test src want_err

let test_mixed_argument_style =
  let src =
    {|
    module test
    pipeline P() {
      def f1(x, y: int) {}
      def f2() {
        return f1(111, y=222)
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "f1(111, y=222)") in
  let expr =
    Located.
      { loc= want_loc
      ; value=
          Ast.Call
            ( Located.
                {loc= loc_of ~n:1 src (Pcre.quote "f1"); value= Ast.Id "f1"}
            , [ Located.
                  { loc= loc_of src (Pcre.quote "111")
                  ; value= Ast.IntLiteral 111 }
              ; Located.
                  { loc= loc_of src (Pcre.quote "y=222")
                  ; value=
                      Ast.NamedArg
                        ( "y"
                        , Located.
                            { loc= loc_of src (Pcre.quote "222")
                            ; value= Ast.IntLiteral 222 } ) } ] ) }
  in
  let want_err = Located.{loc= want_loc; value= `MixedArgumentStyle expr} in
  "test_mixed_argument_style" >:: analysis_error_test src want_err

let test_invalid_argument =
  let src =
    {|
    module test
    pipeline P() {
      def f1(x: int) {}
      def f2() {
        return f1(false)
      }
    }
    |}
  in
  let want_loc = loc_of src "false" in
  let have_type = Type.TypeRef "bool" in
  let want_type = Type.TypeRef "int" in
  let expr = Located.{loc= loc_of src "false"; value= Ast.BoolLiteral false} in
  let want_err =
    Located.
      { loc= want_loc
      ; value= `InvalidArgument (expr, have_type, want_type, "f1") }
  in
  "test_invalid_argument" >:: analysis_error_test src want_err

let test_invalid_return_argument =
  let src =
    {|
    module test
    pipeline P() {
      def f(): int {
        return false
      }
    }
    |}
  in
  let want_loc = loc_of src "false" in
  let have_type = Type.TypeRef "bool" in
  let want_type = Type.TypeRef "int" in
  let expr = Located.{loc= want_loc; value= Ast.BoolLiteral false} in
  let want_err =
    Located.
      { loc= want_loc
      ; value= `InvalidReturnArgument (expr, have_type, want_type) }
  in
  "test_invalid_return_argument" >:: analysis_error_test src want_err

let test_missing_named_argument =
  let src =
    {|
    module test
    pipeline P() {
      def f(x: int, y: int) {}
      def g() {
        return f(x=1, x=2)
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "f(x=1, x=2)") in
  let want_err =
    Located.{loc= want_loc; value= `MissingNamedArgument ("y", "f")}
  in
  "test_missing_named_argument" >:: analysis_error_test src want_err

let test_unexpected_named_argument =
  let src =
    {|
    module test
    pipeline P() {
      def f(x: int, y: int) {}
      def g() {
        return f(x=1, z=2)
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "f(x=1, z=2)") in
  let want_err =
    Located.{loc= want_loc; value= `UnexpectedNamedArgument ("z", "f")}
  in
  "test_unexpected_named_argument" >:: analysis_error_test src want_err

let test_unit_used_as_value =
  let src =
    {|
    module test
    pipeline P() {
      def f1() {}
      def f2() {
        var x, y = 1, f1()
      }
    }
    |}
  in
  let want_loc = loc_of ~n:1 src (Pcre.quote "f1()") in
  let expr =
    Located.
      { loc= want_loc
      ; value=
          Ast.Call (Located.{loc= loc_of ~n:1 src "f1"; value= Ast.Id "f1"}, [])
      }
  in
  let want_err = Located.{loc= want_loc; value= `UnitUsedAsValue expr} in
  "test_unit_used_as_value" >:: analysis_error_test src want_err

let test_not_an_lvalue =
  let src =
    {|
    module test
    pipeline P() {
      def f1() {}
      def f2() {
        f1 = true
      }
    }
    |}
  in
  let want_loc = loc_of ~n:1 src "f1" in
  let expr = Located.{loc= want_loc; value= Ast.Id "f1"} in
  let want_err = Located.{loc= want_loc; value= `NotAnLValue expr} in
  "test_not_an_lvalue" >:: analysis_error_test src want_err

let test_invalid_single_assignment =
  let src =
    {|
    module test
    pipeline P() {
      def f() {
        var x, y = 1, 2
        x, y = 3, true
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "x, y = 3, true") in
  let have_type = Type.TypeRef "bool" in
  let want_type = Type.TypeRef "int" in
  let expr = Located.{loc= loc_of src "true"; value= Ast.BoolLiteral true} in
  let want_err =
    Located.
      { loc= want_loc
      ; value= `InvalidSingleAssignment (expr, have_type, want_type) }
  in
  "test_invalid_single_assignment" >:: analysis_error_test src want_err

let test_invalid_multiple_assignment =
  let src =
    {|
    module test
    pipeline P() {
      def f1(): (int, bool) { return 1, true }
      def f2() {
        var x, y = 1, 2
        x, y = f1()
      }
    }
    |}
  in
  let want_loc = loc_of src (Pcre.quote "x, y = f1()") in
  let have_type = Type.TypeRef "bool" in
  let expr = Located.{loc= loc_of ~n:1 src "y"; value= Ast.Id "y"} in
  let want_type = Type.TypeRef "int" in
  let want_err =
    Located.
      { loc= want_loc
      ; value= `InvalidMultipleAssignment (have_type, expr, want_type) }
  in
  "test_invalid_multiple_assignment" >:: analysis_error_test src want_err

let tests =
  "analysis_suite"
  >::: [ test_empty
       ; test_const_bool_true
       ; test_const_bool_false
       ; test_const_int
       ; test_const_float
       ; test_empty_pipeline
       ; test_type_declaration
       ; test_const_redefined
       ; test_duplicate_member
       ; test_duplicate_parameter
       ; test_unknown_type_name
       ; test_non_integer_array_index
       ; test_pipeline_redefined
       ; test_pipeline_param_redefined
       ; test_pipeline_function_redefined
       ; test_assignment_mismatch
       ; test_invalid_unary_operation
       ; test_invalid_binary_operation
       ; test_invalid_index_operation
       ; test_invalid_call_operation
       ; test_not_an_expression
       ; test_no_such_member
       ; test_not_enough_arguments
       ; test_too_many_arguments
       ; test_not_enough_return_arguments
       ; test_too_many_return_arguments
       ; test_not_enough_indices
       ; test_too_many_indices
       ; test_multiple_value_in_single_value_context
       ; test_mixed_argument_style
       ; test_invalid_argument
       ; test_invalid_return_argument
       ; test_missing_named_argument
       ; test_unexpected_named_argument
       ; test_unit_used_as_value
       ; test_not_an_lvalue
       ; test_invalid_single_assignment
       ; test_invalid_multiple_assignment ]

let _ = run_test_tt_main tests
