module Node = struct
  type t = int * Graph.Graphviz.DotAttributes.vertex list

  let compare = Pervasives.compare

  let hash (i, _) = i

  let equal = ( = )
end

module G = Graph.Imperative.Digraph.ConcreteBidirectional (Node)

module Dot = Graph.Graphviz.Dot (struct
  include G

  let edge_attributes (_, _) = [`Color 4711]

  let default_edge_attributes _ = []

  let get_subgraph _ = None

  let vertex_attributes (_, attrs) = attrs

  let vertex_name (v, _) = string_of_int v

  let default_vertex_attributes _ = []

  let graph_attributes _ = [`OrderingOut]
end)

let string_of_unop = function
  | Ast.UPlus ->
      "unary+"
  | Ast.UMinus ->
      "unary-"
  | Ast.LogicalNot ->
      "!"
  | Ast.BitwiseComplement ->
      "~"

let string_of_binop = function
  | Ast.LogicalOr ->
      "||"
  | Ast.LogicalXor ->
      "^^"
  | Ast.LogicalAnd ->
      "&&"
  | Ast.BitwiseOr ->
      "|"
  | Ast.BitwiseXor ->
      "^"
  | Ast.BitwiseAnd ->
      "&"
  | Ast.Equal ->
      "=="
  | Ast.NotEqual ->
      "!="
  | Ast.LessThan ->
      "<"
  | Ast.GreaterThan ->
      ">"
  | Ast.LessOrEqual ->
      "<="
  | Ast.GreaterOrEqual ->
      ">="
  | Ast.ShiftLeft ->
      "<<"
  | Ast.ShiftRight ->
      ">>"
  | Ast.Plus ->
      "+"
  | Ast.Minus ->
      "-"
  | Ast.Mult ->
      "*"
  | Ast.Div ->
      "/"
  | Ast.Mod ->
      "%"

let string_of_assignop = function
  | Ast.Assign ->
      "="
  | Ast.AssignPlus ->
      "+="
  | Ast.AssignMinus ->
      "-="
  | Ast.AssignMult ->
      "*="
  | Ast.AssignDiv ->
      "/="
  | Ast.AssignMod ->
      "%="

let new_id = Stream.next

let new_generic_node s g attrs =
  let node = (new_id s, attrs) in
  G.add_vertex g node ; node

let new_node s g label =
  new_generic_node s g
    [`Label label; `Shape `Box; `Style `Filled; `Fillcolor 0x00EEEEEE]

let new_dummy_node s g label =
  new_generic_node s g
    [`Label label; `Shape `Box; `Style `Dashed; `Fillcolor 0x00777777]

let new_literal_node s g label =
  new_generic_node s g
    [`Label label; `Shape `Box; `Style `Filled; `Fillcolor 0x0042D7F4]

let new_ident_node s g label =
  new_generic_node s g
    [`Label label; `Shape `Box; `Style `Filled; `Fillcolor 0x00F9F0A7]

let new_op_node s g label =
  new_generic_node s g
    [`Label label; `Shape `Box; `Style `Filled; `Fillcolor 0x00F4A6AE]

let rec dot_expr s g e = dot_raw_expr s g e.Located.value

and dot_raw_expr s g = function
  | Ast.Access (lhs, rhs) ->
      let access_node = new_op_node s g "." in
      let lhs_node = dot_expr s g lhs in
      let rhs_node = new_ident_node s g rhs in
      G.add_edge g access_node lhs_node ;
      G.add_edge g access_node rhs_node ;
      access_node
  | Ast.Index (lhs, rhs) ->
      let index_node = new_op_node s g "[]" in
      let lhs_node = dot_expr s g lhs in
      let exprs_node = new_dummy_node s g "exprs" in
      G.add_edge g index_node lhs_node ;
      G.add_edge g index_node exprs_node ;
      List.iter (G.add_edge g exprs_node) (List.map (dot_expr s g) rhs) ;
      index_node
  | Ast.Call (lhs, rhs) ->
      let call_node = new_op_node s g "()" in
      let lhs_node = dot_expr s g lhs in
      let args_node = new_dummy_node s g "args" in
      G.add_edge g call_node lhs_node ;
      G.add_edge g call_node args_node ;
      List.iter (G.add_edge g args_node) (List.map (dot_expr s g) rhs) ;
      call_node
  | Ast.NamedArg (id, expr) ->
      let named_node = new_node s g "named_arg" in
      let id_node = new_node s g id in
      let expr_node = dot_expr s g expr in
      G.add_edge g named_node id_node ;
      G.add_edge g named_node expr_node ;
      named_node
  | Ast.BundledArg e ->
      let bundled_node = new_dummy_node s g "bundled_args" in
      List.iter (G.add_edge g bundled_node) (List.map (dot_expr s g) e) ;
      bundled_node
  | Ast.BinExpr (lhs, op, rhs) ->
      let op_node = new_op_node s g (string_of_binop op) in
      let lhs_node = dot_expr s g lhs in
      let rhs_node = dot_expr s g rhs in
      G.add_vertex g op_node ;
      G.add_edge g op_node lhs_node ;
      G.add_edge g op_node rhs_node ;
      op_node
  | Ast.UnExpr (op, rhs) ->
      let op_node = new_op_node s g (string_of_unop op) in
      let rhs_node = dot_expr s g rhs in
      G.add_edge g op_node rhs_node ;
      op_node
  | Ast.BoolLiteral b ->
      new_literal_node s g (string_of_bool b)
  | Ast.IntLiteral i ->
      new_literal_node s g (string_of_int i)
  | Ast.FloatLiteral f ->
      new_literal_node s g (string_of_float f)
  | Ast.Id id ->
      new_ident_node s g id

let rec dot_stmt s g e = dot_raw_stmt s g e.Located.value

and dot_raw_stmt s g = function
  | Ast.Var {var_ids; var_values} ->
      let var_node = new_node s g "var" in
      let id_node = new_dummy_node s g "ids" in
      let rhs_node = new_dummy_node s g "exprs" in
      G.add_edge g var_node id_node ;
      G.add_edge g var_node rhs_node ;
      List.iter (G.add_edge g id_node) (List.map (new_ident_node s g) var_ids) ;
      List.iter (G.add_edge g rhs_node) (List.map (dot_expr s g) var_values) ;
      var_node
  | Ast.Assignment {asg_op; asg_lvalues; asg_rvalues} ->
      let assign_node = new_op_node s g (string_of_assignop asg_op) in
      let lhs_node = new_dummy_node s g "lhs" in
      let rhs_node = new_dummy_node s g "rhs" in
      G.add_edge g assign_node lhs_node ;
      G.add_edge g assign_node rhs_node ;
      List.iter (G.add_edge g lhs_node) (List.map (dot_expr s g) asg_lvalues) ;
      List.iter (G.add_edge g rhs_node) (List.map (dot_expr s g) asg_rvalues) ;
      assign_node
  | Ast.If {if_cond; if_true; if_false} ->
      let if_node = new_node s g "if" in
      let cond_node = dot_expr s g if_cond in
      let true_node = new_dummy_node s g "if_true" in
      let false_node = new_dummy_node s g "if_false" in
      G.add_edge g if_node cond_node ;
      G.add_edge g if_node true_node ;
      G.add_edge g if_node false_node ;
      List.iter (G.add_edge g true_node) (List.map (dot_stmt s g) if_true) ;
      List.iter (G.add_edge g false_node) (List.map (dot_stmt s g) if_false) ;
      if_node
  | Ast.ForIter {foriter_id; foriter_it; foriter_body} ->
      let for_node = new_node s g "for_iter" in
      let id_node = new_ident_node s g foriter_id in
      let it_node = dot_expr s g foriter_it in
      G.add_edge g for_node id_node ;
      G.add_edge g for_node it_node ;
      List.iter (G.add_edge g for_node) (List.map (dot_stmt s g) foriter_body) ;
      for_node
  | Ast.ForRange {forrange_id; forrange_from; forrange_to; forrange_body} ->
      let for_node = new_node s g "for_range" in
      let id_node = new_node s g forrange_id in
      let lo_node = dot_expr s g forrange_from in
      let hi_node = dot_expr s g forrange_to in
      G.add_edge g for_node id_node ;
      G.add_edge g for_node lo_node ;
      G.add_edge g for_node hi_node ;
      List.iter (G.add_edge g for_node) (List.map (dot_stmt s g) forrange_body) ;
      for_node
  | Ast.Return exprs ->
      let ret_node = new_node s g "return" in
      List.iter (G.add_edge g ret_node) (List.map (dot_expr s g) exprs) ;
      ret_node

let dot_raw_func s g Ast.{fd_name; fd_type= _; fd_body} =
  let node = new_node s g (Printf.sprintf "func$%s" fd_name) in
  List.iter (G.add_edge g node) (List.map (dot_stmt s g) fd_body) ;
  node

let dot_func s g f = dot_raw_func s g f.Located.value

let dot_raw_toplevel s g = function
  | Ast.ConstDecl {cd_name; cd_value} ->
      let node = new_node s g (Printf.sprintf "const$%s" cd_name) in
      let rhs_node = dot_expr s g cd_value in
      G.add_edge g node rhs_node ; node
  | Ast.TypeDecl {td_name; td_type= _} ->
      new_node s g (Printf.sprintf "type$%s" td_name)
  | Ast.PipelineDecl {pd_name; pd_type= _; pd_functions} ->
      let node = new_node s g (Printf.sprintf "pipeline$%s" pd_name) in
      let children = List.map (dot_func s g) pd_functions in
      List.iter (G.add_edge g node) children ;
      node
  | Ast.RendererDecl {rd_name; rd_type= _; rd_body} ->
      let node = new_node s g (Printf.sprintf "renderer$%s" rd_name) in
      List.iter (G.add_edge g node) (List.map (dot_stmt s g) rd_body) ;
      node

let dot_toplevel s g e = dot_raw_toplevel s g e.Located.value

let dot ch Ast.{module_name; elements} =
  let ids = Stream.from (fun i -> Some i) in
  let g = G.create () in
  let root_node = new_node ids g (Printf.sprintf "module$%s" module_name) in
  G.add_vertex g root_node ;
  List.iter (G.add_edge g root_node) (List.map (dot_toplevel ids g) elements) ;
  Dot.output_graph ch g
