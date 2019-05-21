%token MODULE CONST TYPE PIPELINE RENDERER DEF VAR
%token IF FOR IN TO RETURN

%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token <string> ID

%token LPAREN   RPAREN
%token LBRACKET RBRACKET
%token LBRACE   RBRACE
%token COMMA DOT COLON

%token ASSIGN ASSIGN_PLUS ASSIGN_MINUS ASSIGN_MULT ASSIGN_DIV ASSIGN_MOD
%token LOGICAL_OR
%token LOGICAL_XOR
%token LOGICAL_AND
%token BITWISE_OR
%token BITWISE_XOR
%token BITWISE_AND
%token EQ NEQ
%token LT GT LE GE
%token SHIFTLEFT SHIFTRIGHT
%token PLUS MINUS
%token MULT DIV MOD
%token LOGICAL_NOT BITWISE_COMPLEMENT

%token EOF

(* %right ASSIGN ASSIGN_PLUS ASSIGN_MINUS ASSIGN_MULT ASSIGN_DIV ASSIGN_MOD *)
%left LOGICAL_OR
%left LOGICAL_XOR
%left LOGICAL_AND
%left BITWISE_OR
%left BITWISE_XOR
%left BITWISE_AND
%left EQ NEQ
%left LT GT LE GE
%left SHIFTLEFT SHIFTRIGHT
%left PLUS MINUS
%left MULT DIV MOD
(* %right LOGICAL_NOT BITWISE_COMPLEMENT *)

%start <Ast.root> program

%%

let program :=
  MODULE; module_name = ID; elements = toplevel_elem*; EOF; {
    Ast.{module_name; elements}
  }

let toplevel_elem := 
  located(
  | const_def
  | type_def
  | pipeline_def
  | renderer_def
  )

let const_def :=
  CONST; id = ID; ASSIGN; value = expr; 
    { Ast.ConstDecl {cd_name = id; cd_value = value} }

let type_def :=
  TYPE; id = ID; LBRACE; fields=flatten(field_decl+); RBRACE;
    { Ast.TypeDecl {td_name=id; td_type=(Type.Record fields)} }

let field_decl :=
  ids = separated_list(COMMA, ID); COLON; t=type_id;
    { List.map (fun id -> (id, t)) ids }

let type_id :=
  | ~ = ID; <Type.TypeRef>
  | LBRACKET; dims = separated_nonempty_list(COMMA, array_dimension); RBRACKET; id = ID;
    { Type.Array (Type.TypeRef id, dims) }

let array_dimension ==
  | ~ = INT; <Type.OfInt>
  | ~ = ID; <Type.OfName>

let pipeline_def :=
  PIPELINE; id = ID; fsign = function_signature; LBRACE; funcs = located(func)*; RBRACE; 
    { Ast.PipelineDecl {pd_name = id; pd_type = fsign; pd_functions = funcs} }

let renderer_def :=
  RENDERER; id = ID; fsign = function_signature; LBRACE; stmts = stmt*; RBRACE; 
    { Ast.RendererDecl {rd_name = id; rd_type = fsign; rd_body = stmts} }

let function_signature :=
  LPAREN; args = function_args; RPAREN; ret = function_ret_args?;
    { 
      let v = match ret with
        | Some tl -> tl
        | None -> [] in
      Type.Function (args, v)
    }

let function_args ==
  ~ = flatten(separated_list(COMMA, field_decl)); <>

let function_ret_args :=
  | COLON; t=type_id; { [t] }
  | COLON; LPAREN; ~ = separated_nonempty_list(COMMA, type_id); RPAREN; <>

let func :=
  DEF; id = ID; fsign = function_signature; LBRACE; body = stmt*; RBRACE; 
    { {fd_name = id; fd_type = fsign; fd_body = body} }

let var_declaration ==
  VAR; 
  var_ids=separated_nonempty_list(COMMA, ID); 
  ASSIGN; 
  var_values=separated_nonempty_list(COMMA, expr);
    { Ast.Var {var_ids; var_values} }

let assignment ==
  lhs=separated_nonempty_list(COMMA, lvalue); 
  op=assign_op; 
  rhs=separated_nonempty_list(COMMA, expr);
    { Ast.Assignment {asg_op=op; asg_lvalues=lhs; asg_rvalues=rhs} }

let if_stmt ==
  IF; cond=expr; LBRACE; stmts=stmt*; RBRACE;
    { Ast.If {if_cond=cond; if_body=stmts} }

let for_iter ==
  FOR; id=ID; IN; it=expr; LBRACE; stmts=stmt*; RBRACE;
    { Ast.ForIter {foriter_id=id; foriter_it=it; foriter_body=stmts} }  

let for_range ==
  FOR; id=ID; ASSIGN; lo=expr; TO; hi=expr; LBRACE; stmts=stmt*; RBRACE;
    { Ast.ForRange {forrange_id=id; forrange_from=lo; forrange_to=hi; forrange_body=stmts} }

let return ==
  RETURN; ~ = separated_nonempty_list(COMMA, expr); <Ast.Return>

let stmt :=
  located(
  | var_declaration
  | assignment
  | if_stmt
  | for_iter
  | for_range
  | return
  )

let expr :=
  | unary_expr 
  | located(
      lhs = expr; op = binary_op; rhs = expr; { Ast.BinExpr (lhs, op, rhs) }
    )

let lvalue :=
  located(
  | ~ = lvalue; DOT; ~ = ID; <Ast.Access>
  | ~ = lvalue; LBRACKET; ~ = separated_nonempty_list(COMMA, expr); RBRACKET; <Ast.Index>
  | ~ = ID; <Ast.Id>
  )

let unary_expr :=
  | primary_expr
  | located(
      ~ = unary_op; ~ = unary_expr; <Ast.UnExpr>
    )

let bundled_arg ==
  located(
    LBRACE; ~ = separated_nonempty_list(COMMA, expr); RBRACE; <Ast.BundledArg>
  )

let arg_expr :=
  | expr
  | located(
    | ~ = ID; ASSIGN; ~ = expr; <Ast.NamedArg>
    | ~ = ID; ASSIGN; ~ = bundled_arg; <Ast.NamedArg>
    )

let primary_expr :=
  | LPAREN; ~ = expr; RPAREN; <>
  | constant
  | located(
    | ~ = primary_expr; DOT; ~ = ID; <Ast.Access>
    | ~ = primary_expr; LBRACKET; ~ = separated_nonempty_list(COMMA, expr); RBRACKET; <Ast.Index>
    | ~ = primary_expr; LPAREN; ~ = separated_list(COMMA, arg_expr); RPAREN; <Ast.Call>
    | ~ = ID; <Ast.Id>
    )

let unary_op == 
  | PLUS;               { Ast.UPlus }
  | MINUS;              { Ast.UMinus }
  | LOGICAL_NOT;        { Ast.LogicalNot }
  | BITWISE_COMPLEMENT; { Ast.BitwiseComplement }

let binary_op ==
  | PLUS;           { Ast.Plus }
  | MINUS;          { Ast.Minus }
  | MULT;           { Ast.Mult }
  | DIV;            { Ast.Div }
  | MOD;            { Ast.Mod }
  | LOGICAL_OR;     { Ast.LogicalOr }
  | LOGICAL_XOR;    { Ast.LogicalXor }
  | LOGICAL_AND;    { Ast.LogicalAnd }
  | BITWISE_OR;     { Ast.BitwiseOr }
  | BITWISE_XOR;    { Ast.BitwiseXor }
  | BITWISE_AND;    { Ast.BitwiseAnd }
  | EQ;             { Ast.Equal }
  | NEQ;            { Ast.NotEqual }
  | LT;             { Ast.LessThan }
  | GT;             { Ast.GreaterThan }
  | LE;             { Ast.LessOrEqual }
  | GE;             { Ast.GreaterOrEqual }
  | SHIFTLEFT;      { Ast.ShiftLeft }
  | SHIFTRIGHT;     { Ast.ShiftRight }

let assign_op == 
  | ASSIGN;       { Ast.Assign }
  | ASSIGN_PLUS;  { Ast.AssignPlus }
  | ASSIGN_MINUS; { Ast.AssignMinus }
  | ASSIGN_MULT;  { Ast.AssignMult }
  | ASSIGN_DIV;   { Ast.AssignDiv }
  | ASSIGN_MOD;   { Ast.AssignMod }

let constant :=
  located(
  | ~ = BOOL;  <Ast.BoolLiteral>
  | ~ = INT;   <Ast.IntLiteral>
  | ~ = FLOAT; <Ast.FloatLiteral>
  )

let located(x) ==
  ~ = x; { { Located.loc = $loc; value = x } }
