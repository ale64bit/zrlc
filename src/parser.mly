%token CONST TYPE PIPELINE RENDERER DEF VAR
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

program:
  | l=toplevel_elem* EOF { l }

toplevel_elem:
  | e=const_def { e }
  | e=type_def { e }
  | e=pipeline_def { e }
  | e=renderer_def { e }

const_def:
  | CONST id=ID ASSIGN e=constant { Ast.ConstDecl {cd_name=id; cd_value=e} }

type_def:
  | TYPE id=ID LBRACE fields=flatten(field_decl+) RBRACE 
    { Ast.TypeDecl {td_name=id; td_type=(Type.Record fields)} }

field_decl:
  | ids=separated_list(COMMA, ID) COLON t=type_id 
    { List.map (fun id -> {Type.name=id; Type.t=t}) ids }

type_id:
  | id=ID 
    { Type.TypeRef id }
  | LBRACKET id=ID COMMA dims=separated_list(COMMA, array_dimension) RBRACKET 
    { Type.Array (Type.TypeRef id, dims) }

array_dimension:
  | i=INT 
    { OfInt i }
  | id=ID 
    { OfName id }

pipeline_def:
  | PIPELINE id=ID t=function_signature LBRACE fs=func* RBRACE
    { Ast.PipelineDecl {pd_name=id; pd_type=t; pd_functions=fs} }

renderer_def:
  | RENDERER id=ID t=function_signature LBRACE stmts=stmt* RBRACE
    { Ast.RendererDecl {rd_name=id; rd_type=t; rd_body=stmts} }

function_signature:
  | LPAREN args=function_args RPAREN ret=function_ret_args?
    { 
      let v = match ret with
        | Some tl -> tl
        | None -> [] in
      Type.Function (args, v)
    }

function_args:
  | args=flatten(separated_list(COMMA, field_decl)) 
    { args }

function_ret_args:
  | COLON t=type_id 
    { [t] } 
  | COLON LPAREN tl=separated_nonempty_list(COMMA, type_id) RPAREN 
    { tl }

func:
  | DEF id=ID t=function_signature LBRACE stmts=stmt* RBRACE 
    { {fd_name=id; fd_type=t; fd_body=stmts} }

stmt:
  | VAR ids=separated_nonempty_list(COMMA, ID) ASSIGN rhs=separated_nonempty_list(COMMA, expr) 
    { Ast.Var {var_ids=ids; var_values=rhs} }
  | lhs=separated_nonempty_list(COMMA, lvalue) op=assign_op rhs=separated_nonempty_list(COMMA, expr) 
    { Ast.Assignment {asg_op=op; asg_lvalues=lhs; asg_rvalues=rhs} }
  | IF cond=expr LBRACE stmts=stmt* RBRACE 
    { Ast.If {if_cond=cond; if_body=stmts} }
  | FOR id=ID IN it=expr LBRACE stmts=stmt* RBRACE 
    { Ast.ForIter {foriter_id=id; foriter_it=it; foriter_body=stmts} }  
  | FOR id=ID ASSIGN lo=expr TO hi=expr LBRACE stmts=stmt* RBRACE 
    { Ast.ForRange {forrange_id=id; forrange_from=lo; forrange_to=hi; forrange_body=stmts} }
  | RETURN e=separated_nonempty_list(COMMA, expr) 
    { Ast.Return e }

expr:
  | lhs=expr op=binary_op rhs=expr 
    { Ast.BinExpr (lhs, op, rhs) }
  | e=unary_expr 
    { e }

lvalue:
  | e=lvalue DOT id=ID 
    { Ast.Access (e, id) }
  | e=lvalue LBRACKET args=separated_nonempty_list(COMMA, expr) RBRACKET 
    { Ast.Index (e, args) }
  | id=ID 
    { Ast.Id id }

unary_expr:
  | op=unary_op e=unary_expr 
    { Ast.UnExpr (op, e) }
  | e=primary_expr 
    { e }

arg_expr:
  | e=expr
    { e }
  | id=ID ASSIGN e=expr
    { Ast.NamedArg (id, e) }
  | id=ID ASSIGN LBRACE e=separated_nonempty_list(COMMA, expr) RBRACE
    { Ast.NamedArg (id, Ast.BundledArg e) }

primary_expr:
  | LPAREN e=expr RPAREN
    { e }
  | e=primary_expr DOT id=ID 
    { Ast.Access (e, id) }
  | e=primary_expr LBRACKET args=separated_nonempty_list(COMMA, expr) RBRACKET 
    { Ast.Index (e, args) }
  | e=primary_expr LPAREN args=separated_list(COMMA, arg_expr) RPAREN 
    { Ast.Call (e, args) }
  | id=ID 
    { Ast.Id id }
  | c=constant 
    { c }

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

let constant ==
  | b=BOOL;  { Ast.BoolLiteral b }
  | i=INT;   { Ast.IntLiteral i }
  | f=FLOAT; { Ast.FloatLiteral f }
