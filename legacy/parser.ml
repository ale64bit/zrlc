module Operators = Map.Make(String)

type associativity = Left | Right | None

type operator = {
  lexeme: string;
  precedence: int;
  arity: int;
  assoc: associativity;
}

let operators = Operators.of_seq (List.to_seq [
  (* Operators with precedence=0 are not used in the operator-precedence parser *)
  (* Highest *)
  ("(", {lexeme="("; precedence=0; arity=2; assoc=Left}); (* Method call and grouping *)
  (")", {lexeme=")"; precedence=0; arity=2; assoc=None}); (* Method call and grouping *)
  ("[", {lexeme="["; precedence=0; arity=2; assoc=Left}); (* Array indexing *)
  ("]", {lexeme="]"; precedence=0; arity=2; assoc=None}); (* Array indexing *)
  ("{", {lexeme="{"; precedence=0; arity=2; assoc=Left}); (* Block and binding groups *)
  ("}", {lexeme="}"; precedence=0; arity=2; assoc=None}); (* Block and binding groups *)
  (".", {lexeme="."; precedence=0; arity=2; assoc=Left}); (* Field or method access *)
  (* Unary *)
  (* ("-unary", {lexeme="-"; precedence=0; arity=1; assoc=Right}); *)
  (* ("+unary", {lexeme="+"; precedence=0; arity=1; assoc=Right}); *)
  ("!", {lexeme="!"; precedence=1; arity=1; assoc=Right});
  ("~", {lexeme="~"; precedence=1; arity=1; assoc=Right});
  (* Multiplicative *)
  ("*", {lexeme="*"; precedence=2; arity=2; assoc=Left});
  ("/", {lexeme="/"; precedence=2; arity=2; assoc=Left});
  ("%", {lexeme="%"; precedence=2; arity=2; assoc=Left});
  (* Additive *)
  ("+", {lexeme="+"; precedence=3; arity=2; assoc=Left});
  ("-", {lexeme="-"; precedence=3; arity=2; assoc=Left});
  (* Bitwise Shift *)
  ("<<", {lexeme="<<"; precedence=4; arity=2; assoc=Left});
  (">>", {lexeme=">>"; precedence=4; arity=2; assoc=Left});
  (* Relational *)
  ("<", {lexeme="<"; precedence=5; arity=2; assoc=Left});
  (">", {lexeme=">"; precedence=5; arity=2; assoc=Left});
  ("<=", {lexeme="<="; precedence=5; arity=2; assoc=Left});
  (">=", {lexeme=">="; precedence=5; arity=2; assoc=Left});
  (* Equality *)
  ("==", {lexeme="=="; precedence=6; arity=2; assoc=Left});
  ("!=", {lexeme="!="; precedence=6; arity=2; assoc=Left});
  (* Bitwise AND *)
  ("&", {lexeme="&"; precedence=7; arity=2; assoc=Left});
  (* Bitwise Exclusive OR *)
  ("^", {lexeme="^"; precedence=8; arity=2; assoc=Left});
  (* Bitwise Inclusive OR *)
  ("|", {lexeme="|"; precedence=9; arity=2; assoc=Left});
  (* Logical AND *)
  ("&&", {lexeme="&&"; precedence=10; arity=2; assoc=Left});
  (* Logical Exclusive OR *)
  ("^^", {lexeme="^^"; precedence=11; arity=2; assoc=Left});
  (* Logical Inclusive OR *)
  ("||", {lexeme="||"; precedence=12; arity=2; assoc=Left});
  (* Assignment *)
  ("=", {lexeme="="; precedence=13; arity=2; assoc=Right});
  ("+=", {lexeme="+="; precedence=13; arity=2; assoc=Right});
  ("-=", {lexeme="-="; precedence=13; arity=2; assoc=Right});
  ("*=", {lexeme="*="; precedence=13; arity=2; assoc=Right});
  ("/=", {lexeme="/="; precedence=13; arity=2; assoc=Right});
  ("%=", {lexeme="%="; precedence=13; arity=2; assoc=Right});
  ("<<=", {lexeme="<<="; precedence=13; arity=2; assoc=Right});
  (">>=", {lexeme=">>="; precedence=13; arity=2; assoc=Right});
  ("&=", {lexeme="&="; precedence=13; arity=2; assoc=Right});
  ("^=", {lexeme="^="; precedence=13; arity=2; assoc=Right});
  ("|=", {lexeme="|="; precedence=13; arity=2; assoc=Right});
  (* Comma *)
  (",", {lexeme=","; precedence=0; arity=2; assoc=Right});
  (* Type declaration *)
  (":", {lexeme=":"; precedence=0; arity=2; assoc=None});
])

let is_operator s = match Operators.find_opt s operators with
  | Some _ -> true
  | None -> false

let rec program root = parser
  | [< 'Token.Keyword "const";
       'Token.Id _ ?? "identifier expected"; 
       'Token.Operator "=" ?? "= expected"; 
       _=constant ?? "constant value expected"; 
       stream >] -> program root stream
  | [< 'Token.Keyword "type"; 
       'Token.Id name ?? "identifier expected";
       fields=field_block; 
       stream >] ->
      print_endline (name ^ " = " ^ (Type.string_of_type (Type.Record fields))) ;
      program root stream
  | [< >] -> root

and constant = parser
  | [< 'Token.Int _ >] -> ()
  | [< 'Token.Float _ >] -> ()
  | [< 'Token.Bool _ >] -> ()

and field_block =
  let rec aux acc = parser
    | [< fields=field_decl; e=aux (List.concat [acc; fields]) >] -> e
    | [< >] -> acc in
  parser
    | [< 'Token.Operator "{"; fields=aux []; 'Token.Operator "}" >] -> fields

and field_decl = parser
  | [< ids=id_list; 'Token.Operator ":"; t=type_id >] ->
      List.map (fun id -> {Type.name=id; Type.t=t} ) ids

and id_list = 
  let rec ids acc = parser
    | [< 'Token.Operator ","; 'Token.Id id; e=ids (id::acc) >] -> e
    | [< >] -> acc in
  parser
    | [< 'Token.Id id; stream >] -> List.rev (ids [id] stream)

and array_dimensions = 
  let rec aux acc = parser
    (* TODO: handle constants *)
    | [< 'Token.Operator ","; 'Token.Id _; e=aux (0::acc) >] -> e 
    | [< 'Token.Operator ","; 'Token.Int i; e=aux (i::acc) >] -> e
    | [< >] -> acc in
  parser
    (* TODO: handle constants *)
    | [< 'Token.Id _; stream >] -> List.rev (aux [0] stream)
    | [< 'Token.Int i; stream >] -> List.rev (aux [i] stream)

and type_id = parser
  | [< 'Token.Id name >] -> Type.Primitive name
  | [< 'Token.Keyword "array"; 'Token.Operator "["; 'Token.Id name; 
       'Token.Operator ","; dims=array_dimensions; 'Token.Operator "]" >] -> 
         Type.Array ((Type.Primitive name), dims)

let parse stream = 
  let root = Ast.Root {pipelines=[]; renderers=[]} in
  program root stream

