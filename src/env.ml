module SymbolTable = Map.Make(String)

type scope = {name: string; symbols: Type.t SymbolTable.t}
type env = {scopes: scope list}

let empty = {scopes=[]}

let enter_scope name env = 
  let new_scope = {name=name; symbols=SymbolTable.empty} in
  {
    scopes = new_scope :: env.scopes; 
  }

let current_scope env =
  let rec aux acc = function
    | hd :: tl -> aux ("::" ^ hd.name ^ acc) tl
    | [] -> acc in
  aux "" env.scopes

let exit_scope env =
  {
    scopes = List.tl env.scopes; 
  }

let get symbol env =
  let rec aux l = 
    match SymbolTable.find_opt symbol (List.hd l).symbols with
    | Some t -> t
    | None -> aux (List.tl l) in
  aux env.scopes

let put symbol t env =
  let current_head = List.hd env.scopes in
  let updated_head = {name=current_head.name; symbols=(SymbolTable.add symbol t current_head.symbols)} in
  {
    scopes = updated_head :: (List.tl env.scopes);
  }
