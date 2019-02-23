module SymbolTable = Map.Make(String)

type const_value = 
  | Int of int
  | Float of float
  | Bool of bool

type t = {
  id: string;
  types: Type.t SymbolTable.t;
  constants: const_value SymbolTable.t;
  vars: Type.t SymbolTable.t;
}

let empty id = 
  {
    id = id; 
    types = SymbolTable.empty;
    constants = SymbolTable.empty;
    vars = SymbolTable.empty;
  }

