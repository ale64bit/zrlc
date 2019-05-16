type const =
  | OfBool of bool
  | OfInt of int
  | OfFloat of float
  | OfName of string

type primitive_type = Bool | Int | Float

type primitive

type basic

type compound

type _ t =
  | TypeRef : string -> basic t
  | Record : (string * basic t) list -> compound t
  | Array : basic t * const list -> basic t
  | Function : (string * basic t) list * basic t list -> compound t
  | Primitive : primitive_type -> primitive t

let rec string_of_t : type a. a t -> string = function
  | TypeRef name ->
      Printf.sprintf "TypeRef %s" name
  | Record fields ->
      let xs =
        String.concat "; "
          (List.map
             (fun (id, t) -> Printf.sprintf "%s: %s" id (string_of_t t))
             fields)
      in
      Printf.sprintf "Record {%s}" xs
  | Array _ ->
      "Array"
  | Function _ ->
      "Function"
  | Primitive Bool ->
      "Primitive Bool"
  | Primitive Int ->
      "Primitive Int"
  | Primitive Float ->
      "Primitive Float"

let () =
  let a =
    Record
      [ (**************************************************************)
        ("x", TypeRef "int")
      ; ("y", Array (TypeRef "float", [OfInt 2]))
      ; ("z", Record []) ]
  in
  (*   let b = Record [] in *)
  print_endline (string_of_t a)
