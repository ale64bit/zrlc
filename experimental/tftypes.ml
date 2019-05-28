(* tagless final types *)

module type Types = sig
  type 'dv repr

  type basic

  type compound

  val name : string -> basic repr

  val arr : int -> basic repr -> basic repr

  val record : (string * basic repr) list -> compound repr

  val func : (string * basic repr) list -> basic repr -> compound repr
end

module Examples (T : Types) = struct
  open T

  let t1 = name "int"

  let t2 = arr 4 (name "float")

  let t3 = arr 4 (arr 8 (name "bool"))

  let t4 = record [("x", name "int")]

  (* Does not compile *)
  (* let t5 = arr 2 (record [("y", name "float")]) *)

  let t6 = func [("a", name "float")] (arr 2 (name "int"))

  (* Does not compile *)
  (* let t7 = func [("b", t6)] (name "void") *)
end

module Print = struct
  type 'dv repr = 'dv

  type basic = string

  type compound = string

  let name s = s

  let arr n t = Printf.sprintf "[%d]%s" n t

  let record fields =
    let fs = List.map (fun (id, t) -> Printf.sprintf "%s: %s" id t) fields in
    Printf.sprintf "record {%s}" (String.concat "; " fs)

  let func args ret =
    let args = List.map (fun (id, t) -> Printf.sprintf "%s: %s" id t) args in
    Printf.sprintf "func (%s): %s" (String.concat ", " args) ret
end

module Sizeof = struct
  type 'dv repr = 'dv

  type basic = int

  type compound = int

  let name _ = 1

  let arr n t = n * t

  let record fields = List.fold_left (fun acc (_, t) -> acc + t) 0 fields

  let func _ _ = 0
end

module EP = Examples (Print)
module ES = Examples (Sizeof)

let () =
  (* Print *)
  print_endline EP.t1 ;
  print_endline EP.t2 ;
  print_endline EP.t3 ;
  print_endline EP.t4 ;
  print_endline EP.t6 ;
  (* Sizeof *)
  print_endline (string_of_int ES.t1) ;
  print_endline (string_of_int ES.t2) ;
  print_endline (string_of_int ES.t3) ;
  print_endline (string_of_int ES.t4) ;
  print_endline (string_of_int ES.t6)
