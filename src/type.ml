type t =
  | Unit
  | TypeRef of string
  | Record of (string * t) list
  | Array of t * const list
  | Function of (string * t) list * t list
  | Primitive of primitive_type
  | Vector of primitive_type * int
  | Matrix of primitive_type * int * int
  | Sampler of int
  | Texture of int
  | RenderTarget of render_target_type
  | Atom of atom_type
[@@deriving to_yojson]

and primitive_type = Bool | Int | UInt | Float | Double
[@@deriving to_yojson]

and render_target_type = RGB | RGBA | DS [@@deriving to_yojson]

and atom_type = Singleton | List | Set [@@deriving to_yojson]

and const =
  | OfBool of bool
  | OfInt of int
  | OfFloat of float
  | OfName of string
[@@deriving to_yojson]

let rec string_of_type = function
  | Unit -> "()"
  | TypeRef name -> Printf.sprintf "%s" name
  | Record fields ->
      let field_strs =
        List.map
          (fun (name, t) -> Printf.sprintf "%s: %s" name (string_of_type t))
          fields
      in
      "record { " ^ String.concat "; " field_strs ^ " }"
  | Array (t, dims) ->
      let dim_strs =
        List.map
          (function
            | OfBool b -> string_of_bool b
            | OfInt i -> string_of_int i
            | OfFloat f -> string_of_float f
            | OfName name -> name)
          dims
      in
      Printf.sprintf "[%s]%s" (String.concat "; " dim_strs) (string_of_type t)
  | Function (args, rets) ->
      let arg_strs =
        List.map
          (fun (name, t) -> Printf.sprintf "%s: %s" name (string_of_type t))
          args
      in
      let ret_strs = List.map string_of_type rets in
      Printf.sprintf "fun (%s) -> (%s)"
        (String.concat ", " arg_strs)
        (String.concat ", " ret_strs)
  | Primitive Bool -> "bool"
  | Primitive Int -> "int"
  | Primitive UInt -> "uint"
  | Primitive Float -> "float"
  | Primitive Double -> "double"
  | Vector (Bool, n) -> Printf.sprintf "bvec%d" n
  | Vector (Int, n) -> Printf.sprintf "ivec%d" n
  | Vector (UInt, n) -> Printf.sprintf "uvec%d" n
  | Vector (Float, n) -> Printf.sprintf "fvec%d" n
  | Vector (Double, n) -> Printf.sprintf "dvec%d" n
  | Matrix (Bool, m, n) -> Printf.sprintf "bmat%dx%d" m n
  | Matrix (Int, m, n) -> Printf.sprintf "imat%dx%d" m n
  | Matrix (UInt, m, n) -> Printf.sprintf "umat%dx%d" m n
  | Matrix (Float, m, n) -> Printf.sprintf "fmat%dx%d" m n
  | Matrix (Double, m, n) -> Printf.sprintf "dmat%dx%d" m n
  | Sampler n -> Printf.sprintf "sampler%dD" n
  | Texture n -> Printf.sprintf "texture%dD" n
  | RenderTarget RGB -> "rt_rgb"
  | RenderTarget RGBA -> "rt_rgba"
  | RenderTarget DS -> "rt_ds"
  | Atom Singleton -> "atom"
  | Atom List -> "atomlist"
  | Atom Set -> "atomset"

let is_ref = function TypeRef _ -> true | _ -> false
