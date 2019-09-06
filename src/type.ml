(**
   Copyright 2019 Google LLC

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

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
  | SamplerCube
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
  | SamplerCube -> "samplerCube"
  | Texture n -> Printf.sprintf "texture%dD" n
  | RenderTarget RGB -> "rt_rgb"
  | RenderTarget RGBA -> "rt_rgba"
  | RenderTarget DS -> "rt_ds"
  | Atom Singleton -> "atom"
  | Atom List -> "atomlist"
  | Atom Set -> "atomset"

let is_ref = function TypeRef _ -> true | _ -> false

let is_record = function Record _ -> true | _ -> false

module Common = struct
  let bool = Primitive Bool

  let int = Primitive Int

  let uint = Primitive UInt

  let float = Primitive Float

  let double = Primitive Double

  let bvec2 = Vector (Bool, 2)

  let bvec3 = Vector (Bool, 3)

  let bvec4 = Vector (Bool, 4)

  let ivec2 = Vector (Int, 2)

  let ivec3 = Vector (Int, 3)

  let ivec4 = Vector (Int, 4)

  let uvec2 = Vector (UInt, 2)

  let uvec3 = Vector (UInt, 3)

  let uvec4 = Vector (UInt, 4)

  let fvec2 = Vector (Float, 2)

  let fvec3 = Vector (Float, 3)

  let fvec4 = Vector (Float, 4)

  let dvec2 = Vector (Double, 2)

  let dvec3 = Vector (Double, 3)

  let dvec4 = Vector (Double, 4)

  let bmat2 = Matrix (Bool, 2, 2)

  let bmat3 = Matrix (Bool, 3, 3)

  let bmat4 = Matrix (Bool, 4, 4)

  let bmat2x2 = Matrix (Bool, 2, 2)

  let bmat2x3 = Matrix (Bool, 2, 3)

  let bmat2x4 = Matrix (Bool, 2, 4)

  let bmat3x2 = Matrix (Bool, 3, 2)

  let bmat3x3 = Matrix (Bool, 3, 3)

  let bmat3x4 = Matrix (Bool, 3, 4)

  let bmat4x2 = Matrix (Bool, 4, 2)

  let bmat4x3 = Matrix (Bool, 4, 3)

  let bmat4x4 = Matrix (Bool, 4, 4)

  let imat2 = Matrix (Int, 2, 2)

  let imat3 = Matrix (Int, 3, 3)

  let imat4 = Matrix (Int, 4, 4)

  let imat2x2 = Matrix (Int, 2, 2)

  let imat2x3 = Matrix (Int, 2, 3)

  let imat2x4 = Matrix (Int, 2, 4)

  let imat3x2 = Matrix (Int, 3, 2)

  let imat3x3 = Matrix (Int, 3, 3)

  let imat3x4 = Matrix (Int, 3, 4)

  let imat4x2 = Matrix (Int, 4, 2)

  let imat4x3 = Matrix (Int, 4, 3)

  let imat4x4 = Matrix (Int, 4, 4)

  let umat2 = Matrix (UInt, 2, 2)

  let umat3 = Matrix (UInt, 3, 3)

  let umat4 = Matrix (UInt, 4, 4)

  let umat2x2 = Matrix (UInt, 2, 2)

  let umat2x3 = Matrix (UInt, 2, 3)

  let umat2x4 = Matrix (UInt, 2, 4)

  let umat3x2 = Matrix (UInt, 3, 2)

  let umat3x3 = Matrix (UInt, 3, 3)

  let umat3x4 = Matrix (UInt, 3, 4)

  let umat4x2 = Matrix (UInt, 4, 2)

  let umat4x3 = Matrix (UInt, 4, 3)

  let umat4x4 = Matrix (UInt, 4, 4)

  let fmat2 = Matrix (Float, 2, 2)

  let fmat3 = Matrix (Float, 3, 3)

  let fmat4 = Matrix (Float, 4, 4)

  let fmat2x2 = Matrix (Float, 2, 2)

  let fmat2x3 = Matrix (Float, 2, 3)

  let fmat2x4 = Matrix (Float, 2, 4)

  let fmat3x2 = Matrix (Float, 3, 2)

  let fmat3x3 = Matrix (Float, 3, 3)

  let fmat3x4 = Matrix (Float, 3, 4)

  let fmat4x2 = Matrix (Float, 4, 2)

  let fmat4x3 = Matrix (Float, 4, 3)

  let fmat4x4 = Matrix (Float, 4, 4)

  let dmat2 = Matrix (Double, 2, 2)

  let dmat3 = Matrix (Double, 3, 3)

  let dmat4 = Matrix (Double, 4, 4)

  let dmat2x2 = Matrix (Double, 2, 2)

  let dmat2x3 = Matrix (Double, 2, 3)

  let dmat2x4 = Matrix (Double, 2, 4)

  let dmat3x2 = Matrix (Double, 3, 2)

  let dmat3x3 = Matrix (Double, 3, 3)

  let dmat3x4 = Matrix (Double, 3, 4)

  let dmat4x2 = Matrix (Double, 4, 2)

  let dmat4x3 = Matrix (Double, 4, 3)

  let dmat4x4 = Matrix (Double, 4, 4)

  let sampler1D = Sampler 1

  let sampler2D = Sampler 2

  let sampler3D = Sampler 3

  let samplerCube = SamplerCube

  let texture1D = Texture 1

  let texture2D = Texture 2

  let texture3D = Texture 3

  let rt_rgb = RenderTarget RGB

  let rt_rgba = RenderTarget RGBA

  let rt_ds = RenderTarget DS

  let atom = Atom Singleton

  let atomlist = Atom List

  let atomset = Atom Set
end
