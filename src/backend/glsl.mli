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

module Function : sig
  type t

  val empty : string -> t

  val set_return_type : string -> t -> t

  val add_in_param : string * string -> t -> t

  val add_out_param : string * string -> t -> t

  val add_inout_param : string * string -> t -> t

  val append_code_section : string -> t -> t

  val append_code_sections : string list -> t -> t

  val prepend_code_section : string -> t -> t

  val prepend_code_sections : string list -> t -> t

  val string_of_implementation : t -> string
end

module Shader : sig
  type t

  type stage = Vertex | Geometry | Fragment | Compute

  val empty : string -> stage -> t

  val name : t -> string

  val stage : t -> stage

  val add_constant : string -> t -> t

  val add_struct : string -> t -> t

  val add_uniform : int -> int -> string * string -> t -> t

  val add_input : int -> string * string -> t -> t

  val add_output : int -> string * string -> t -> t

  val add_function : Function.t -> t -> t

  val string_of_source : t -> string
end

module Library : sig
  type t

  val empty : string -> t

  val name : t -> string

  val shaders : t -> Shader.t list

  val add_shader : Shader.t -> t -> t
end
