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

  val name : t -> string

  val params : t -> (string * string) list

  val template_params : t -> string list

  val return_type : t -> string

  val is_template : t -> bool

  val set_return_type : string -> t -> t

  val set_parent_class : string -> t -> t

  val add_param : string * string -> t -> t

  val add_template_param : string -> t -> t

  val add_member_initializer : string * string -> t -> t

  val append_code_section : string -> t -> t

  val append_code_sections : string list -> t -> t

  val prepend_code_section : string -> t -> t

  val prepend_code_sections : string list -> t -> t

  val string_of_signature : t -> string

  val string_of_implementation : t -> string
end

module Class : sig
  type t

  val empty : string -> t

  val name : t -> string

  val private_functions : t -> Function.t list

  val add_include : string -> t -> t

  val add_public_function : Function.t -> t -> t

  val add_private_function : Function.t -> t -> t

  val add_private_member : string * string -> t -> t

  val add_static_section : string -> t -> t

  val string_of_header : t -> string

  val string_of_source : t -> string
end

module Header : sig
  type t

  val empty : string -> t

  val name : t -> string

  val add_include : string -> t -> t

  val add_section : string -> t -> t

  val string_of_header : t -> string
end

module Library : sig
  type t

  val empty : string -> t

  val classes : t -> Class.t list

  val headers : t -> Header.t list

  val set_copts : string -> t -> t

  val set_defines : string -> t -> t

  val add_class : Class.t -> t -> t

  val add_header : Header.t -> t -> t
end
