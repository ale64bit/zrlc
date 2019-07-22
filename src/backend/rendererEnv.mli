open Cpp

module MapString : Map.S with type key = string

type render_target_type = Color | DepthStencil

type t = {
  name : string;
  render_targets : render_target_type MapString.t;
  rclass : Class.t;
  render : Function.t;
  ctor : Function.t;
  dtor : Function.t;
}

val empty : string -> string list -> t

val export : t -> Class.t
