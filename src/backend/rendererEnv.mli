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
  before_recording : string list;
  outside_render_passes : string list;
  inside_render_passes : string list;
  after_recording : string list;
}

val empty : string -> string list -> t

val add_stmt_before_recording : string -> t -> t

val add_stmt_outside_render_passes : string -> t -> t

val add_stmt_inside_render_passes : string -> t -> t

val add_stmt_after_recording : string -> t -> t

val export : t -> Class.t
