(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

module type DEBUG =
  sig
    val debug : bool
    val warn : bool
    val pdebug : string -> unit
    val dprintf : ('a, unit, string, unit) format4 -> 'a
    val dtrace :
      before:('a -> unit) -> f:('a -> 'b) -> after:('b -> unit) -> 'a -> 'b
    val pwarn : string -> unit
    val wprintf : ('a, unit, string, unit) format4 -> 'a
  end
module type DEBUG_MOD_INFO =
sig
  val name : string
  val default : [ `Debug | `NoDebug ]
end
module NoDebug : DEBUG
module Make : functor (Module : DEBUG_MOD_INFO) -> DEBUG
