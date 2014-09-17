(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

val calculate_wp : (Gcl.exp -> Gcl.exp) -> Gcl.exp -> Gcl.t -> Gcl.exp
val dijkstra_wp : (Gcl.exp -> Gcl.exp) -> Gcl.exp -> Gcl.t -> Gcl.exp
val calculate_wlp : (Gcl.exp -> Gcl.exp) -> Gcl.exp -> Gcl.t -> Gcl.exp
val calculate_wp_ssa :
  (Vine.exp -> Vine.exp) -> Vine.exp -> Gcl.gcl -> Vine.exp
val leino_wp : (Gcl.exp -> Gcl.exp) -> Gcl.exp -> Gcl.t -> Vine.exp
val simp_skip : ('a -> 'b) -> int -> ('a -> 'b) -> 'a -> 'b
val simp_debug : ('a -> unit) -> ('b -> unit) -> ('a -> 'b) -> 'a -> 'b
val simp_print : (Vine.exp -> Vine.exp) -> Vine.exp -> Vine.exp

val globalize_wp : Vine.exp -> Vine.var list * Vine.exp
