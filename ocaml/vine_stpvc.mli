(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

type ctx
type revctx

val typ_to_stp : Stpvc.vc -> Vine.typ -> Stpvc.typ
val empty_ctx : unit -> ctx
val new_ctx : Stpvc.vc -> Vine.decl list -> ctx
val rev_ctx : ctx -> revctx
val vine_to_stp : Stpvc.vc -> ctx -> Vine.exp -> Stpvc.exp
val stp_to_type : Stpvc.exp -> Vine.typ
val stp_to_vine : ?strip_nums:bool -> ?ctx:revctx -> Stpvc.exp -> Vine.exp

val vc_simplify : Stpvc.vc -> ctx -> Vine.exp -> Vine.exp
