(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(**)
exception SsaError of string

type label = Vine.label
type typ = Vine.typ
type cast_type = Vine.cast_type
type binop_type = Vine.binop_type
type unop_type = Vine.unop_type
type var = Vine.var
type decl = Vine.decl
type value =
    Int of int64 * typ
  | Str of string
  | Lval of lvalue
  | Name of label
and lvalue = var
type exp =
    BinOp of binop_type * value * value
  | UnOp of unop_type * value
  | Val of value
  | Cast of cast_type * typ * value
  | Unknown of string
  | Get of value * value * typ
  | Set of value * value * value * typ
  | Phi of var list
type stmt =
    Jmp of value
  | CJmp of value * value * value
  | Move of lvalue * exp
  | Label of label
  | Comment of string
  | Return of value option
  | Call of lvalue option * value * value list
  | Assert of value 
  | Halt of value
  | Attr of stmt * Vine.attribute

(** Visiting works the same as in [Vine]
    See {!Vine.visit_action} for more information.*)
type 'a visit_action = 'a Vine.visit_action


(** {6 Visitors } *)

(** visiting works the same as in vine.
    See {!Vine.visitor} for more information.*)
class type ssa_visitor =
object
  method visit_decl : decl -> decl visit_action
  method visit_exp : exp -> exp visit_action
  method visit_stmt : stmt -> stmt visit_action
  method visit_value : value -> value visit_action
  method visit_rvar : var -> var visit_action
  method visit_avar : var -> var visit_action
end
(** A nop visitor similar to [nop_vine_visitor].
    See {!Vine.nop_vine_visitor} for more information.   *)
class nop_ssa_visitor : ssa_visitor


(** The following are just like in {!Vine} but for SSA stmts *)

val exp_accept : #ssa_visitor -> exp -> exp
val rvar_accept : #ssa_visitor -> var -> var
val avar_accept : #ssa_visitor -> var -> var
val value_accept : #ssa_visitor -> value -> value
val decl_accept : #ssa_visitor -> decl -> decl
val stmt_accept : #ssa_visitor -> stmt -> stmt
val stmts_accept : #ssa_visitor -> stmt list -> stmt list

  (** Apply a visitor to a CFG.
      Modifies the CFG in place.
      The order in wich the basic blocks will be visited is unspecified.
  *)
val cfg_accept : #ssa_visitor -> stmt list #Vine_cfg.cfg -> unit



(** {6 Pretty printing} *)

val pp_type : (string -> unit) -> Vine.typ -> unit
val pp_value : (string -> unit) -> value -> unit
val value_to_string : value -> string
val pp_lval : (string -> unit) -> lvalue -> unit
val pp_exp : (string -> unit) -> exp -> unit
val pp_stmt : (string -> unit) -> stmt -> unit

val stmt_to_string : stmt -> string
val exp_to_string : exp -> string
val stmt_printer : stmt list #Vine_cfg.cfg -> stmt list Vine_cfg.bb -> string


(** {6 More stuff } *)

val newvar : string -> typ -> var
val newssavar : Vine.var -> var

val cfg2ssa : Vine.stmt list Vine_cfg.cfg -> stmt list Vine_cfg.cfg
val trans_cfg : Vine.stmt list Vine_cfg.cfg ->
  stmt list Vine_cfg.cfg * (Vine.var -> Vine.var) * (Vine.var -> Vine.var)

module G : Vine_cfg.GSIG with type t = stmt list Vine_cfg.cfg
module StmtG : Vine_cfg.GSIG with type t = stmt Vine_cfg.cfg

val rm_phis : stmt list Vine_cfg.cfg -> unit


val cfg2vine : stmt list Vine_cfg.cfg -> Vine.stmt list Vine_cfg.cfg

(** like cfg2vine but also sets the variable declarations correctly,
    and calls rm_phis for you *)
val to_vine : stmt list Vine_cfg.cfg -> Vine.stmt list Vine_cfg.cfg

(** [exp_to_vine e] returns a Vine.exp equiv. to the ssa exp [e] *)
val exp2vine : exp -> Vine.exp

val ssalist_to_ssa : stmt list Vine_cfg.cfg -> stmt list Vine_cfg.cfg 

(** Simplify an expression. In SSA, this is almost trivial where we
    simply constant binops and unops *)
val simplify_exp : exp -> exp

(** Simplify a statement *)
val simplify_stmt : stmt -> stmt
module NP : Vine_cfg.NodePartition.NP with type t = G.t and type vertex = G.V.t

(** Remove unreachable nodes. See {!Vine_cfg.remove_unreachable}. *)
val remove_unreachable : ?from:Vine_cfg.bbid -> stmt list Vine_cfg.cfg -> unit

(** Check that operations on this CFG will be well defined.
    See {!Vine_cfg.well_defined}.
*)
val well_defined : stmt list Vine_cfg.cfg -> bool

val ssa_iter_labels : (label -> unit) -> stmt -> unit
val ssa_list_iter_labels : (label -> unit) -> stmt list -> unit

(** Returns a list of variables that are used uninitialized in the CFG *)
val uninitialized_vars : stmt list #Vine_cfg.cfg -> var list

val trace_ssa : Vine.program -> Vine.program
