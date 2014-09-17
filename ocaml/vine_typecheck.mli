(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(**
   Type inference and typechecker for the VinE IR.
*)

(** The type used for the typechecking context. *)
type ctx

(** The type for optional contexts *)
type gamma = ctx option

(** [tint t] true iff [t] is an integer type *)
val tint : Vine.typ -> bool

(** [tcompat t1 t2] true if t1 compatible with t2, e.g., only differ
  on attributes. *)
val tcompat : Vine.typ -> Vine.typ -> bool

(** create a new persistent typing context *)
val gamma_create : unit -> gamma

(** Extend the typing context and returns a new
    persistent context. *)
val gamma_extend :
  gamma -> Vine.var -> Vine.typ -> gamma

(** [gamma_find gamma v] returns Some(t) where [t] is the type of [v]
    if [v] is in the context, else None *)
val gamma_find : gamma -> Vine.var -> Vine.typ option

(** [gamma_check gamma v t] returns t if v is of type [t] in gamma,
    error if [v] is not of type [t], AND returns [t] if [v] is not in
    gamma. Note this last part is what makes it not just a boolean
    test.  *)
val gamma_check : gamma -> Vine.var -> Vine.typ -> Vine.typ

(** typecheck a value. no context needed. *)
val typecheck_value : Vine.value -> Vine.typ

(** [typecheck_lval names gamma lv] typechecks the lv under
    gamma. [names] is a symbol table of names used. Returns a new
    names and the type of [lv] *)
val typecheck_lval :
  (Vine.label, 'a option) Symbols.t ->
  gamma ->
  Vine.lvalue -> (Vine.label, 'a option) Symbols.t * Vine.typ

(** [typecheck_exp names gamma e] typechecks the [e] under
    gamma. [names] is a symbol table of names used. Returns a new
    names and the type of [e] *)
val typecheck_exp :
  (Vine.label, 'a option) Symbols.t ->
  gamma ->
  Vine.exp -> (Vine.label, 'a option) Symbols.t * Vine.typ

(** [typecheck_stmt scope omega names labels sigma gamma s] typechecks
    statement s. [scope] is the current scope number, used to make
    sure of things like we don't declare functions outside the global
    scope.  [omega] is the return type option of the current
    function.  [names] are referenced Names.  [labels] are defined
    labels. Typechecking can make sure that all [names] are to real
    [labels].  [sigma] is our function typing context, and [gamma] is
    our variable typing context *)
val typecheck_stmt :
  int ->
  Vine.typ option ->
  (Vine.label, 'a option) Symbols.t ->
  (Vine.label, 'a option) Symbols.t ->
  (Vine.label, Vine.typ) Symbols.t ->
  gamma ->
  Vine.stmt ->
  (Vine.label, 'a option) Symbols.t * (Vine.label, 'a option) Symbols.t *
  (Vine.label, Vine.typ) Symbols.t * gamma

(** typecheck a program. *)
val typecheck : Vine.program -> unit

(** [infer_type gamma e] infers the type of [e] under [gamma]. [gamma]
    can be None *)
val infer_type : gamma -> Vine.exp -> Vine.typ
