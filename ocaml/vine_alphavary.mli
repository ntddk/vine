(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(**
   Code for alpha-varying and descoping programs.
*)


(** class used for renaming variables for both alpha-varying and
    descoping *)
class var_renamer :
  object
    val ctx : (int, Vine.var) Hashtbl.t
    method alpha_vary_decl : Vine.decl -> Vine.var
    method alpha_vary_decls : Vine.decl list -> Vine.decl list
    method exit_decl : Vine.decl -> unit
    method exit_decls : Vine.decl list -> unit
    method init_ctx : Vine.decl list -> unit
    method ctx_to_lst : unit -> Vine.decl list 
    method visit_alvalue : Vine.lvalue -> Vine.lvalue Vine.visit_action
    method visit_binding :
      Vine.lvalue * Vine.exp -> (Vine.lvalue * Vine.exp) Vine.visit_action
    method visit_decl : Vine.decl -> Vine.decl Vine.visit_action
    method visit_exp : Vine.exp -> Vine.exp Vine.visit_action
    method visit_rlvalue : Vine.lvalue -> Vine.lvalue Vine.visit_action
    method visit_stmt : Vine.stmt -> Vine.stmt Vine.visit_action
  end



(** alpha-vary a single expression *)
val alpha_vary_exp : Vine.exp -> Vine.exp

(** alpha-vary a single statement *)
val alpha_vary_stmt : Vine.stmt -> Vine.stmt

(** alpha-vary a sequence of statements. *)
val alpha_vary_stmts : Vine.stmt list -> Vine.stmt list

(** alpha-vary a complete program, except for globals *)
val alpha_vary_program : Vine.program -> Vine.program

(** remove all scoping from a program. Note variable names will be
    globally unique. 
*)
val descope_program : Vine.program -> Vine.program

(** descope a single function. *)
val descope_function : Vine.stmt -> Vine.stmt

val descope_stmts : var_renamer -> Vine.stmt list -> Vine.program 
