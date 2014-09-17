(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

type 'a loop
type combine_type = KeepDisjoint | UniqueHeader

module LOOP :functor (Lang : sig type t end) ->
sig
  val make_reducible : 
    (Lang.t Vine_cfg.cfg -> 
      (Lang.t Vine_cfg.bb, Lang.t Vine_cfg.bb) Hashtbl.t -> unit) -> 
    Lang.t Vine_cfg.cfg -> unit

  val get_reducible : 
    (Lang.t Vine_cfg.cfg -> 
      (Lang.t Vine_cfg.bb, Lang.t Vine_cfg.bb) Hashtbl.t -> unit) -> 
    (Lang.t Vine_cfg.cfg as 'b) -> 'b

  val get_loops : ?combine_method:combine_type -> Lang.t Vine_cfg.cfg -> 
    Lang.t loop list
    
  val get_loops_from_bb : ?combine_method:combine_type -> 
    Lang.t Vine_cfg.cfg -> Lang.t Vine_cfg.bb -> Lang.t loop list
    
  val unroll_loop_by_backedges :
    ((Lang.t Vine_cfg.cfg as 'a) -> ((Lang.t Vine_cfg.bb as 'b), 'b) Hashtbl.t
      -> unit) ->
    ('a -> ('b * 'b) list -> ('b, 'b) Hashtbl.t -> unit) ->
    'a -> 'b list -> ('b * 'b) list -> int -> unit
    
  val unroll_loop :
    ((Lang.t Vine_cfg.cfg as 'a) -> ((Lang.t Vine_cfg.bb as 'b), 'b) Hashtbl.t
      -> unit) ->
    ('a -> ('b * 'b) list -> ('b, 'b) Hashtbl.t -> unit) ->
    'a -> Lang.t loop -> int -> unit
    
end

val get_loop_header : 'a loop -> 'a Vine_cfg.bb
  
val get_loop_body : 'a loop -> 'a Vine_cfg.bb list
    
val get_loop_back_nodes : 'a loop -> 'a Vine_cfg.bb list
  
val get_loop_exit_edges : 'a loop -> 'a #Vine_cfg.cfg ->
  ('a Vine_cfg.bb * 'a Vine_cfg.bb) list
    
val get_inner_loops : 'a loop -> 'a #Vine_cfg.cfg -> 'a loop list
  
val get_outer_loops : 'a loop -> 'a #Vine_cfg.cfg -> 'a loop list
  
val pp_loop : (string -> unit) -> 'a loop -> unit
  
val make_reducible : Vine.stmt list Vine_cfg.cfg -> unit
  
val get_reducible : (Vine.stmt list Vine_cfg.cfg as 'b) -> 'b
  
val get_loops : ?combine_method:combine_type -> 
  Vine.stmt list Vine_cfg.cfg -> Vine.stmt list loop list
  
val get_loops_from_bb : ?combine_method:combine_type -> 
  Vine.stmt list Vine_cfg.cfg -> 
  Vine.stmt list Vine_cfg.bb -> Vine.stmt list loop list
    
val unroll_loop_by_backedges :
  ?fwd_jmps:(Vine.stmt list Vine_cfg.cfg -> 
    Vine_cfg.label -> Vine_cfg.label * Vine.stmt list Vine_cfg.bb) ->
  Vine.stmt list Vine_cfg.cfg -> 
  Vine.stmt list Vine_cfg.bb list -> 
  (Vine.stmt list Vine_cfg.bb * Vine.stmt list Vine_cfg.bb) list ->
    int ->
  unit
      
val unroll_loop :
    ?fwd_jmps:(Vine.stmt list Vine_cfg.cfg -> 
      Vine_cfg.label -> Vine_cfg.label * Vine.stmt list Vine_cfg.bb) ->
  Vine.stmt list Vine_cfg.cfg -> Vine.stmt list loop -> int -> unit
  
