(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Dataflow module for use with the ocamlgraph library
*)


(** the types of graphs we work on *)
module type G =
  sig
    type t
    module V : Graph.Sig.COMPARABLE
    val pred : t -> V.t -> V.t list
    val succ : t -> V.t -> V.t list
    val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  end

(** dataflow directions *)
type direction = Forward | Backward


(** Dataflow doesn't need a full latice, but we do need this much.
    http://en.wikipedia.org/wiki/Meet-semilattice
 *)
module type BOUNDED_MEET_SEMILATTICE =
sig
  
  (** The type of a latice element *)
  type t
    
  (** Top of the latice *)
  val top : t
    
  (** The meet operator.
      [meet v1 v2] should form a lattice. in particular,
      remember that meet v1 Top = meet Top v1 = v1, 
      and meet Bottom _ = meet _ Bottom = Bottom
  *)
  val meet : t -> t -> t

  (** Equality checking for the latice.
      Returns true when the two latice elements are the same.
  *)
  val equal : t -> t -> bool
end

(** A dataflow problem is defined by a lattice over a graph. *)
module type DATAFLOW =
  sig

    module L : BOUNDED_MEET_SEMILATTICE
    module G : G

    (** The transfer function over node elements, e.g., statements *)
    val transfer_function : G.t -> G.V.t -> L.t -> L.t
      
    (** the starting node for the analysis *)
    val s0 : G.V.t

    (** the initial value for analysis. This is what s0 should start
	out with. All other nodes start out with Top *)
    val init : L.t

    (** the dataflow direction *)
    val dir : direction
  end

(** Make(DATAFLOW) returns a worklist algorithm, which when applied to
    a graph, returns a pair of functions. The first function f1 is given
    v,  returns in[v].  The function f2 is for v, return out[v] *)
module Make :
  functor (D : DATAFLOW) ->
    sig
      val worklist_iterate : ?init:D.L.t ->
        D.G.t -> (D.G.V.t -> D.L.t) * (D.G.V.t -> D.L.t)
      val topological_worklist_iterate : ?init:D.L.t ->
        D.G.t -> (D.G.V.t -> D.L.t) * (D.G.V.t -> D.L.t)

    end

(** [worklist_iterate nodes entry  f init pred top meet] will return a
    pair of functions in(B) and out(B) defined for all B in [nodes]. 
    
    This function works in the forward direction. (swap the pred and succ
    arguments to work backwards)
    @param f_t  takes in a  node ('n) and whatever you are computing
    ('l), and returns a new whatever you are computing.
*)
val worklist_iterate :
  nodes:'a list ->
  entry:'a ->
  f_t:('a -> 'b -> 'b) ->
  init:'b ->
  pred:('a -> 'a list) ->
  succ:('a -> 'a list) ->
  top:'b ->
  meet:('b -> 'b -> 'b) -> 
  eq:('b -> 'b -> bool) -> 
      ('a -> 'b) * ('a -> 'b)

val topological_worklist_iterate :
  nodes:'a list ->
  entry:'a ->
  f_t:('a -> 'b -> 'b) ->
  init:'b ->
  pred:('a -> 'a list) ->
  succ:('a -> 'a list) ->
  top:'b ->
  meet:('b -> 'b -> 'b) -> 
  eq:('b -> 'b -> bool) -> 
      ('a -> 'b) * ('a -> 'b)
	

val worklist_iterate_in :
  nodes:'a list ->
  entry:'a ->
  f_t:('a -> 'b -> 'b) ->
  init:'b ->
  pred:('a -> 'a list) ->
  succ:('a -> 'a list) ->
  top:'b -> meet:('b -> 'b -> 'b) -> eq:('b -> 'b -> bool) -> 'a -> 'b
val worklist_iterate_out :
  nodes:'a list ->
  entry:'a ->
  f_t:('a -> 'b -> 'b) ->
  init:'b ->
  pred:('a -> 'a list) ->
  succ:('a -> 'a list) ->
  top:'b -> meet:('b -> 'b -> 'b) -> eq:('b -> 'b -> bool) -> 'a -> 'b
