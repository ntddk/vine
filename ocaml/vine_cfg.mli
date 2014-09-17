(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(**
   A Control Flow Graph for Vine programs.

*)

(** Types *)

exception DataflowError of string

type bbid =
    BB_Entry
  | BB_Exit
  | BB_Indirect
  | BB_Error
  | BB of int


(** The abstract basic block type. It is only meaningful in conjunction with
    the CFG it belongs to. *)
type 'a bb

type label = Vine.label

(** The control flow graph type. Note that all methods that take a bb
    require that it belong to this graph. Using a bb from a different graph
    will result in undefined behaviour. All methods that return a bb return
    one belonging to this CFG. *)
class type ['a] cfg =
object ('b)
  method find : bbid -> 'a bb
    (** Find the BB corresponding to the given bbid *)

  method find_label : label -> 'a bb
    (** Find the BB which contains the given label *)

  method add_edge : 'a bb -> 'a bb -> unit
    (** [cfg#add_edge a b] adds an edge in cfg from [a] to [b]. Behavior is
	undefined if there already was an edge from [a] to [b]. *)

  method remove_edge : 'a bb -> 'a bb -> unit
    (** [cfg#remove_edge a b] removes the edge from [a] to [b]. Behavior is
	undefined if no such edge was in the CFG. *)
    
  method has_edge : 'a bb -> 'a bb -> bool
    (** [cfg#has_edge a b] returns true when there is an edge from [a] to [b]
	in the CFG. *)

  method create_bb : bbid -> 'a -> 'a bb
    (** [cfg#create_bb id s] creates a new bb in the CFG, with bbid [id] and
	info [s]. Behavior is undefined when a bb with the given [id] already
	exists in the graph.
	@return the newly created bb. *)

  method add_bb : bbid -> 'a -> unit
    (** Like [create_bb], but doesn't return the new bb. *)

  method rename_bb : 'a bb -> bbid -> unit
    (** Changes the id of the given bb. Behaviour is undefined if the given
	bbid is already used. *)

  method remove_bb : 'a bb -> unit
    (** Removes a bb from the CFG. *)
    
  method iter_bb : ('a bb -> unit) -> unit
    (** [cfg#iter f] applies [f] to every bb in the CFG. The order in which
	bbs are passed to [f] is undefined. *)

  method fold_bb : 'b. ('a bb -> 'b -> 'b) -> 'b -> 'b
    (** [cfg#fold_bb f init] computes [f bN (...(f b1 init)...)] where
	[b1 ... bN] are all the bbs in the CFG. The ordering of the bbs is 
	unspecified. *)

  method iter_edges : ('a bb -> 'a bb -> unit) -> unit
    (** [cfg#iter_edges f] calls [f a b] forall [(a,b)] where there is an edge
	from [a] to [b] in the CFG.  *)

  method get_info : 'a bb -> 'a
    (** Returns the info associated with the given bb. *)

  method set_info : 'a bb -> 'a -> unit
    (** Sets the info associated with the given bb. *)

  method get_id : 'a bb -> bbid
    (** Returns the bbid for the given bb. This is the inverse of [find]. *)
    
  method succ : 'a bb -> 'a bb list
    (** Returns a list of successors of the given bb *)
    
  method pred : 'a bb -> 'a bb list
    (** Returns a list of predecessors of the given bb *)

  method iter_succ : ('a bb -> unit) -> 'a bb -> unit
    (** [cfg#iter_succ f b] applies [f] to all the successors of [b] *)

  method iter_pred : ('a bb -> unit) -> 'a bb -> unit
    (** [cfg#iter_succ f b] applies [f] to all the predecessors of [b] *)

  method fold_succ : 'b. ('a bb -> 'b -> 'b) -> 'a bb -> 'b -> 'b
    (** [cfg#fold_succ f b init] computes
	[f bN (...(f b1 init)...)] where [b1 ... bN] are the
	successors of [b]. *)

  method fold_pred : 'b. ('a bb -> 'b -> 'b) -> 'a bb -> 'b -> 'b
    (** [cfg#fold_succ f b init] computes
	[f bN (...(f b1 init)...)] where [b1 ... bN] are the
	predecessors of [b]. *)

  method length : int
    (** Returns the number of bbs that are in the CFG *)

  method newid : bbid
    (** Returns a new bbid. This means one which hasn't been returned by this
	method for this CFG yet. As a rule, all normal bbids used in this CFG
	should have been aquired through this method, so you can assume
	there won't be any collisions with this bbid. *)

  method copy : 'b
    (** Make a copy of this CFG *)

  method new_empty : 'b
    (** Create a new, empty CFG of the same type *)



    (** Warning: The following methods are for use inside vine_cfg and should
	not be used anywhere else. These interfaces will be changing when
	we figure out better solutions. *)

  method get_iter_labels_function : (label->unit) -> 'a -> unit
    (** Do not use! *)

  method vardecls : Vine.decl list
    (** Do not use! *)

  method set_vardecls : Vine.decl list -> unit
    (** Do not use! *)

end



(** Returns the string representation of a bbid. (for printing, etc.)*)
val bbid_to_string : bbid -> string



(* Do we want to expose these? *)
val vine_iter_labels : (Vine.label -> unit) -> Vine.stmt -> unit
val vine_list_iter_labels : (Vine.label -> unit) -> Vine.stmt list -> unit



val coalesce_bb : Vine.stmt list #cfg -> unit
  (** Coalesces the basic blocks in the given CFG.
      The interface to this might change in the future to support other types of
      CFGs. *)


val prog_to_cfg : Vine.program -> Vine.stmt list cfg
  (** Builds a CFG for the given Vine.program. *)
  
val func_to_cfg : Vine.stmt -> Vine.stmt list cfg
  (** Builds a CFG for the given function *)

val trace_to_cfg : Vine.program -> Vine.stmt list cfg
  (** Builds a CFG for the given trace. *)

val map :  (bbid -> 'a) -> 'b cfg ->
  ((Vine.label -> unit) -> 'a -> unit) -> 'a cfg
(** [map f cfg iter_labs] creates a new CFG where the info at each node
    is the data returned by [f]. *)

(** Translates a CFG back into a Vine.program.*)
val cfg_to_prog : Vine.stmt list #cfg -> bbid -> bbid -> Vine.program

val normal_cfg_to_prog : Vine.stmt list #cfg -> Vine.program

(** Adds adges, specified as a list of (src, dst) addresses, in the
    cfg. This list can be extracted from a dynamic trace elsewhere. **)
val add_indirect_jumps : Vine.stmt list cfg -> (int64 * int64) list -> unit 

(** The signature for the subset of Graph.Sig.G that we support.
    If you need something that is in ocamlgraph, please file a bug report.

    See http://ocamlgraph.lri.fr/doc/Sig.G.html for documentation.
*)
module type GSIG =
(* maybe someday... (* Graph.Sig.G with type V.t = bbid *) *)
sig
  module V :
  sig
    type t = bbid
    val hash : t -> int
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end
  module E :
  sig
    type t = V.t * V.t
    type label = t
    val label : t -> label
    val src : t -> V.t
    val dst : t -> V.t
  end
  type t
  type vertex = V.t

  val is_directed : bool

  val is_empty : t -> bool
  val nb_vertex : t -> int

  val iter_vertex : (bbid -> unit) -> t -> unit
  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_edges_e : (E.t -> unit) -> t -> unit
  val iter_succ : (vertex -> unit) -> t -> vertex -> unit
  val iter_pred : (vertex -> unit) -> t -> vertex -> unit
  val in_degree : t -> vertex -> int
  val out_degree : t -> vertex -> int
  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
  val fold_succ_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val iter_pred_e : (E.t -> unit) -> t -> V.t -> unit
  val fold_pred_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val vertex_name : vertex -> string
  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val remove_vertex : t -> vertex -> unit
end


module MakeG :
  functor (Lang : sig type t end) ->
    GSIG with type t = Lang.t cfg

module G : GSIG with type t = Vine.stmt list cfg

module NodePartition :
  sig

    module type NP = 
    sig
      type t
      type vertex

      module S : Set.S with type elt = vertex
	
      (** [partition_nodes g v] partitions nodes into two sets:
	  (reachable from [v], unreachable from [v])*)
      val partition_nodes : t -> vertex -> S.t * S.t
	
      (** [unreachable g v] returns all nodes unreachable from [v] *)
      val unreachable : t -> vertex -> S.t
	
      (** [reachable g v] returns all nodes reachable from [v] *)
      val reachable : t -> vertex -> S.t
	
      (** [has_unreachable g v] returns true iff g has a vertex
	  unreachable from [v] *)
      val has_unreachable : t -> vertex -> bool
	
      (** [fold_reachable f g v a] folds over all reachable
	  nodes from [v] *)
      val fold_reachable : 
	(vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a 
	
      (** [fold_unreachable f g v a] folds over all unreachable
	  nodes from [v] *)
      val fold_unreachable : 
	(vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a 
	
      (** [iter_reachable f g v] iterates [f] over all 
	  reachable vertices from [v] *)
      val iter_reachable : (vertex -> unit) -> t -> vertex -> unit
	
      (** [iter_unreachable f g v] iterates [f] over all 
	  reachable vertices from [v] *)
      val iter_unreachable : (vertex -> unit) -> t -> vertex -> unit
	
    end

    module type G = sig
      type t
      module V : Graph.Sig.COMPARABLE
      type vertex  = V.t
      val pred : t -> V.t -> V.t list
      val succ : t -> V.t -> V.t list
      val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
      val iter_succ : (vertex -> unit) -> t -> vertex -> unit
    end

    module Make : functor (G: G) -> NP with type t= G.t and type vertex = G.V.t
  end

(* I kind of wish ocamlgraph had signatures for these... *)

module Toposort :
sig
  val fold : (G.V.t -> 'a -> 'a) -> G.t -> 'a -> 'a
  val iter : (G.V.t -> unit) -> G.t -> unit
end

module Component :
sig
  val scc : G.t -> int * ( G.V.t -> int )
  val scc_list : G.t -> G.V.t list list
end

module Bfs :
sig
  val iter : (G.V.t -> unit) -> G.t -> unit
  val iter_component : (G.V.t -> unit) -> G.t -> G.V.t -> unit
  type iterator = Graph.Traverse.Bfs(G).iterator
  val start : G.t -> iterator
  val step : iterator -> iterator
  val get : iterator -> G.V.t
end

module Dfs :
sig
  val iter : ?pre:(G.V.t -> unit) -> ?post:(G.V.t -> unit) -> G.t -> unit
  val prefix : (G.V.t -> unit) -> G.t -> unit
  val postfix : (G.V.t -> unit) -> G.t -> unit
  val iter_component :
    ?pre:(G.V.t -> unit) -> ?post:(G.V.t -> unit) -> G.t -> G.V.t -> unit
  val prefix_component : (G.V.t -> unit) -> G.t -> G.V.t -> unit
  val postfix_component : (G.V.t -> unit) -> G.t -> G.V.t -> unit
  type iterator = Graph.Traverse.Dfs(G).iterator
  val start : G.t -> iterator
  val step : iterator -> iterator
  val get : iterator -> G.V.t
  val has_cycle : G.t -> bool
end

module BBSet : Set.S with type elt = G.V.t

(** Remove unreachable nodes from a CFG.
    It does not remove anything if BB_Indirect is reachable.
    @param from bbid to test for reachability from. Defaults to BB_Entry.
*)
val remove_unreachable : ?from:bbid -> ?consider_ind:bool -> Vine.stmt list cfg -> unit

(** [well_defined g] checks whether [g] is really well defined. A
    graph is well defined if it has an entry, an exit, does not have
    BB_Indirect (else we really don't know all control flow), and does
    not have unreachable nodes *)
val well_defined : Vine.stmt list cfg -> bool



val split_list_cfg :
  'a list cfg ->
  'a ->
  ((label -> unit) -> 'a list -> unit) ->
  'a list cfg


(** essentially exposes the constructor of cfg so we can make cfg
    objects outside this file *)
val empty_cfg :
  int -> ((label -> unit) -> 'a -> unit) -> Vine.decl list -> 'a cfg


(** {6 Older interface} *)

(**  This represents the older interface to the CFG. Functionality is
    basically the same as that of the newer interface, but it is functional
    and operates only on [bbid]s. *)

val add_edge : 'a #cfg -> bbid -> bbid -> unit
val remove_edge : 'a #cfg -> bbid -> bbid -> unit
val is_edge : 'a #cfg -> bbid -> bbid -> bool
val add_bb : 'a #cfg -> bbid -> 'a -> unit
val rename_bb : 'a #cfg -> bbid -> bbid -> unit
val cfg_nodes : 'a #cfg -> bbid list
val cfg_has_bb : 'a #cfg -> bbid -> bool
val entry_node : 'a -> bbid
val exit_node : 'a -> bbid
val cfg_numnodes : 'a #cfg -> int
val find_label : 'a #cfg -> label -> bbid
val cfg_has_label : 'a #cfg -> label -> bool
val bb_stmts : 'a #cfg -> bbid -> 'a
val bb_set_stmts : 'a #cfg -> bbid -> 'a -> unit
val bb_pred : 'a #cfg -> bbid -> bbid list
val bb_succ : 'a #cfg -> bbid -> bbid list

(** {6 Icky/Deprecated functions} *)

(** FIXME: Some of these are kind of icky and need to be dealt with
    properly. *)


(**/**)
(* FIXME: Go through these and decide which should stay. *)
val cfg_entry : bbid
val cfg_ret : bbid
val cfg_halt : bbid
val cfg_ijmp : bbid
val cfg_unknown : bbid


val chop : G.t ->  G.V.t -> G.V.t -> bbid -> Vine.stmt list cfg
val remove_chop_trailing_edges :
  Vine.stmt list #cfg -> bbid -> Vine.label -> unit

val remove_call_returns : Vine.stmt list cfg -> unit
val create_chop_virtual_cfg :
  (< add_bb : 'b -> 'c list -> unit; add_edge : 'd -> 'd -> unit;
     find : 'b -> 'd; newid : 'b; pred : 'd -> 'd list;
     remove_edge : 'd -> 'd -> unit; succ : 'd -> 'd list; .. >
   as 'a) ->
  'b list -> 'b list -> 'b * 'b * 'a
val create_callgraph : 'a -> 'b
val add_returns :
  < find : bbid -> Vine.stmt list bb; get_id : 'a -> bbid;
    pred : Vine.stmt list bb -> 'a list; .. > ->
  unit
val function_unmangle : string -> string
val supergraph_to_cfgs :
  (Vine.stmt list #cfg as 'a) ->
  (bbid -> Vine.label * bbid) ->
  (Vine.stmt * 'a) list -> (Vine.label, Vine.stmt list cfg) Hashtbl.t
val superchop_to_cfgs :
  (Vine.stmt list #cfg as 'a) ->
  (bbid -> Vine.label * bbid) ->
  (Vine.stmt * 'a) list ->
  bbid -> bbid -> bbid -> (Vine.label, Vine.stmt list cfg) Hashtbl.t
val cfgs_to_prog :
  'a -> (Vine.label, Vine.stmt list #cfg) Hashtbl.t -> 'a * Vine.stmt list
val supergraph_to_prog :
  (Vine.stmt list #cfg as 'a) ->
  (bbid -> Vine.label * bbid) ->
  (Vine.stmt * 'a) list -> Vine.decl list * Vine.stmt list
val superchop_to_prog :
  (Vine.stmt list #cfg as 'a) ->
  (bbid -> Vine.label * bbid) ->
  (Vine.stmt * 'a) list ->
  bbid -> bbid -> bbid -> Vine.decl list * Vine.stmt list
val create_supergraph :
  (Vine.stmt * Vine.stmt list cfg) list ->
  Vine.stmt list cfg * (Vine.label * bbid, bbid) Hashtbl.t *
  (bbid -> Vine.label * bbid)
val print_succ_lst : 'a cfg -> out_channel -> 'a bb -> unit
val print_dot_reachable_cfg :
  Vine.stmt list cfg ->
  string ->
  (Vine.stmt list cfg -> Vine.stmt list bb -> string) ->
  G.V.t -> out_channel -> unit
val print_dot_cfg :
  'a cfg -> string -> ('a cfg -> 'a bb -> string) -> out_channel -> unit
val default_blk_printer : 'a bb -> string
val function_name_printer :
  (bbid, string) Hashtbl.t -> 'a -> 'b bb -> string
val stmt_printer : 'a -> Vine.stmt list bb -> string
val label_printer :
  < get_id : 'a -> bbid; get_info : 'a -> Vine.stmt list; .. > ->
  'a -> string
val comment_printer :
  < get_id : 'a -> bbid; get_info : 'a -> Vine.stmt list; .. > ->
  'a -> string
val supergraph_name_printer :
  (bbid -> string * bbid) ->
  (string,
   < find : bbid -> 'a; get_id : 'a -> bbid; get_info : 'a -> Vine.stmt list;
     .. >)
  Hashtbl.t -> 'b -> 'c bb -> string
val supergraph_small_name_printer :
  (bbid -> string * bbid) ->
  (string,
   < find : bbid -> 'a; get_id : 'a -> bbid; get_info : 'a -> Vine.stmt list;
     .. >)
  Hashtbl.t -> 'b -> Vine.stmt list bb -> Vine.label
val supergraph_bbid_printer :
  (bbid -> string * bbid) -> 'a -> 'b -> 'c bb -> string
val print_backedges :
  < find : 'a -> 'b bb;
    iter_succ : ('b bb -> unit) -> 'b bb -> unit; .. > ->
  'a -> unit
(**/**)
