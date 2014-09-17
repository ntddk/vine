(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Callgraphs of Vine programs.

    If you are interested in preserving calling contexts, see
    Vine_callstrings instead.
*)


module G :
  Graph.Sig.P with type V.t = Vine.label and type V.label = Vine.label

(** [mk_callmap sl] returns a pair [(cmap,fmap)].

    [cmap] is a mapping from caller name to a list of
    (callee, stmt no. in caller) pairs.

    [fmap] is a mapping from function name -> Vine.Function.  Note that
    the data in this mapping may be either a function definition or
    external function declaration.
    
    This function is a utility which is not specific to callstrings,
    but is here nonetheless.
*)
val mk_callmap : ?indirect_label:Vine.label -> Vine.stmt list ->
  (Vine.label, Vine.label list) PMap.t * (Vine.label, Vine.stmt) PMap.t

(** make a normal callgraph. A callgraph is similar to a callstring
    graph, except that different calling contexts are not
    distinguished.  [mk_cg root prog] acts like [mk_csg root prog]
*)
val mk_cg : ?indirect_label:Vine.label -> Vine.program -> G.t


(** Make a function dependence graph.
    Every function has an edge to every function it calls either directly or
    indirectly.
*)
val mk_fdep : Vine.program -> G.t

(** Uses the function dependence graph to tell which functions are recursive. *)
val is_recursive : Vine.program -> Vine.label -> bool

(** Output a graph to a [.dot] file *)
val output_dot : out_channel -> G.t -> unit
