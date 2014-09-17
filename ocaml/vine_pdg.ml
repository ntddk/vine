(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(**
   Program-Dependence Graphs for Vine. The PDG works on SSA stmt
   lists.  If the stmt list contains more than a single statment, you
   may get more edges than strictly necessary.  Use Ssa.ssalist_to_ssa
   to break an ssa list cfg into an ssa list cfg where each list has
   only one statement.
*)

module C = Vine_cfg

module D = Debug.Make(struct let name = "PDG" and default=`Debug end)
open D


module CDG(Lang: sig type t end) =
struct
  module G = Vine_cfg.MakeG(Lang)

  (* the graph we will return *)
  module CDG = Graph.Persistent.Digraph.Concrete(G.V)
  include CDG

  (* reverse graph with a fake edge from Exit to Entry *)
  module G' = struct
    type t = G.t
    module V = G.V

    let pred g v = 
      let r = G.succ g v in
	if v == C.BB_Entry then C.BB_Exit::r else r

    let succ g v =
      let r = G.pred g v in
	if v == C.BB_Exit then C.BB_Entry::r else r
      
    let nb_vertex = G.nb_vertex
    let fold_vertex = G.fold_vertex
    let iter_vertex = G.iter_vertex
  end

  (* inverse dominators module *)
  module D = Dominator.Make(G')

  (** This function builds a CDG (Control-Dependence Graph) for the given
      CFG. A CDG has an edge from x to y any time y is control-dependent on x.*)
  let compute_cdg cfg =
    (* Note that we don't add an extra entry node, so everything is control
       dependent on the entry node of the CFG *)
    (*
    let () = dprintf "compute_cdg: computing dominators" in
    let dominators = D.compute_dominators cfg C.BB_Exit in
    let () = dprintf "compute_cdg: computing dom tree" in
    let dom_tree = D.dominators_to_dom_tree cfg dominators in
    let () = dprintf "compute_cdg: getting dom function" in
    let dom = D.dominators_to_dom dominators in
    *)
    let () = dprintf "compute_cdg: computing idom" in
    let idom = D.compute_idom cfg C.BB_Exit in
    let () = dprintf "compute_cdg: computing dom tree" in
    let dom_tree = D.idom_to_dom_tree cfg idom in
    let () = dprintf "compute_cdg: computing dom frontier" in
    let df = D.compute_dom_frontier cfg dom_tree idom in
    let vertices =  G.fold_vertex (fun v g -> add_vertex g v) cfg empty in
      G.fold_vertex
	(fun v g -> 
	   if G.in_degree cfg v > 0
	   then List.fold_left (fun g v' -> add_edge g v' v) g (df v)
	   else g (* can't compute DF for lonely nodes *)
	)
	cfg vertices
      
end


(** A Data-Dependency Graph for SSA *)
module SSA_DDG =
struct
  module S = Vine.VarSet
  module Lang =
  struct
    open Ssa
    type t = Ssa.stmt list
    module Var = Vine.Var


    let get_defs acc = function
	Move(l,_) | Call(Some l,_,_) -> S.add l acc
      | _ -> acc
    let get_defs stmts =
      S.elements (List.fold_left get_defs S.empty stmts)

    let rec get_uses acc =
      let add_val_uses us = function
	  Lval l -> S.add l us
	| _ -> us
      in
      let add_vals_uses us vals =  List.fold_left add_val_uses us vals in
      let add_exp_uses acc  = function
	| UnOp(_,v)
	| Val v
	| Cast(_,_,v)
	  -> add_val_uses acc  v
	| BinOp(_,v1,v2)
	| Get(v1,v2,_) ->
	    add_vals_uses acc [v1;v2]
	| Set(v1,v2,v3,_) ->
	    add_vals_uses acc [v1;v2;v3]
	| Phi vs  -> List.fold_left (fun acc v -> S.add v acc) acc vs
	| Unknown _ -> acc
      in
	function
	  | Jmp v
	  | Return(Some v)
	  | Halt v
	  | Assert v
	    -> add_val_uses acc v
	  | CJmp(v1,v2,v3) -> add_vals_uses acc [v1;v2;v3]
	  | Move(_,e) -> add_exp_uses acc e
	  | Label _
	  | Comment _
	  | Return None 
	    -> acc
	  | Call(_,_,vs) -> add_vals_uses acc vs
	  | Attr(s,_) -> get_uses acc s
    let get_uses stmts =
      S.elements (List.fold_left get_uses S.empty stmts)
	
  end

  module VH = Hashtbl.Make(Lang.Var)
  module G = Vine_cfg.MakeG(Lang)

  (* the graph we will return *)
  module DDG = Graph.Persistent.Digraph.Concrete(G.V)
  include DDG
  
  
  let compute_true_dependence cfg =
    let defs = VH.create 57 in
    let () =
      G.iter_vertex
	(fun v -> let s = C.bb_stmts cfg v in
          List.iter (fun var -> VH.add defs var v) (Lang.get_defs s))
	cfg
    in
    let vertices =  G.fold_vertex (fun v g -> add_vertex g v) cfg empty in
      G.fold_vertex
	(fun v g ->
           List.fold_left
             (fun g var ->
		try (
		  add_edge g (VH.find defs var) v
		) with Not_found ->
		  (wprintf "Can't find definition for %s" (Vine.var_to_string var);
		   g)
	     )
	     g
	     (Lang.get_uses (C.bb_stmts cfg v))
	)
	cfg vertices

end

type dependence = [`True | `Control]

module SSA_PDG  =
struct
  module Lang = struct type t = Ssa.stmt list end 
  module G = Vine_cfg.MakeG(Lang)

  module Label =
  struct
    type t = dependence
    let compare = Pervasives.compare
    let default = `True
  end
  module PDG = Graph.Persistent.Digraph.ConcreteLabeled(G.V)(Label)
  include PDG

  module CDG = CDG(Lang)
  module DDG = SSA_DDG

  let compute_pdg cfg =
    let cdg = CDG.compute_cdg cfg in
    let ddg = DDG.compute_true_dependence cfg in
    let vertices =  G.fold_vertex (fun v g -> add_vertex g v) cfg empty in
    let withddg = 
      DDG.fold_edges
	(fun v v' g -> add_edge_e g (v, `True, v'))
	ddg
	vertices
    in
    let pdg = 
      CDG.fold_edges
	(fun v v' g ->
	   if not(mem_edge g v v')
	   then add_edge_e g (v, `Control, v')
	   else g
	)
	cdg
	withddg
    in
      (* FIXME: Remove other unneeded control dependences from the graph, as
	 is traditional, according to the Muchnick book. *)
      pdg

  module REVG = 
  struct
    type t = PDG.t
    module V = PDG.V
    type vertex = PDG.V.t
      
    let pred = PDG.succ
    let succ  = PDG.pred
    let fold_vertex = PDG.fold_vertex
    let iter_vertex = PDG.iter_vertex
    let iter_succ = PDG.iter_pred 
  end

  module REVNP = Vine_cfg.NodePartition.Make(REVG);;
	
  let slice g v  = 
    REVNP.reachable g v  
      
end


module PdgSsaStmtsPrinter =
struct
  module Helper =
    Vine_graphviz.MakeOtherCfgPrinter(SSA_PDG)(Ssa.G)(Vine_graphviz.PrintSsaStmts)
  include Helper

  let edge_attributes (((_,t,_),_): E.t) = 
    match t with
	`Control -> [`Style `Dashed]
      | `True -> []
end

module PdgSsaStmtDot = Graph.Graphviz.Dot(PdgSsaStmtsPrinter)

