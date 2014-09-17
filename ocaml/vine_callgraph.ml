(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

open Vine_util

module FunLab =
struct
  type t = Vine.label
  let compare = String.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module G =
struct
  include Graph.Persistent.Digraph.Concrete(FunLab)

    (* Stuff for Dot *)
  let graph_attributes g = []
  let default_vertex_attributes g = []
  let vertex_name v = v
  let vertex_attributes v = [`Label v]
  let default_edge_attributes g = []
  let edge_attributes e = []
  let get_subgraph v = None

end

module Dot = Graph.Graphviz.Dot(G);;

let output_dot = Dot.output_graph

module M = PMap

let rec fold_stmt f s a = match s with
    | Vine.Block(dl,sl) -> fold_stmts f sl a
    | Vine.Attr(s,_) -> fold_stmt f s a
    | s -> f s a
and fold_stmts f ss a = match ss with
  | [] -> a
  | s::sl -> fold_stmts f sl (fold_stmt f s a)


let mk_callmap ?indirect_label sl =
  List.fold_left
    (fun (cmap,fmap) s ->
       match fst(Vine.strip_attrs s) with
	 | Vine.Function(v,_,_,_,Some s) as f -> (
	     let callees =
	       fold_stmt
		 (fun s acc -> match s with
		    | Vine.Call(_,Vine.Name(v),_) -> 
			v::acc
		    | Vine.Call _ -> ( match indirect_label with
					 | Some x -> x::acc
					 | None -> acc )
		    | _ -> 
			acc )
		 s []
	     in
	      (M.add v (List.rev callees) cmap, M.add v f fmap)
	   )
	 | Vine.Function(v,_,_,true,None) as f ->
	     (M.add v [] cmap, M.add v f fmap)
	 | _ -> (cmap, fmap)
      ) (M.empty, M.empty) sl


let mk_cg ?indirect_label (dl,sl) = 
  let (cmap,_)  = mk_callmap ?indirect_label sl in
  let g_with_vertices = M.foldi (fun k _ g -> G.add_vertex g k) cmap G.empty in
    M.foldi
      (fun caller callees g ->
	 list_foldl
	   (fun callee g ->
	      G.add_edge g caller callee
	   )
	   callees g
      )
      cmap g_with_vertices


let mk_fdep prog =
  let rec add_deps g = function
    | [] -> g
    | n::ns ->
	let g' = G.fold_succ
	  (fun s a ->
	     G.fold_succ
	       (fun ss g ->
		  if G.mem_edge g n ss then g
		  else G.add_edge g n ss
	       )
	       g s a
	  )
	  g n g
	in
	let nodes =
	  if g == g' then ns
	  else list_union ns (List.filter ((!=)n) (G.pred g n))
	in
	  add_deps g' nodes
  in
  let cg = mk_cg prog in
  let nodes = G.fold_vertex (fun v l -> v::l) cg [] in
    add_deps cg nodes

let is_recursive prog =
  let fdep = mk_fdep prog in
  let ht = Hashtbl.create (G.nb_vertex fdep) in
    G.iter_vertex (fun v -> Hashtbl.add ht v (G.mem_edge fdep v v)) fdep;
    Hashtbl.find ht

    

