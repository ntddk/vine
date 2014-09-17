(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Vine output to Graphviz.
    This module contains stuff for outputing CFGs and other graphs used in
    Vine to graphviz format.
*)

open Vine_util

module V = Vine
module C = Vine_cfg
module S = Ssa


module type G =
sig
  type t 
  module V :
  sig
    type t = C.bbid
    val hash : t -> int
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end
  module E :
  sig
    type t
    type label
    val label : t -> label
    val src : t -> V.t
    val dst : t -> V.t
  end

  val iter_vertex : (V.t -> unit) -> t -> unit

  val iter_edges_e : (E.t -> unit) -> t -> unit

end
module type CFG =
sig
  include G
  val vertex_name : V.t -> string
end

module type DOTTYG =
sig
  type t 
  module V : Graph.Sig.COMPARABLE
  module E :
  sig
    type t
    type label
    val label : t -> label
    val src : t -> V.t
    val dst : t -> V.t
  end


  val iter_vertex : (V.t -> unit) -> t -> unit

  val iter_edges_e : (E.t -> unit) -> t -> unit
  val graph_attributes : t -> Graph.Graphviz.DotAttributes.graph list
  val default_vertex_attributes : t -> Graph.Graphviz.DotAttributes.vertex list
  val vertex_name : V.t -> string
  val vertex_attributes : V.t -> Graph.Graphviz.DotAttributes.vertex list
  val get_subgraph : V.t -> Graph.Graphviz.DotAttributes.subgraph option
  val default_edge_attributes : t -> Graph.Graphviz.DotAttributes.edge list
  val edge_attributes : E.t -> Graph.Graphviz.DotAttributes.edge list
end


(* Just for convenience *)
module NoAttrs =
struct
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end


(** Makes a module suitable for use with Graph.Graphviz.Dot  for writting out
    a CFG. *)
module MakeCfgPrinter
  (G:CFG with type V.t = C.bbid)
  (Printer:sig val print: G.t -> C.bbid -> string end)
  : (DOTTYG with type t = G.t and type V.t = G.V.t * G.t and type E.t =  G.E.t * G.t)
  =
struct
  type t = G.t

  module V =
  struct
    type t = G.V.t * G.t
    let hash = G.V.hash <@ fst
    let equal x y = G.V.equal (fst x) (fst y)
    let compare x y = G.V.compare (fst x) (fst y)
  end
  module E =
  struct 
    type t = G.E.t * G.t
    type label = G.E.label
    let label (e,g) = G.E.label e
    let src (e,g) = (G.E.src e, g)
    let dst (e,g) = (G.E.dst e, g)
  end

  let iter_edges_e f g =
    G.iter_edges_e (fun e -> f (e,g)) g

  let iter_vertex f g =
    G.iter_vertex (fun v -> f (v,g)) g

  include NoAttrs

  let vertex_name(v,g) = G.vertex_name v

  let vertex_attributes (v,g) =
    (* FIXME: The Dot module really should be the one doing the escaping here *)
    [`Label (String.escaped(Printer.print g v));
     `Shape `Box]

end


(** Like MakeCfgPrinter, but prints the structure from a different graph *)
module MakeOtherCfgPrinter
  (G:G with type V.t = C.bbid)
  (CFG:CFG with type V.t = C.bbid)
  (Printer:sig val print: CFG.t -> C.bbid -> string end)
  : (DOTTYG with type t = (G.t * CFG.t) and type E.t =  G.E.t * CFG.t)
  =
struct
  module P = MakeCfgPrinter (CFG) (Printer)
  type t = G.t * CFG.t

  module V = P.V

  module E =
  struct 
    type t = G.E.t * CFG.t
    type label = G.E.label
    let label (e,g) = G.E.label e
    let src (e,g) = (G.E.src e, g)
    let dst (e,g) = (G.E.dst e, g)
  end

  let iter_edges_e f (g,cfg) =
    G.iter_edges_e (fun e -> f (e,cfg)) g

  let iter_vertex f (g,cfg) =
    G.iter_vertex (fun v -> f (v,cfg)) g

  include NoAttrs
      
  let vertex_name = P.vertex_name
  let vertex_attributes = P.vertex_attributes

end


module PrintVineStmts =
struct
       (* Old Vine_cfg.stmt_printer *)
       let print g b =
	 let blk = g#find b in
	 let re = Str.regexp "[\t\n]" in 
	   C.bbid_to_string b^"\n"
	   ^ List.fold_left (fun r s -> 
			       let x = V.stmt_to_string s in 
			       let x' = Str.global_replace re " " x in 
			       let x' = String.escaped x' in 
				 r^"\n"^x') "" (g#get_info blk)

end

module VineStmtsPrinter = MakeCfgPrinter (Vine_cfg.G) (PrintVineStmts)
module VineStmtsDot = Graph.Graphviz.Dot(VineStmtsPrinter)


module PrintVineLabel =
struct
       let print g b =
	 let blk = g#find b in
	   match g#get_info blk with
	       [] -> C.bbid_to_string b
	     | stmts -> 
		 List.fold_left
		   (fun acc -> function
			V.Label x -> acc^"\n"^x
		      | _ -> acc )
		   "" stmts

end

module VineLabelPrinter = MakeCfgPrinter (Vine_cfg.G) (PrintVineLabel)
module VineLabelDot = Graph.Graphviz.Dot(VineLabelPrinter)


module PrintVineComment =
struct
       let print g b =
	 let blk = g#find b in
	   match g#get_info blk with
	       [] -> C.bbid_to_string b
	     | stmts -> 
		 List.fold_left
		   (fun acc -> function
			V.Comment(x) -> acc^"\n"^x
		      | _ -> acc )
		   "" stmts
end

module VineCommentPrinter = MakeCfgPrinter (Vine_cfg.G) (PrintVineComment)
module VineCommentDot = Graph.Graphviz.Dot(VineCommentPrinter)


module PrintSsaStmts =
struct
       let print g b =
	 let blk = g#find b in
	 let stmts = g#get_info blk in
	 let buf = Buffer.create (20*(List.length stmts+1)) in
	 let pr = Buffer.add_string buf in
	   pr(C.bbid_to_string b);
	   pr "\n";
	   List.iter (S.pp_stmt pr) stmts;
	   Buffer.contents buf
end

module SsaStmtsPrinter = MakeCfgPrinter (Ssa.G) (PrintSsaStmts)
module SsaStmtsDot = Graph.Graphviz.Dot(SsaStmtsPrinter)


module PrintSsaStmt =
struct
       let print g b =
	 let blk = g#find b in
	   C.bbid_to_string b^"\n"
	   ^  (String.escaped (S.stmt_to_string (g#get_info blk)))
end

module SsaStmtPrinter = MakeCfgPrinter (Ssa.StmtG) (PrintSsaStmt)
module SsaStmtDot = Graph.Graphviz.Dot(SsaStmtPrinter)

module PrintSsaLabels = 
struct
  let print g b =
    let blk = g#find b in
      match g#get_info blk with
	  [] -> C.bbid_to_string b
	| stmts -> 
	    List.fold_left
	      (fun acc -> function
		   S.Label x -> acc^"\n"^x
		 | _ -> acc )
	      "" stmts


end

module SsaLabelPrinter = MakeCfgPrinter (Ssa.G) (PrintSsaLabels)
module SsaLabelDot = Graph.Graphviz.Dot(SsaLabelPrinter)
