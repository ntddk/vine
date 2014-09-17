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

open Vine_util
module List = ExtList.List;;



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


module type BOUNDED_MEET_SEMILATTICE =
sig
  type t
  val top : t
  val meet : t -> t -> t
  val equal : t -> t -> bool
end

(** a dataflow is defined by a lattice over a graph. *)
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


(** [worklist_iterate nodes entry  f init pred top meet] will return a
    pair of functions in(B) and out(B) defined for all B in [nodes]. 
    
    This function works in the forward direction. (swap the pred and succ
    arguments to work backwards)
    @param f_t  takes in a  node ('n) and whatever you are computing
    ('l), and returns a new whatever you are computing.
*)
let worklist_iterate ~(nodes:'n list) ~(entry:'n) ~(f_t:'n->'l->'l) ~(init:'l)
    ~(pred:'n->'n list) ~(succ:'n->'n list)
    ~(top:'l) ~(meet:'l->'l->'l) ~(eq:'l->'l->bool)
    =
  let htin = Hashtbl.create (List.length nodes) in
  let dfin = Hashtbl.find htin in (* function to be returned *)
  let htout = Hashtbl.create (List.length nodes) in
  let dfout n =
    try
      Hashtbl.find htout n
    with Not_found ->
      let out = (f_t n (dfin n)) in
      let () = Hashtbl.add htout n out in
	out
  in
  let () = List.iter (fun n -> Hashtbl.add htin n top) nodes in
  let () = Hashtbl.replace htin entry init in
  let rec do_work = function
      [] -> ()
    | b::worklist when b = entry ->
	(* don't ever update the latice value for entry *)
	do_work worklist
    | b::worklist -> 
	let totaleffect =
	  match pred b with
	      x::xs ->
		List.fold_left (fun l p -> meet l (dfout p)) (dfout x) xs
	    | [] -> top
	in
	let newout = f_t b totaleffect in
	let worklist =
	  if eq (dfout b) newout
	  then worklist
	  else (Hashtbl.replace htin b totaleffect;
		Hashtbl.replace htout b newout;
		worklist@list_difference (succ b) worklist )
	in
	  do_work worklist
  in
  let () = do_work (list_difference nodes [entry]) in
    (dfin, dfout)
;;


let topological_worklist_iterate ~(nodes:'n list) ~(entry:'n) ~(f_t:'n->'l->'l) ~(init:'l)
    ~(pred:'n->'n list) ~(succ:'n->'n list)
    ~(top:'l) ~(meet:'l->'l->'l) ~(eq:'l->'l->bool)
    =
  let htin = Hashtbl.create (List.length nodes) in
  let dfin = Hashtbl.find htin in (* function to be returned *)
  let htout = Hashtbl.create (List.length nodes) in
  let dfout n =
    try
      Hashtbl.find htout n
    with Not_found ->
      let out = (f_t n (dfin n)) in
      let () = Hashtbl.add htout n out in
	out
  in
  let () = List.iter (fun n -> Hashtbl.add htin n top) nodes in
  let () = Hashtbl.replace htin entry init in
  let rec do_work = function
      [] -> ()
    | b::worklist ->  
	let inset = (dfin b) in 
	let outset = (f_t b inset) in 
	let () = Hashtbl.replace htout b outset in
	let affected_elems =
	  List.filter 
	    (fun s -> let newin = meet (dfin s) (outset) in
	       if eq (dfin s) newin
	       then false
	       else let () = Hashtbl.replace htin s newin in true
	    )
	    (succ b)
	in
	let newwklist = worklist@list_difference affected_elems worklist
	in
	  do_work newwklist
  in
  let () = do_work ([entry]) in
    (dfin, dfout)
;;

(* 
let worklist_iterate = topological_worklist_iterate
*)

module Make (D:DATAFLOW) = 
struct
  (** returns a pair of functions for in(B) and out(B) for all nodes B
      in the graph *)
  let worklist_iterate ?(init = D.init) g = 
    let nodes = D.G.fold_vertex (fun x acc -> x::acc) g [] in
    let f_t = D.transfer_function g in 
    let succ,pred = 
      if D.dir = Forward then 
	( D.G.succ g, D.G.pred g) 
      else
	(D.G.pred g, D.G.succ g)
    in
      worklist_iterate ~nodes:nodes ~entry:D.s0 ~f_t:f_t
	~init:init ~pred:pred ~succ:succ ~top:D.L.top ~meet:D.L.meet
	~eq:D.L.equal

  let topological_worklist_iterate ?(init = D.init) g = 
    let nodes = D.G.fold_vertex (fun x acc -> x::acc) g [] in
    let f_t = D.transfer_function g in 
    let succ,pred = 
      if D.dir = Forward then 
	( D.G.succ g, D.G.pred g) 
      else
	(D.G.pred g, D.G.succ g)
    in
      topological_worklist_iterate ~nodes:nodes ~entry:D.s0 ~f_t:f_t
	~init:init ~pred:pred ~succ:succ ~top:D.L.top ~meet:D.L.meet
	~eq:D.L.equal

end





(** Like [worklist_iterate], but only returns the [in] function *)
let worklist_iterate_in
    ~(nodes:'n list) ~(entry:'n) ~(f_t:'n->'l->'l) ~(init:'l)
    ~(pred:'n->'n list) ~(succ:'n->'n list)
    ~(top:'l) ~(meet:'l->'l->'l) ~(eq:'l->'l->bool)
    =
  fst(worklist_iterate nodes entry f_t init pred succ top meet eq)

(** Like [worklist_iterate], but only returns the [out] function *)
let worklist_iterate_out
    ~(nodes:'n list) ~(entry:'n) ~(f_t:'n->'l->'l) ~(init:'l)
    ~(pred:'n->'n list) ~(succ:'n->'n list)
    ~(top:'l) ~(meet:'l->'l->'l) ~(eq:'l->'l->bool)
    =
  snd(worklist_iterate nodes entry f_t init pred succ top meet eq)


