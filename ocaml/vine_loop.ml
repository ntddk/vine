(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(**
   Loop analysis inteface for Vine CFG. The functions in this module are
   for getting general loop information and performing loop analysis
   on CFG of type [Vine.stmt list Vine_cfg.cfg]. For other types of CFG,
   please use a functor [LOOP].

   Loop Analysis:
   - Conversion from irreducible CFG to reducible CFG
   - Loop identification
   - Loop unrolling
   - Induction variable identification

   "Loop" here refers to a natural loop which has the following properties:
   - has one header node (block)
   - has at least one back-edge
   - its body contains all nodes that can reach the back node without 
   going through the header node.
   - all back-edges go back to the header
   - if 2 natural loops have different headers, either they are disjoint or
     one is nested within another
   - if 2 natural loops have the same header, either one is nested within
     another or both are in the same natural loop

   For example, ( 1->2, 2,->1, 2->3, 2->4, 3->1, 4->1 ) has 2 natural loops:
   ( 1->2, 2->1 ) and ( 1->2, 2->3, 2->4, 3->1, 4->1 ) where the first is
   nested in the second. Note that there are also sub-loops ( 1->2->3->1 ) and
   ( 1->2->4->1 ) which are parts of the second natural loop.
*)

open ExtList (* replace List with ExtList.List *)

module V = Vine
module VCFG = Vine_cfg
module VUTIL = Vine_util
module D = Debug.Make(struct let name = "Vine_loop" and default=`Debug end)

type color = White | Grey | Black
type region_id = int
type 'a region = { mutable id        : region_id;
                   mutable nodes     : 'a list; (* bb list/set *)
                   mutable pred      : region_id list; (* region list/set *)
                   mutable succ      : region_id list; (* region list/set *)
		   mutable color     : color; (* walk color for an iteration *)
}

let new_region () =
  let cur_id = ref 0 in
    fun x ->
      cur_id := !cur_id + 1;
      {id = !cur_id;
       nodes = x;
       pred = [];
       succ = [];
       color = White;
      }
	
let get_unique = 
  let rec get_unique' cur = function
      x::l -> if (cur = x) then get_unique' cur l else None
    | [] -> Some cur
  in function 
      [] -> None
    | x::l -> get_unique' x l

let dprint_reg_ht cfg reg_ht =
  let printed = ref [] in
    Hashtbl.iter (fun _ b ->
      if not (List.mem b !printed)
      then (
	D.dprintf "ID   : %d" b.id;
	D.dprintf "MEM  : %s" (List.fold_left (fun a b -> 
	  a^", "^(VCFG.bbid_to_string (cfg#get_id b))) "" b.nodes);
	D.dprintf "PRED : %s" 
	  (List.fold_left (fun a b -> a^", "^(string_of_int b)) "" b.pred);
	D.dprintf "SUCC : %s" 
	  (List.fold_left (fun a b -> a^", "^(string_of_int b)) "" b.succ);
	D.dprintf "\n";
	printed := b :: !printed
      )
    ) reg_ht
      
let add_new_edge a b c =
  if not (a#has_edge b c) then a#add_edge b c
      
(**
   @param bb_list list of bb to be duplicated. 
   The list must have unique elements.
   @return hashtbl of old bb to new bb.
*)
let dup_bbs cfg bb_list =
  let bb_maps = Hashtbl.create (List.length bb_list) in
    List.iter (fun bb -> Hashtbl.add bb_maps bb
      (cfg#create_bb (cfg#newid) (cfg#get_info bb))) bb_list;
(*    Hashtbl.iter (fun a b ->
      D.dprintf "%s -> %s" ((VCFG.bbid_to_string (cfg#get_id a)))
	((VCFG.bbid_to_string (cfg#get_id b)))) bb_maps;*)
    let old2new old_bb =
      try Hashtbl.find bb_maps old_bb
      with Not_found -> old_bb
    in
      Hashtbl.iter (fun old_bb new_bb ->
	List.iter (fun old_pred ->
	  add_new_edge cfg (old2new old_pred) new_bb
	) (cfg#pred old_bb);
	List.iter (fun old_succ ->
	  add_new_edge cfg new_bb (old2new old_succ)) (cfg#succ old_bb)
      ) bb_maps;
      bb_maps
	
let remove_edges cfg from_bbs to_bbs =
  List.iter (fun from_bb ->
    List.iter (fun to_bb ->
      if cfg#has_edge from_bb to_bb then cfg#remove_edge from_bb to_bb
    ) to_bbs
  ) from_bbs

(**
   @param reg_ht hash_table that maps region's id to a region 
   @param reg_id id of the region to start merging recursively.
   This region must have not yet been called by merge_regions before in
   this iteration.
   @return true if any regions are merged
*)
let merge_regions reg_ht reg =
  let rec merge_regions' reg_ht reg =
    let reg_id = reg.id in
      reg.color <- Grey; 
      let succ_regs = List.map (Hashtbl.find reg_ht) reg.succ in
      let has_merged =
	match get_unique reg.pred with
	    Some pred_id -> (* unique predecessor: merge *)
	      let pred_reg = Hashtbl.find reg_ht pred_id in
		pred_reg.nodes <- 
		  List.append reg.nodes pred_reg.nodes;
		List.iter (fun succ_reg ->
		  succ_reg.pred <- VUTIL.list_union [pred_id] 
		    (List.filter ((<>) reg_id) succ_reg.pred)
		) succ_regs;
		pred_reg.succ <- List.filter ((<>) reg_id) (
		  VUTIL.list_union reg.succ pred_reg.succ);
		pred_reg.succ <- List.filter ((<>) pred_id) pred_reg.succ;
		pred_reg.pred <- List.filter ((<>) pred_id) pred_reg.pred;
		Hashtbl.remove reg_ht reg_id;
		true
	  | None -> false
      in
	List.fold_left (fun merged succ_reg ->
          if (succ_reg.color = White)
          then
            (merge_regions' reg_ht succ_reg or merged)
          else
            merged) has_merged succ_regs
  in
    Hashtbl.iter (fun _ b -> b.color <- White) reg_ht;
    merge_regions' reg_ht reg

(**
   @param reg_ht hash_table that maps region's id to a region
   @param get_newreg a function that create new region
   @return true if any region is splited
*)
let split_regions dup_fixup cfg get_newreg reg_ht entry_reg =
  let sorted_by_subsize = List.fast_sort 
    (fun a b -> (List.length a.nodes) - (List.length b.nodes))
    (List.rev_map (Hashtbl.find reg_ht) entry_reg.succ) 
  in
    (match sorted_by_subsize with
	[] -> false
      | reg :: _ ->
	  let reg_id = reg.id in
	  let pred_regs = List.map (Hashtbl.find reg_ht) reg.pred in
	    List.iter (fun pred_reg -> pred_reg.succ <- 
	      List.filter ((<>) reg_id) pred_reg.succ) pred_regs;
	    (match pred_regs with
		pred_reg_head :: pred_reg_tail ->
		  let pred_bb_pool = List.fold_left
		    (fun a b -> b.nodes @ a) [] pred_regs in
		    List.iter (fun pred_reg ->
		      let bb_map = 
			dup_bbs cfg reg.nodes in
		      let new_reg = get_newreg 
			(Hashtbl.fold (fun _ b c -> b :: c) bb_map []) in
			remove_edges cfg
			  (VUTIL.list_difference pred_bb_pool pred_reg.nodes)
			  new_reg.nodes;
			remove_edges cfg pred_reg.nodes reg.nodes;
			dup_fixup cfg bb_map;
			Hashtbl.add reg_ht new_reg.id new_reg;
			new_reg.pred <- [pred_reg.id];
			new_reg.succ <- reg.succ;
			List.iter (fun succ ->
			  succ.pred <- new_reg.id :: succ.pred)
			  (List.map (Hashtbl.find reg_ht) new_reg.succ);
			pred_reg.succ <- new_reg.id :: pred_reg.succ
		    ) pred_reg_tail;
		    pred_reg_head.succ <- reg_id :: pred_reg_head.succ;
		    reg.pred <- [pred_reg_head.id];
	      | [] -> failwith "the node to be splited has no predecessor"
	    );
	    true
    )
      
(**
   A loop type used in this module and its submodules.
*)
type 'a loop = { cfg                 : 'a VCFG.cfg;
                 mutable header      : 'a VCFG.bb;
		 mutable body        : 'a VCFG.bb list;
		 mutable back_nodes  : 'a VCFG.bb list;
		 mutable exit_edges  : ('a VCFG.bb * 'a VCFG.bb) list;
		 mutable inner_loops : 'a loop list;
		 mutable outer_loops : 'a loop list;
}

(**
   A type that represents how to combine two loops that have the same header.
   
   [KeepDisjoint] - Keep any subloops (both nested and disjoint) separated.
   [UniqueHeader] - Combine all loops with the same header (i.e. eventually,
   each loop has a unique header).
   Future value (i.e. not yet implemented) :
   [KeepNested]   - Combine disjoint loops as one single loop.
   
   NOTE: Two natural loops, that have the same header, 
   are either nested (entirely contained) or disjoint.
*)
type combine_type = KeepDisjoint | UniqueHeader

let insert block_bb loop_i s =
  if not(List.mem block_bb loop_i.body)
  then (
    loop_i.body <- block_bb::loop_i.body;
    Stack.push block_bb s
  )

(**
  Return the loop header
*)
let get_loop_header lp = lp.header
		       
(**
   A topological order of a loop starting from the loop's header
*)
let get_loop_body lp = lp.body

(**
   A list of nodes that have back edges to the header
*)
let get_loop_back_nodes lp = lp.back_nodes

(**
   A list of (source, destination) pairs of edges that leave the loop.
   The input cfg must be the one used to obtain the loop
*)
let get_loop_exit_edges lp _ = lp.exit_edges

(**
   A list of inner loops nested inside a given loop
*)
let get_inner_loops lp cfg = lp.inner_loops

(**
   [get_outer_loops loop cfg] returns 
   a list of outer loops that [loop] is nested within.
*)
let get_outer_loops lp cfg = lp.outer_loops

(**
   Pretty-print of a loop and its information
*)
let pp_loop ps loop =
  let b2s bb = VCFG.bbid_to_string (loop.cfg#get_id bb) in
    ps ("{ header = " ^ (b2s loop.header) ^ ";\n");
    ps  "  body   = ";
    List.iter (fun b -> ps (b2s b^" ")) loop.body;
    ps  ";\n";
    ps  "  backs  = ";
    List.iter (fun b -> ps (b2s b^" ")) loop.back_nodes;
    ps  "; }\n"
	
(**
   [LOOP(struct type t = some_t end)] gives a module that provides interfaces
   for graph reducibility, loop detection, and loop unrolling for a graph of
   type [some_t Vine_cfg.cfg].
*)
module LOOP(Lang : sig type t end) =
struct
  type t = Lang.t VCFG.cfg
  module G = VCFG.MakeG(Lang)
  module Bfs = Graph.Traverse.Bfs(G)
  module Weight = 
  struct
    type label = G.E.label
    type t = int
    let weight _ = 1
    let zero = 0
    let add = (+)
    let compare = Pervasives.compare
  end
  module Dijkstra = Graph.Path.Dijkstra(G)(Weight)
  module DOM = Dominator.Make(G)
  module Toposort = Graph.Topological.Make(G)
    
  (**
     [make_reducible dup_fixup cfg] simplifies [cfg] in a reducible format.
     The content (info) in each copied basic block can be changed by providing
     [dup_fixup] function. [dup_fixup cfg bb_map] is a function that
     takes a BB-duplicated graph and [bb_map] (a map of existing BBs to their
     copies) and correct the content (info) in copied BBs, in their 
     predecessors, and in their successors, if necessary. 
     [dup_fixup cfg bb_map] should only change the content (info) and not
     the graph flow.     

     [dup_fixup] is helpful in correcting info in copied BBs that should not
     be copied directly. For example, copied BB should not have the same label
     as the original one.
  *)
  let make_reducible dup_fixup cfg = 
    let cfgsize = VCFG.cfg_numnodes cfg in
    let reg_ht = Hashtbl.create (cfgsize*2) in
    let get_newreg = new_region () in
    let entry_bb = cfg#find (VCFG.cfg_entry) in
    let entry_reg = 
      let bbmap = Hashtbl.create cfgsize in
	Bfs.iter_component (fun bbid ->
	  let bb = cfg#find bbid in
	    (match bbid with
		VCFG.BB _ 
	      | VCFG.BB_Entry ->
		  let newreg = get_newreg [bb] in
		    Hashtbl.add reg_ht (newreg.id) newreg;
		    Hashtbl.add bbmap bb newreg
	      | _ -> ())) cfg VCFG.cfg_entry;
	Hashtbl.iter (fun bb reg ->
	  reg.pred <-
	    List.fold_left (fun a p ->
	      try
		let mapid = (Hashtbl.find bbmap p).id in
		  if mapid <> reg.id then mapid :: a else a
	      with Not_found -> a) [] (cfg#pred bb);
	  reg.succ <- List.fold_left (fun a s ->
	    try 
	      let mapid = (Hashtbl.find bbmap s).id in
		if mapid <> reg.id then mapid :: a else a
	    with Not_found -> a)
	    [] (cfg#succ bb)) bbmap;
	Hashtbl.find bbmap entry_bb
    in
      while (
	(*dprint_reg_ht cfg reg_ht; D.dprintf "merge_regions";*)
	merge_regions reg_ht entry_reg)
	|| (
          (*dprint_reg_ht cfg reg_ht;D.dprintf "split_regions";*)
          split_regions dup_fixup cfg get_newreg reg_ht entry_reg
	)
	do
	() 
      done
	
  (**
     [get_reducible] is a alternative interface of 
     [make_reducible]. Unlike [make_reducible], [get_reducible] returns
     a new graph and leaves the original graph untouched.
  *)
  let get_reducible dup_fixup ocfg =
    let ncfg = ocfg#copy in
    let () = make_reducible dup_fixup ncfg in
      ncfg

  let is_reachable cfg x =
    let x_reach = Dijkstra.shortest_path cfg x in
      fun y ->
	try ignore(x_reach y); true
	with Not_found -> false

  (*
    Merge 2 subloops into one, assuming that loop.cfg and loop.header are 
    the same for all loops in [loops]
  *)
  let merge_loops loops =
    let loops_hd = List.hd loops in
    let loops_cfg = loops_hd.cfg in
    let bhtbl = 
      let bhtbl = Hashtbl.create 13 in
      let () = List.iter (
	fun loop ->
	  List.iter (fun bb -> Hashtbl.replace bhtbl (loops_cfg#get_id bb) bb)
	    loop.body 
      ) loops 
      in
	bhtbl
    in
    let loops_body = 
	Toposort.fold (
	  fun bbid cur -> 
	    if Hashtbl.mem bhtbl bbid 
	    then (Hashtbl.find bhtbl bbid) :: cur 
	    else cur
	) loops_cfg []
    in
    let loops_bnodes = 
      List.fold_left (fun cur loop -> loop.back_nodes @ cur) [] loops
    in
    let loops_eedges =
      let ehtbl = Hashtbl.create 13 in
      let () = List.iter (
	fun loop -> 
	  List.iter (
	    fun (fbb,tbb) ->
	      let tbbid = loops_cfg#get_id tbb in
		if not (Hashtbl.mem bhtbl tbbid) then
		  let key = (loops_cfg#get_id fbb, tbbid) in
		    Hashtbl.replace ehtbl key (fbb,tbb)
	  ) loop.exit_edges
      ) loops
      in
	Hashtbl.fold (fun _ b c -> b :: c) ehtbl []
    in
      {
	cfg = loops_hd.cfg;
	header = loops_hd.header;
	body = loops_body;
	back_nodes = loops_bnodes;
	exit_edges = loops_eedges;
	inner_loops = []; (* not being used and thus not implemented*)
	outer_loops = []; (* not being used and thus not implemented*)
      }

  (*
    Combine loops using the given method, [combine_method]
  *)
  let combine_loops combine_method loops =
    match combine_method with
	KeepDisjoint -> loops
      | UniqueHeader ->
	  let lhtbl = Hashtbl.create 13 in
	  let () = List.iter (
	    fun loop -> 
	      let loop_cfg = loop.cfg in
	      let hid = loop_cfg#get_id loop.header in
	      let llist = try Hashtbl.find lhtbl hid with Not_found -> [] in
		Hashtbl.replace lhtbl hid (loop :: llist)
	  ) loops
	  in
	  let llistlist = Hashtbl.fold (fun _ b c -> b :: c) lhtbl [] in
	    List.rev_map merge_loops llistlist

  (**
     Get a list of all natural loops of a reducible CFG.
     The input CFG must be reducible. 
     A reducible CFG can be generated by [make_reducible] or [get_reducible].
  *)
  let get_loops ?(combine_method=KeepDisjoint) cfg =
    let {DOM.dominators=dominators} = DOM.compute_all cfg VCFG.BB_Entry in
    let loop_list = ref []  in
    let cfg_nodes = VCFG.cfg_nodes cfg in
    let _ =
      List.iter (fun block_id ->
	List.iter (fun i -> 
	  if VCFG.is_edge cfg block_id i (* back_edge *) 
	  then (
            let s = Stack.create () in 
	    let bb_i = cfg#find i in
	    let block_bb = cfg#find block_id in
            let loop_i = {
	      cfg = cfg;
	      header = bb_i;
	      body = [];
	      back_nodes = [block_bb];
	      exit_edges = [];
	      inner_loops = [];
	      outer_loops = [];
	    } in
              insert block_bb loop_i s ;
              while not (Stack.is_empty s) do 
		let bb = Stack.pop s in 
		let bbpred = cfg#pred bb in 
		  List.iter (fun bbp -> 
		    let bbp_id = cfg#get_id bbp in
                    let bbp_dom_lst = dominators bbp_id in 
                      if ( List.mem i bbp_dom_lst && bbp_id <> i )  
                      then insert bbp loop_i s ;
		  ) bbpred;
              done; 
              loop_i.body <- bb_i::loop_i.body;
	      
              List.iter (fun lb -> 
		let lb_suc = cfg#succ lb in 
		  List.iter (fun ls -> 
                    if not (List.mem ls loop_i.body ) 
                    then 
                      let ee = (lb,ls) in 
			if not (List.mem ee loop_i.exit_edges) 
			then 
			  loop_i.exit_edges <- ee::loop_i.exit_edges;
		  ) lb_suc
              ) loop_i.body ;
	      
              loop_list := loop_i::!loop_list;     
	  )
	) (dominators block_id)
      ) cfg_nodes
    in
    let combined_loop_list = combine_loops combine_method !loop_list in
      combined_loop_list
	
  (**
     Get a list of all natural loops of a reducible CFG that contains
     the given basic block.
     The input CFG must be reducible.
     A reducible CFG can be generated by [make_reducible] or [get_reducible].

     The returned list of loops is not in any specific ordering.

     Note: One can get a bb containing a label by calling [Vine_cfg.find_label]
  *)
  let get_loops_from_bb ?(combine_method=KeepDisjoint) cfg bb =
    let bbid = cfg#get_id bb in
      List.filter (fun loop ->
		     List.mem bbid (List.map cfg#get_id loop.body))
	(get_loops ~combine_method:KeepDisjoint cfg)
      
  (**
     Unless necessary, you should use [unroll_loop] function instead of
     [unroll_loop_by_backedges].

     [unroll_loop_by_backedges dup_fixup finaldup_fixup cfg body backedges n]
     unroll a loop in [cfg] with the given [body] on the given [backedges] for
     [n] times. 

     [backedges] is a list of backedges to be unrolled.
     All backedges must have their source and destination belong to [body].
     
     Each nodes in [body] will be copied [n] times and the backedges
     backedge will be redirect to the destination node in the next copied
     loop body.
     
     The content (info) in each copied basic block can be changed by providing
     [dup_fixup] function. [dup_fixup cfg bb_map] is a call that
     takes [cfg] and [bb_map] (a map of existing BBs to their
     copies) and correct the content (info) in copied BBs, in their
     predecessors, and in their successors, if necessary.
     [dup_fixup cfg bb_map] should only change the content (info) and not
     the graph flow.
     [dup_fixup] is helpful in correcting info in copied BBs that should not
     be copied directly. For example, copied BB should not have the same label
     as the original one.

     The unrolling edges in the final iteration can be directed by providing
     [finaldup_fixup] function. [finaldup_fixup cfg backedges bb_map] 
     can change both the content (info) the graph flow in the final unrolled
     loop iteration.
  *)
  let unroll_loop_by_backedges dup_fixup finaldup_fixup cfg body backedges n =
    let () = if n <= 0 then 
      failwith "Failure: trying to unroll a loop for less than 1 time." 
      else () in
    let old2new bb_map old_bb =
      try Hashtbl.find bb_map old_bb
      with Not_found -> old_bb
    in
      match body with
	  entry :: _ ->
	    let loop_preds = VUTIL.list_difference (cfg#pred entry) body in
	      List.iter (fun a ->
		cfg#remove_edge a entry) loop_preds;
	      List.iter (fun (a,b) ->
		cfg#remove_edge a b) backedges;
	      (* unroll the first iteration *)
	      let bb_map = dup_bbs cfg body in
	      let new_entry = old2new bb_map entry in
	      let () = 
		List.iter (fun a ->
		  cfg#add_edge a new_entry) loop_preds;
		dup_fixup cfg bb_map in
	      let prev_bb_map = ref bb_map in
		(* do all other iterations *)
		for i = 1 to n-1 do
		  let bb_map = dup_bbs cfg body in
		    List.iter (fun (a,b) ->
		      cfg#add_edge (old2new !prev_bb_map a) (old2new bb_map b)
		    ) backedges;
		    prev_bb_map := bb_map;
		    dup_fixup cfg bb_map;
		done;
		(* handle the backedges of the last iteration *)
		finaldup_fixup cfg backedges !prev_bb_map;
		(* finally, remove the original loop body *)
		List.iter cfg#remove_bb body;
		()
	| _ -> ()
	
  (**
     [unroll_loop_by_backedges dup_fixup finaldup_fixup cfg loop n]
     unroll [loop] in [cfg] for [n] times. 

     [backedges] is a list of backedges to be unrolled.
     All backedges must have their source and destination belong to [body].
     
     Each nodes in [body] will be copied [n] times and the backedges
     backedge will be redirect to the destination node in the next copied
     loop body.
     
     The content (info) in each copied basic block can be changed by providing
     [dup_fixup] function. [dup_fixup cfg bb_map] is a call that
     takes [cfg] and [bb_map] (a map of existing BBs to their
     copies) and correct the content (info) in copied BBs, in their
     predecessors, and in their successors, if necessary.
     [dup_fixup cfg bb_map] should only change the content (info) and not
     the graph flow.
     [dup_fixup] is helpful in correcting info in copied BBs that should not
     be copied directly. For example, copied BB should not have the same label
     as the original one.

     The unrolling edges in the final iteration can be directed by providing
     [finaldup_fixup] function. [finaldup_fixup cfg backedges bb_map] 
     can change both the content (info) the graph flow in the final unrolled
     loop iteration.
  *)
  let unroll_loop dup_fixup finaldup_fixup cfg loop n = 
    unroll_loop_by_backedges dup_fixup finaldup_fixup cfg loop.body
      (List.map (fun a -> (a, loop.header)) loop.back_nodes) n
end

(*
  Below are the loop functionalities for Vine_stmt list cfg type
*)

module L=LOOP(struct type t = V.stmt list end)

let vine_dup_fixup cfg bb_map =
  let new_bbs = (Hashtbl.fold (fun _ b c -> b :: c) bb_map []) in
  let related_bbs = List.fold_left (fun a b ->
    (cfg#pred b) @ a) [] new_bbs in
  let label_map = Hashtbl.create 17 in
  let label_vis = object
    inherit V.nop_vine_visitor
    method visit_stmt stmt =
      match stmt with
	  V.Label l -> 
	    (if not (Hashtbl.mem label_map l)
	    then Hashtbl.add label_map l (V.newlab l));
	    V.SkipChildren
	| _ -> V.DoChildren
  end in
  let name_replace_vis = object
    inherit V.nop_vine_visitor
    method visit_exp exp =
      match exp with
	  V.Name l -> V.ChangeTo (
	    try V.Name (Hashtbl.find label_map l)
	    with Not_found -> V.Name l)
	| _ -> V.DoChildren
  end in
  let label_replace_vis = object
    inherit V.nop_vine_visitor
    method visit_stmt stmt =
      match stmt with 
	  V.Label l -> V.ChangeTo (
	    try V.Label (Hashtbl.find label_map l) 
	    with Not_found -> V.Label l)
	| _ -> V.DoChildren
  end in
    List.iter (fun bb -> List.iter (
      fun st -> ignore(V.stmt_accept label_vis st)) (cfg#get_info bb)) new_bbs;
    List.iter (fun bb -> 
      let newinfo = List.map(V.stmt_accept label_replace_vis) 
	(cfg#get_info bb) in
	cfg#set_info bb newinfo) new_bbs;
    List.iter (fun bb -> 
      let newinfo = List.map(V.stmt_accept name_replace_vis) 
	(cfg#get_info bb) in
	cfg#set_info bb newinfo) related_bbs
      
let vine_finaldup_fixup fwd_jmps cfg backedges bb_map =
  let old2new old_bb =
    try Hashtbl.find bb_map old_bb
      with Not_found -> old_bb
  in
    List.iter (fun (old_from_bb, old_to_bb) ->
      let new_from_bb = old2new old_from_bb in
      (* FIXME: write a new function instead of visitor *)
      let jmp_vis = object
	inherit V.nop_vine_visitor
	method visit_exp exp =
	  match exp with
	      V.Name l -> 
		if (cfg#find_label l == old_to_bb) 
		then (
		  let (new_to_label, new_to_bb) = fwd_jmps cfg l in
		  add_new_edge cfg new_from_bb new_to_bb;
		    V.ChangeTo(V.Name new_to_label))
		else V.SkipChildren
	    | _ -> V.SkipChildren
      end in
      let new_info = 
	List.map (V.stmt_accept jmp_vis) (cfg#get_info new_from_bb)
      in cfg#set_info new_from_bb new_info
    ) backedges
      
(**
   [make_reducible cfg] simplifies [cfg] in a reducible format.
   [cfg] itself will be changed by the call.
*)
let make_reducible = L.make_reducible vine_dup_fixup

(**
   [get_reducible] is a alternative interface of
   [make_reducible]. Unlike [make_reducible], [get_reducible] returns
   a new graph and leaves the original graph untouched.
*)
let get_reducible = L.get_reducible vine_dup_fixup

(**
   Get a list of all natural loops of a reducible CFG.
   The input CFG must be reducible.
   A reducible CFG can be generated by [make_reducible] or [get_reducible].
*)
let get_loops = L.get_loops

(**
   Get a list of all natural loops of a reducible CFG that contains
   the given basic block.
   The input CFG must be reducible.
   A reducible CFG can be generated by [make_reducible] or [get_reducible].
   
   The list is sorted in the nested order. The inner-most loop is at the
   beginning and the outer-most loop is at the end of the list.
   
   Note: One can get a bb containing a label by calling [Vine_cfg.find_label]
*)
let get_loops_from_bb = L.get_loops_from_bb

(**
   Unless necessary, you should use [unroll_loop] function instead of
   [unroll_loop_by_backedges].

   [unroll_loop_by_backedges ?fwd_jmps cfg body backnodes n] 
   changes [cfg] by unrolling the loop containing [body] for [n] times.
   
   [backedges] is a list of backedges to be unrolled. 
   All backedges must have their source and destination belong to [body].
   
   Each nodes in [body] will be copied [n] times and the backedges
   backedge will be redirect to the destination node in the next copied
   loop body.
   
   By setting the optional [fwd_jmps] argument, you can control what happens
   at the last unrolled iteration. [fwd_jmps] is a function that 
   takes in a jump/cjump target label and return a label, basic block pair
   of the new target to be replace in the last unrolled iteration.
   (Default is to go to a new failure node).
   
   For example, fwd_jmps = fun cfg l -> (l, cfg.find_label l) will
   make the last unrolled iteration to go back to the first iteration.
   Another example, fwd_jmps = fun cfg l -> 
   ("CFG_ERROR", cfg.find_label "CFG_ERROR") will redirect all jumps to 
   Label l in the last iteration to the CFG ERROR node.
*)
let unroll_loop_by_backedges =
  fun ?(fwd_jmps = (fun cfg _ -> 
    ("CFG_ERROR", cfg#find VCFG.BB_Error))) -> 
    L.unroll_loop_by_backedges vine_dup_fixup (vine_finaldup_fixup fwd_jmps)
      
(**
   [unroll_loop ?fwd_jmps cfg loop n] unrolls the given loop [loop] in [cfg]
   for [n] times.
   Note that nested loops inside the given loop will not be unrolled.
   
   By setting the optional [fwd_jmps] argument, you can control what happens
   at the last unrolled iteration. [fwd_jmps] is a function that 
   takes in a jump/cjump target label and return a label, basic block pair
   of the new target to be replace in the last unrolled iteration.
   (Default is to go to a new failure node).
   
   For example, fwd_jmps = fun cfg l -> (l, cfg.find_label l) will
   make the last unrolled iteration to go back to the first iteration.
   Another example, fwd_jmps = fun cfg l -> 
   ("CFG_ERROR", cfg.find_label "CFG_ERROR") will redirect all jumps to 
   Label l in the last iteration to the CFG ERROR node.
*)
let unroll_loop = 
  fun ?(fwd_jmps = (fun cfg _ -> 
    ("CFG_ERROR", cfg#find VCFG.BB_Error))) -> 
    L.unroll_loop vine_dup_fixup (vine_finaldup_fixup fwd_jmps)
