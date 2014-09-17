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

   Poking at the internals of a CFG is gone. The following unknown labels
   indicate that you need to switch to using the new (or even the old)
   interface:
   blk_tbl, lbl_tbl, exit_node, vardecls, dom_tbl, idom_tbl, and dom_tree.

   Basic blocks are about to become opaque, so if you are accessing any of
   their fields directly, from outside this file, you should stop. Said
   fields are: id, stmts, succ, pred.

*)
open Vine
open Vine_util
open Vine_alphavary
(* open ExtList *)

module D = Debug.Make(struct let name = "CFG" and default=`Debug end)
open D

module DeclOrderedT =
struct
  type t = Vine.decl
  let compare = Pervasives.compare
end
module DeclSet = Set.Make(DeclOrderedT) ;;

module SSet = Set.Make(String);;

exception DataflowError of string;;

(** A basic block identifier. *)
type bbid =
    BB_Entry (** entry node *)
  | BB_Exit   (** return/exit node *)
  | BB_Indirect (** indirect jump to/from a node *)
  | BB_Error (** jump target when an error occurs *)
  | BB of int (** for normal basic blocks *)
(** If a node has BB_Indirect as it's successor it means it is an indirect
    jump. If it has BB_Indirect as it's predecessor, it means it is the
    possible target of an indirect jump. *)


let bbid_to_string = function
  | BB_Entry     -> "BB_Entry"
  | BB_Exit	 -> "BB_Exit"
  | BB_Indirect	 -> "BB_Indirect"
  | BB_Error	 -> "BB_Error"
  | BB n         -> "BB_"^string_of_int n

module BBid =
struct
  type t = bbid
  let compare = compare
  let hash = function
    | BB_Entry     ->  -1
    | BB_Exit	   ->  -2
    | BB_Indirect  ->  -3
    | BB_Error	   ->  -4
    | BB n         ->   n
end

module BBidSet = Set.Make(BBid)



(* Some important nodes. Should we still use these? *)
let cfg_entry : bbid = BB_Entry      (** entry node *)

let cfg_ret : bbid = BB_Exit       (** exit node *)

let cfg_halt : bbid = BB_Exit      (** exit node *)

let cfg_ijmp : bbid = BB_Indirect      (** target of ijmp *)

let cfg_unknown : bbid = BB_Error   (** unknown nodes *)


(** The data structure that is actually stored for each BB in the CFG.
    This should be invisible to outside users.
*)
type 'a real_bb = {
  mutable id : bbid; (** a unique node id *)
  mutable stmts : 'a; (** a list of stmts (or other representation) in the bb *)
  mutable pred : 'a bb list; (** a set of predecessors nodes *)
  mutable succ : 'a bb list; (** a set of successor nodes *)
}
and 'a bb = 'a real_bb;;

type label = Vine.label

class type ['a] cfg_type =
object ('b)
  method find : bbid -> 'a bb
  method find_label : label -> 'a bb

  method add_edge : 'a bb -> 'a bb -> unit
  method remove_edge : 'a bb -> 'a bb -> unit
  method has_edge : 'a bb -> 'a bb -> bool

    (* like create_bb, but doesn't return the new bb *)
  method add_bb : bbid -> 'a -> unit
  method create_bb : bbid -> 'a -> 'a bb

  method rename_bb : 'a bb -> bbid -> unit
  method remove_bb : 'a bb -> unit
  method iter_bb : ('a bb -> unit) -> unit
  method fold_bb : 'b. ('a bb -> 'b -> 'b) -> 'b -> 'b
  method iter_edges : ('a bb -> 'a bb -> unit) -> unit
  method get_info : 'a bb -> 'a
  method set_info : 'a bb -> 'a -> unit
  method get_id : 'a bb -> bbid
  method succ : 'a bb -> 'a bb list
  method pred : 'a bb -> 'a bb list
  method iter_succ : ('a bb -> unit) -> 'a bb -> unit
  method iter_pred : ('a bb -> unit) -> 'a bb -> unit
  method fold_succ : 'b. ('a bb -> 'b -> 'b) -> 'a bb -> 'b -> 'b
  method fold_pred : 'b. ('a bb -> 'b -> 'b) -> 'a bb -> 'b -> 'b
  method length : int
  method newid : bbid
  method copy : 'b
  method new_empty : 'b
end



(** The type of a control flow graph.
    This should be an opaque type. IE: No poking around with it from outside
    this file.
*)
class ['a] cfg size (iter_labels: (label->unit) -> 'a -> unit) (vardecls:decl list) =
  (* change to true or debug to enable consistency selftests *)
  let selftest = false in
object (self: 'a #cfg_type)
  (* maps bbids to basic blocks *)
  val blktbl = Hashtbl.create (size+4)

  (* maps labels to basic blocks *)
  val lbltbl = Hashtbl.create (size*3/2)

  method find = 
    Hashtbl.find blktbl
  method find_label =
    self#check_graph;
    Hashtbl.find lbltbl

      (* internal use only: add labels in b *)
  method private add_labels b =
    iter_labels (fun l -> Hashtbl.add lbltbl l b) b.stmts
      (* internal use only: remove labels in b *)
  method private remove_labels b =
    iter_labels (fun l -> Hashtbl.remove lbltbl l) b.stmts


  method add_edge b1 b2 =
    self#check_bb b1; self#check_bb b2;
    if selftest && self#has_edge b1 b2
    then failwith "Tried to add a duplicate edge";
    b1.succ <- b2::b1.succ;
    b2.pred <- b1::b2.pred;
    self#check_graph

  method remove_edge b1 b2 =
    self#check_bb b1; self#check_bb b2;
    b1.succ <- List.filter ((!=) b2) b1.succ;
    b2.pred <- List.filter ((!=) b1) b2.pred;
    self#check_graph

  method has_edge b1 b2 =
    self#check_bb b1; self#check_bb b2;
    List.mem b2 (self#succ b1)

  method create_bb b s =
    self#check_not_bbid b;
    self#check_graph;
    self#setmaxid b;
    let bb = {id = b; stmts = s; pred = []; succ = [];} in
    let () = Hashtbl.add blktbl b bb in
    let () = self#add_labels bb in
      bb

  method add_bb b s =
    ignore(self#create_bb b s : 'a bb)

  method rename_bb b id =
    self#check_bb b;
    self#check_not_bbid id;
    Hashtbl.remove blktbl b.id;
    Hashtbl.add blktbl id b;
    b.id <- id;
    self#check_graph

  method remove_bb b =
    self#check_bb b;
    self#check_graph;
    self#remove_labels b;
    self#iter_succ (self#remove_edge b) b;
    self#iter_pred (fun p -> self#remove_edge p b) b;
    self#remove_labels b;
    Hashtbl.remove blktbl b.id


  method succ b =
    self#check_bb b;
    b.succ
  method pred b =
    self#check_bb b;
    b.pred
  method iter_succ f b = List.iter f (self#succ b)
  method iter_pred f b = List.iter f (self#pred b)
  method fold_succ f b i = List.fold_left (fun b a -> f a b) i (self#succ b)
  method fold_pred f b i = List.fold_left (fun b a -> f a b) i (self#pred b)

  method fold_bb f a =
    self#check_graph;
    let res = Hashtbl.fold (fun _ -> f) blktbl a in
      self#check_graph;
      res
  method iter_bb f =
    self#check_graph;
    Hashtbl.iter (fun _ -> f) blktbl;
    self#check_graph

  method iter_edges f =
    self#check_graph;
    self#iter_bb (fun b -> List.iter (f b) (self#succ b));
    self#check_graph;

  method length = Hashtbl.length blktbl

  method get_info b =
    self#check_bb b;
    b.stmts
  method set_info b s =
    self#check_bb b;
    self#remove_labels b;
    b.stmts <- s;
    self#add_labels b

  method get_id b =
    self#check_bb b;
    b.id
    
  val mutable lastid = 0
  method newid =
    lastid <- lastid+1;
    BB lastid


  method private setmaxid = function
      BB i -> if i >= lastid then lastid <- i
    | _ -> ()

  method get_iter_labels_function = iter_labels

  val mutable vardecls = vardecls

  (* This should get replaced by something better *)
  method vardecls = vardecls
  method set_vardecls vds = vardecls <- vds


  method new_empty = {<
    blktbl = Hashtbl.create size;
    lbltbl = Hashtbl.create size;
    lastid = 0;
    vardecls = vardecls;
  >}
  method copy =
    let newblktbl = Hashtbl.create (Hashtbl.length blktbl)
    and newlbltbl = Hashtbl.create (Hashtbl.length lbltbl)
    and copybb {id=id; stmts=stmts; pred=pred; succ=succ} =
      {id=id; stmts=stmts; pred=pred; succ=succ}
    in
    let findbb {id=id} = Hashtbl.find newblktbl id in
    let updatebb ({pred=pred; succ=succ} as bb) =
      bb.pred <- List.map findbb pred;
      bb.succ <- List.map findbb succ
    in
      Hashtbl.iter (fun k bb -> Hashtbl.add newblktbl k (copybb bb)) blktbl;
      Hashtbl.iter (fun k bb -> Hashtbl.add newlbltbl k (findbb bb)) lbltbl;
      Hashtbl.iter (fun _ -> updatebb) newblktbl;
      {<
	blktbl = newblktbl;
	lbltbl = newlbltbl;
	lastid = lastid;
	vardecls = vardecls
      >}



(* for debugging *)
    (** Checks a graph for consistency, if debugging is enabled *)
  method private check_graph =
    if selftest then self#do_check_graph else ()
      
  (** Checks that a bb belongs to this graph, if debugging is enabled *)
  method private check_bb b =
    if selftest then self#do_check_bb b else ()

  (** Checks that a bbid does not already exist in the graph, if debugging
      is enabled *)
  method private check_not_bbid bid =
    if selftest then self#do_check_not_bbid bid else ()

  method private do_check_not_bbid bid =
    try ignore(self#find bid);
      failwith("Tried to add (or rename to) "^bbid_to_string bid
	       ^" but that ID is already used in this CFG.")
    with Not_found -> ()

      (* check that a block belongs to this cfg *)
  method private do_check_bb b =
    try
      if self#find b.id == b then ()
      else failwith("Tried to use "^bbid_to_string b.id^" from a different CFG")
    with Not_found ->
      failwith("Tried to use inexistent block "^bbid_to_string b.id)

  (* check consistency of the cfg *)
  method private do_check_graph =
    let bad p s =
      failwith("Inconsistent edge "^bbid_to_string p.id
	       ^" -> "^bbid_to_string s.id)
    in
    let check_edges b =
      List.iter
	(fun s ->
	   if List.memq b (self#pred s)
	   then self#check_bb s else bad b s)
	(self#succ b);
      List.iter
	(fun p ->
	   if List.memq b (self#succ p)
	   then self#check_bb p else bad p b)
	(self#pred b)
    in
      (* can't use self#iter_bb here, because it calls us *)
      Hashtbl.iter (fun _ -> check_edges) blktbl
	(* FIXME: also check lbltbl *)
end

let empty_cfg size f dl = 
  new cfg size f dl 


(* helper function to get a real_bb from the cfg *)
let real_bb cfg = cfg#find






(*************************** BEGIN Components that make up a CFG ***********)


(******** BEGIN Label table management ***********)


(** The type of a jump target can be direct or indirect.
    Indirect (lablist, is_complete) contains the list
    of possible targets and a boolean indicate whether
    the list is complete or not.
*)
type target = Direct of label
		| Indirect of (label list * bool)
;;


(** Get target labels mentioned in a statement *)
let get_targets_of s =
  let get_labels_of e =  (
    match e with
	Name(l) -> Direct(l)
      | Constant(Int(REG_64,i)) -> Direct(Vine.addr_to_label i)
      | _ -> Indirect ([], false)
  )
  in match s with
      CJmp(_, l1, l2) -> [(get_labels_of l1); (get_labels_of l2)]
    | Jmp(l) -> [(get_labels_of l)];
    | _ -> []
;;

(** get only the direct targets of stmt s *)
let get_direct_targets_of s = 
  let t = 
    List.fold_left (fun acc x -> match x with
                       Direct(l) -> l :: acc
                     | Indirect _ -> acc) [] (get_targets_of s) 
  in
    t
;;

(** 
    @param tl target list
    @return (is_complete?, label list). If list_complete = false,
    then we need to add a jump to the ijmp node.
*)
let targets_to_labels (tl: target list) : (bool *label list) = 
  let is_complete = ref true in 
  let t = 
    List.fold_left (fun acc x -> match x with
	Direct(l) -> l :: acc
      | Indirect(ll,b) ->  (* one incomplete in list is enough *)
	  if (not b) then is_complete := false;
	  ll @ acc) [] tl
  in
    (!is_complete,t)

(** true iff a stmt contains an indirect jump *)
let has_indirect_target s =
  List.exists (fun x -> match x with Indirect _ -> true | Direct _ -> false)
    (get_targets_of s)


let rec vine_iter_labels f s =
    match s with
	Label(l) -> 
	  f l
      | Attr(s', _) -> vine_iter_labels f s'
      | Block _ -> 
	  raise (DataflowError("CFG routines expect flattened  stmts"))
      | _ -> ()

let vine_list_iter_labels f sl =
  List.iter (vine_iter_labels f) sl

(******** END Label table management ***********)


(** add an edge (v1,v2) to the cfg. Maintain invariant that there can
    be at most one edge between any two nodes.
    @param blk_tbl is the cfg
    @param v1 is a blk id inside cfg
    @param v2 is a blk id inside cfg
*)
let add_edge cfg v1 v2  = 
  let v1blk = cfg#find v1 in 
  let v2blk = cfg#find v2 in
    cfg#add_edge v1blk v2blk


(** add an edge (v1,v2) to the cfg. Maintain invariant that there can
    be at most one edge between any two nodes.
    @param blk_tbl is the cfg
    @param v1 is a blk id inside cfg
    @param v2 is a blk id inside cfg
*)
let remove_edge cfg v1 v2 =
  let v1blk = cfg#find v1 in 
  let v2blk = cfg#find v2 in
    cfg#remove_edge v1blk v2blk


(** return true iff (v1,v2) is an edge in the cfg. 
    @param v1 a block id inside the cfg
    @param v2 a block id inside the cfg
    @param blk_tbl the cfg
*)
let is_edge cfg v1 v2 = 
  try
    let v1blk = cfg#find v1 in 
    let v2blk = cfg#find v2 in 
      List.memq v2blk (cfg#succ v1blk)
  with
      Not_found -> false
;;

(** adds a basic block to cfg
    @param cfg - the cfg
    @param cntr - blk id 
*)
let add_bb cfg id stmts = 
  cfg#add_bb id stmts


(** Renames a basic block within the cfg *)
let rename_bb cfg oldbb newbb =
  let b = real_bb cfg oldbb in
    cfg#rename_bb b newbb





(** @return true when control will transfer to a stmt other than the one
    immediately after *)
let rec is_jump = function
    Jmp _ | CJmp _ | Call _ | Return _ | Halt _ -> true
  | Attr(s', ACall) -> true
  | Attr(s', AReturn) -> true
  | Attr(s',_) -> is_jump s'
  | _ -> false

(** @return true when the stmt is a Jmp (optionally wrapped inside attrs) *)
let rec is_jmp = function
    Jmp _ -> true
  | Attr(s,_) -> is_jmp s
  | _ -> false
    
(** @return true when the given stmt is a call *)
let rec is_call = function
    Call _ -> true
  | Attr(s', _) -> is_call s'
  | _ -> false

(** @return true when the given stmt is an assert *)
let rec is_assert = function
    Assert _ -> true
  | Attr(s',_) -> is_assert s'
  | _ -> false

(** @return true when the given stmt is a return *)
let rec is_return = function
    Return _ -> true
  | Attr(s', _) -> is_call s'
  | _ -> false


(** @return true when control will flow from the preceding stmt to this one
    to the one after it, and the stmt has some effect *)
let rec is_sequential = function
    Move _ | Special _ | Assert _ -> true
  | Attr(s',_) -> is_sequential s'
  | _ -> false

(** @return true when control can flow here from somewhere other than the
    preceding stmt *)
let rec is_label = function
    Label _ -> true
  | Attr(s',_) -> is_label s'
  | _ -> false

(** @return true when the stmt has no side effects whatsoever
    (ie, doesn't need to ever be executed) *)
let rec is_ignored = function
    Comment _ | ExpStmt _ | Function _ -> true
  | Attr(s',_) -> is_ignored s'
  | _ -> false


(** @return true if the first statement in the list is a call *)
let first_is_call stmts =
  match stmts with
    | stmt::_ ->
	is_call stmt
    | [] -> false




(** 
  coalesce a bb with one successor, and that successor has only one
  predecessor.  Could be more agressive. 
    Note: updates input blktbl:blk_tbl_t.
*)
let coalesce_bb cfg  = 
  let processed = Hashtbl.create cfg#length in
    (* temporarily store the new stmt lists here to avoid recomputing labels
       many times when we have a long chain of blocks to coalesce. If we
       repeatedly update the block in the CFG instead, it gets slow because
       it will walk the list looking for labels every time. Another solution
       would be to have the CFG update labels lazily. *)
  let newinfo = Hashtbl.create cfg#length in
  let get_info b =
    try Hashtbl.find newinfo (cfg#get_id b)
    with Not_found -> cfg#get_info b
  in
  let set_info b =
    Hashtbl.add newinfo (cfg#get_id b)
  in
  let my_rev_append revstmts stmts =
    match revstmts with
      | x::xs when is_jmp x ->
	  List.rev_append xs stmts
      | _ -> List.rev_append revstmts stmts
  in
  let first_is_jmp_or_notjump stmts =
    match stmts with
      | stmt::_ ->
	  not(is_jump stmt) || is_jmp stmt
      | [] -> true
in
  let rec coalesce blk =
    if not(Hashtbl.mem processed blk.id) then (
      Hashtbl.add processed blk.id ();
      (match cfg#succ blk with
	   (* coalesce successor first, for O(n) total runtime on the appends *)
	 | [suc] -> coalesce suc
	 | _ -> ());
      coalesce_node blk
    )
  and coalesce_node blk = 
    match blk with
	(* coalesce normal blocks *)
      | {id=BB _; pred=[{id=(BB _); succ=[predsucc]} as predblk]}
	  when predblk != blk && predsucc == blk
	    -> 
	  let revstmts = List.rev(cfg#get_info predblk) in
	  let blkstmts = get_info blk in
	    if first_is_jmp_or_notjump revstmts then (
	      dprintf "coalescing %s and %s" (bbid_to_string(cfg#get_id predblk)) (bbid_to_string(cfg#get_id blk));
	      List.iter (cfg#add_edge predblk) (cfg#succ blk);
	      cfg#remove_bb blk; (* must remove before adding stmts to pred *)
	      Hashtbl.remove newinfo (cfg#get_id blk);
	      set_info predblk (my_rev_append revstmts blkstmts)
	    )
      | _ -> ()
  in
    cfg#iter_bb coalesce;
    Hashtbl.iter (fun k v -> cfg#set_info (cfg#find k) v) newinfo
;;


(** Splits a list of statements into groups that can be put together in a basic
    block. This is accomplished by spliting after every jump and before any
    label that is preceded by non-labels.
    Block coalescing may be able to further join groups. *)
let split_stmts (sl:stmt list) : (stmt list * stmt option) list =
  let f (sll,curl,onlylabs) s =
    match s with (* hmm, not much nicer than a bunch of if/then/else *)
	_ when is_jump s ->
	  ((s::curl)::sll, [], true)
      | _ when is_sequential s ->
	  (sll, s::curl, false)
      | _ when is_label s ->
	  if onlylabs
	  then (sll, s::curl, true)
	  else (curl::sll, [s], true)
      | _ when is_ignored s ->
	  (sll, s::curl, onlylabs)
      | _ ->
	  failwith "CFG: ack, don't know what to do with this type of statement"
  in
  let (sll,last,_) = List.fold_left f ([],[],true) sl in
    List.rev_map (fun x -> (List.rev x, match x with l::_->Some l |_->None))
      (if last = [] then sll else last::sll)


(*
  Breaks a list of statements into basic blocks, and puts them in a CFG.
  This function doesn't do much analysis, so it assumes that every label might
  be the start of a new block. coalesce_bb can be used to merge blocks that
  could be merged.
*)
let stmts_to_bb (dl:decl list) (sl:stmt list) : stmt list cfg = 
  let sll = split_stmts sl in
  let size = List.length sll in
  let cfg = new cfg size vine_list_iter_labels dl in
  let entry_node = cfg#create_bb BB_Entry [Comment "entry node"] in
  let exit_node = cfg#create_bb BB_Exit [Comment "exit node"] in
  let error_node = cfg#create_bb BB_Error [Label "CFG_ERROR"; Assert(exp_false)] in
  let indirect_node = cfg#create_bb BB_Indirect [] in
  let add_jmp_targets blk lbls =
    List.iter (fun lbl ->
		 try
		   cfg#add_edge blk (cfg#find_label lbl)
		 with 
		     Not_found ->  	  
		       cfg#add_edge blk (cfg#find BB_Error)
	      ) lbls
  in
  let rec add_edges bb next s' =
    match s' with
	    Jmp _ 
	  | CJmp _ -> 
	      let targetlist = get_targets_of s' in 
	      let (b, lbls) = targets_to_labels  targetlist
	      in 
		add_jmp_targets bb lbls;
		if (not b) then cfg#add_edge bb indirect_node else ()
	  | Return _
	  | Halt _ -> cfg#add_edge bb exit_node
	  | Attr(s'',_) -> add_edges bb next s''
	  | Block _ -> raise 
	      (DataflowError("Stmts must be flattened to create flow graph"))
	  | Special _ ->
	      cfg#add_edge bb error_node
	  | _ ->
	      (match next with
		   Some n ->
		     cfg#add_edge bb n
		 | _ ->
		     cfg#add_edge bb exit_node

	      )
  in
  let add_edges_block bb next =
	  (match cfg#get_info bb with
	       _::_ as l ->
		 add_edges bb next (list_last l)
	     | [] ->
		 prerr_endline "aij doesn't think we should have an empty list here"
	  )
  in
  let revblocks =
    List.fold_left (fun blocks (s,l) -> cfg#create_bb (cfg#newid) s::blocks)
      [] sll
  in
  let first =
    List.fold_left (fun next bb -> add_edges_block bb next; Some bb)
      None revblocks
  in
  let () = match first with
      Some first -> 
	cfg#add_edge entry_node first
    | None ->
	cfg#add_edge entry_node exit_node
  in
  let () = if false then coalesce_bb cfg in
    if cfg#pred indirect_node = [] then cfg#remove_bb indirect_node;
    if cfg#pred error_node = [] then cfg#remove_bb error_node;
    cfg




(********* External interface to the CFG ******************)



(** Create the control flow graph for a program. Like stmts_to_bb but
    flattens the scope so you don't need to.
    This should be used instead of stmts_to_bb because in the future it
    could take better advantage of scoping.
*)
let prog_to_cfg ((dl,sl):program) =
  let (dl,sl) = Vine_alphavary.descope_program (dl,sl) in
    (* the cfg routines shouldn't be worried about "special" jumps. *)
    (*  let sl = replace_special_jumps sl in *)
    stmts_to_bb dl sl


let func_to_cfg = function
  | Function(name,topt, args, b, Some(Block(dl,sl))) ->
      let (dl,sl) = Vine_alphavary.descope_program ((args@dl), sl) in
	stmts_to_bb dl sl
  | _ -> failwith "func_to_cfg must be given a function definition"

let trace_to_cfg ((dl,sl):program) =
  let (dl,sl) = Vine_alphavary.descope_program (dl,sl) in
  let cfg = new cfg 1 vine_list_iter_labels dl in
  let entry_node = cfg#create_bb BB_Entry [Comment "entry node"] in
  let exit_node = cfg#create_bb BB_Exit [Comment "exit node"] in
  let n = cfg#create_bb cfg#newid sl in
    cfg#add_edge entry_node n;
    cfg#add_edge n exit_node;
    cfg

(** return a list of all basic blocks id's in a cfg *)
let cfg_nodes (cfg: 'a #cfg) =
  cfg#fold_bb (fun b r -> b.id::r) []

(** @return true if the cfg contains the given BB *)
let cfg_has_bb cfg bb =
  try ignore(real_bb cfg bb); true with Not_found -> false

(** @return the entry node for the CFG *)
let entry_node cfg = BB_Entry

(** @return the exit node for the CFG *)
let exit_node cfg = BB_Exit

(** @return the number of basic blocks in a cfg *)
let cfg_numnodes cfg = cfg#length

(** @return the bb corresponding to the given label *)
let find_label cfg l = (cfg#find_label l).id

(** @return true if the cfg contains the given label *)
let cfg_has_label cfg l =
  try ignore(find_label cfg l); true with Not_found -> false


(** returns the statements in a bb *)
let bb_stmts cfg bb = cfg#get_info (cfg#find bb)

(** changes the statements in a bb *)
let bb_set_stmts cfg bb stmts =
  let rbb = real_bb cfg bb in
    rbb.stmts <- stmts
      (* FIXME: update the label table *)

(** return the predecessors of a bb *)
let bb_pred cfg bb = List.map cfg#get_id (cfg#pred (cfg#find bb))

(** return the successsors of a bb *)
let bb_succ cfg bb = List.map cfg#get_id (cfg#succ (cfg#find bb))

(** map a CFG's nodes *)
let map (f:bbid -> 'b) (cfg:'a cfg) iter_labels : 'b cfg =
  let size = cfg_numnodes cfg in
  let newcfg = new cfg size iter_labels cfg#vardecls in
  let () = cfg#iter_bb (fun b -> newcfg#add_bb b.id (f b.id)) in
  let () = cfg#iter_edges (fun a b -> add_edge newcfg a.id b.id ) in
    newcfg



(* A data structure that allows fast concatenation and getting of the
   first element *)
module Trace =
struct
  type t = Vine.stmt list bb * rest
  and rest = Empty | Other of t | Concat of rest * t

  let concat (x,xt) y =
    match xt with
      | Empty -> (x, Other y)
      | Other _ | Concat _ -> (x, Concat(xt, y))

  let rec fold f i (t,r) =
    fold_rest f (f t i) r
  and fold_rest f i = function
    | Empty -> i
    | Other t -> fold f i t
    | Concat(r,t) -> fold f (fold_rest f i r) t
end


let cfg_to_prog (cfg: Vine.stmt list #cfg) entry_node exit_node =
  let rec find_label = function
      Label l::_ -> Some l
      | Attr(x,_)::xs -> find_label (x::xs)
      | x::xs when is_ignored x -> find_label xs
      | _ -> None
  in
  let labs = Hashtbl.create (cfg_numnodes cfg) in
  let rec get_label (bb:bbid) =
    let lab = 
    try fst(Hashtbl.find labs bb)
    with Not_found ->
      (try
	 let (l,need_to_add) = match find_label(cfg#get_info(cfg#find bb)) with
	     Some l -> (l,false)
	   | None -> (newlab "autolabel", true)
	 in
	   Hashtbl.add labs bb (l,need_to_add);
	   l
       with
	   Not_found when bb <> cfg_unknown -> 
	     (* IF the block doesn't exist, make it a jump to unknown*)
	     dprintf "get_label: %s not found, trying unknown" (bbid_to_string bb);
	     get_label cfg_unknown
(*
	 | Not_found when bb <> cfg_chop_outside ->
	     dprintf "get_label: %s not found, trying chop_outside" (bbid_to_string bb);
	     get_label cfg_chop_outside (* FIXME: why are these different? --aij*)
*)
      )
    in
      dprintf "get_label %s -> %s" (bbid_to_string bb) lab;
      lab
  in
  let jumps = Hashtbl.create cfg#length in
  let has_jump src =
    match cfg#get_info src with
      | [] -> false
      | stmts ->
	  let last = list_last stmts in
	    is_jump last && not(is_call last)
  in
  let ensure_jump src dst =
    let srcid = cfg#get_id src in
      if not(has_jump src)
      then match cfg#succ src with
	| [_] ->
	    Hashtbl.add jumps srcid (Jmp(Name(get_label (cfg#get_id dst))))
	| _ ->
	    failwith("cfg_to_prog: no jump at end of block with > 1 succ: "
		     ^ bbid_to_string srcid)
  in
  let nodes =
    let traces = Hashtbl.create cfg#length in
    let joined = Hashtbl.create cfg#length in
    (* hashtbl maps startid bbid to reverse list of bbs in the trace *)
    let () =
      cfg#iter_bb (fun b -> Hashtbl.add traces (cfg#get_id b) (b,Trace.Empty))
    in
    let rec join_trace cond headid =
      try
	let trace = Hashtbl.find traces headid in
	let tail = fst trace in
	let rec find_succ = function
	  | [] -> ()
	  | suc::rest ->
	      if cond tail suc
	      then (
		try 
		   let sucid = cfg#get_id suc in
		   let tailid = cfg#get_id tail in
		   let trace = Trace.concat (Hashtbl.find traces sucid) trace in
		     dprintf "join_traces: combining %s and %s"
		       (bbid_to_string tailid)
		       (bbid_to_string sucid);
		     Hashtbl.add joined (tailid, sucid) ();
		     Hashtbl.replace traces headid trace;
		     Hashtbl.remove traces sucid;
		     join_trace cond headid
		with Not_found -> find_succ rest
	      )
	      else find_succ rest
	in
	  find_succ (cfg#succ tail)
      with Not_found -> (* this trace was already joined *)
	()
    in
    let join_traces cond = 
      let worklist = Hashtbl.fold (fun k v w -> k::w) traces [] in
	List.iter (join_trace cond) worklist
    in
    let joinable x =
      not(List.mem (cfg#get_id x) [entry_node; exit_node; BB_Error; BB_Indirect])
    in
    let () = (* join traces without jumps *)
      join_traces (fun b suc -> 
		     pdebug (bbid_to_string(cfg#get_id b));
		     joinable b && not(has_jump b) )
    in
    let () = (* join other traces (if we cared, we could remove some jumps) *)
      join_traces (fun b suc -> joinable b)
    in
    let () = (* join the entry node, NOT with the trace containing the exit *)
      join_trace
	(fun b suc ->
	   let suctrace = Hashtbl.find traces (cfg#get_id suc) in
	     cfg#get_id(fst suctrace) <> exit_node
	)
	entry_node
    in
    let () = (* add jumps for edges that need them *)
      cfg#iter_bb
	(fun b -> 
	   cfg#iter_succ
	     (fun s ->
		if not(Hashtbl.mem joined (cfg#get_id b, cfg#get_id s))
		then ensure_jump b s
	     )
	     b
	)
    in
    (*let () = if debug then (
      pdebug "Printing traces:";
      Hashtbl.iter
	(fun start trace ->
	   pdebug (String.concat ", " 
		     (List.rev_map (bbid_to_string <@ cfg#get_id) trace)) )
	traces
    )
    in *)
      (* Note that for this to work, the entry node must not be in the same
	 trace as the exit node. For now, we simply don't join the entry node,
	 but later we may want to join it when it is safe. *)
    let revordered_traces, exittrace =
      Hashtbl.fold
	(fun start trace (traces,exittrace) ->
	     if start = entry_node then (traces, exittrace)
	     else
	       let tail = cfg#get_id (fst trace) in
		 if tail = exit_node then (traces, Some trace)
		 else (trace::traces, exittrace)
	)
	traces
	([Hashtbl.find traces entry_node], None)
    in
    let revordered_traces = match exittrace with 
      | Some x ->
	      (x::revordered_traces)
      | None ->
	  try ignore(cfg#find exit_node);
	    failwith "none of my traces conained the exit node"
	  with Not_found ->
	    pwarn "exit node not found. Program won't terminate.";
	    revordered_traces
    in
      List.fold_left
	(fun res t -> Trace.fold (fun x r -> x::r) res t)
	[]
	revordered_traces
  in
  let get_stmts b =
    let bb = cfg#get_id b in
    let jump = try Some(Hashtbl.find jumps bb) with Not_found -> None in
    let lab =
      try
	match Hashtbl.find labs bb with 
	  | (l,true) -> Some l
	  | (_,false) -> None
      with Not_found -> None
    in
    let stmts = cfg#get_info b in
      match (lab, jump) with
	| (Some l, Some j) ->    Label l :: stmts@[j]
	| (Some l, None) ->      Label l :: stmts
	| (None,Some j) ->	 stmts@[j]
	| (None,None) ->	 stmts
  in
  let stmts = List.flatten(List.map get_stmts nodes) in
    ((cfg#vardecls, stmts) : Vine.program)
	       

let normal_cfg_to_prog cfg = 
  List.iter (fun bbid ->
	       try (
		 let bb = cfg#find bbid in 
		   match (bb.succ,bb.pred) with
		       [],[] -> cfg#remove_bb bb
		     | _ -> ()
	       )
	       with
		   Not_found -> ()
	    ) [BB_Indirect;BB_Error;BB_Exit];
  cfg_to_prog cfg (entry_node cfg) (exit_node cfg)

(* maybe move the signature to a different module? *)
module type GSIG =
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
  val fold_vertex : (bbid -> 'a -> 'a) -> t -> 'a -> 'a
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

  val succ : t -> V.t -> V.t list
  val pred : t -> V.t -> V.t list
  val remove_vertex : t -> V.t -> unit
end


(** Graph structure for use with the ocamlgraph module *)
module MakeG (Lang : sig type t end) =
struct
  module V =  struct
    (** The vertex type *)
    type t = bbid
    let hash = Hashtbl.hash
    let equal = (=)
    let compare = compare
  end
  module E = struct
    type t = V.t * V.t
    type label = t
    let label = (fun x -> x)
    let src = fst
    let dst = snd
  end

  type vertex = V.t
  type t = Lang.t cfg

  let is_directed = true

  let nb_vertex cfg = cfg#length
  let is_empty cfg = nb_vertex cfg = 0

  let iter_vertex f g = g#iter_bb (fun b -> f b.id)
  let fold_vertex f (g:t) a = g#fold_bb (fun b -> f b.id) a 
  let iter_edges_e f (g:t) =
    g#iter_edges (fun x y -> f (g#get_id x, g#get_id y))
  let iter_succ f g v = List.iter f (bb_succ g v)
  let iter_pred f g v = List.iter f (bb_pred g v)
  let in_degree g v = List.length (bb_pred g v)
  let out_degree g v = List.length (bb_succ g v)

  let fold_succ f g v a = List.fold_left (fun acc x -> f x acc) a (bb_succ g v)

  let fold_succ_e f g v = fold_succ (fun x a -> f (v,x) a) g v

  let iter_succ_e f g v =  fold_succ (fun x () -> f (v,x)) g v ()

  let fold_pred f g v a = List.fold_left (fun acc x -> f x acc) a (bb_pred g v)

  let fold_pred_e f g v = fold_pred (fun x a -> f (x,v) a) g v

  let iter_pred_e f g v = fold_pred (fun x () -> f (x,v)) g v ()

  let vertex_name = bbid_to_string

  let succ g v = bb_succ g v 

  let pred g v = bb_pred g v

  let remove_vertex g v = g#remove_bb (g#find v)

end



module NodePartition = 
struct


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
    
  module Make (G:G) : NP with type t= G.t and type vertex = G.V.t
  = 
  struct
    type t = G.t
    type vertex = G.V.t

    module S = Set.Make(G.V) 
    module HBB = Hashtbl.Make(G.V)


    let partition_nodes cfg root = 
      let h = HBB.create 65537 in 
      let rec visit v = 
	if not (HBB.mem h v) then (
	  HBB.add h v ();
	  G.iter_succ (visit) cfg v
	)
      in
      let () = visit root in 
	G.fold_vertex 
	  (fun v (racc,uacc) -> 
	     if (HBB.mem h v) then 
	       (S.add v racc, uacc) (* reachable *)
	     else 
	       (racc, S.add v uacc) (* unreachable *)
	  ) cfg (S.empty, S.empty)
	  
    let reachable g v = fst (partition_nodes g v)
      
    let unreachable g v = snd (partition_nodes g v)
      
    let has_unreachable g v = S.is_empty (unreachable g v)
      
    let fold_unreachable f g v a =
      S.fold (f) (unreachable g v) a
	
    let fold_reachable f g v a =
      S.fold (f) (reachable g v) a

    let iter_unreachable f g v  = 
      S.iter (f) (unreachable g v)

    let iter_reachable f g v = 
      S.iter (f) (reachable g v)
	
  end
end
module G = MakeG(struct type t = stmt list end);;
module Toposort = Graph.Topological.Make(G);;
module Component = Graph.Components.Make(G);;
module Bfs = Graph.Traverse.Bfs(G);;
module Dfs = Graph.Traverse.Dfs(G);;
module HBB = Hashtbl.Make(G.V)
module BBSet = Set.Make(G.V)
module NP = NodePartition.Make(G);;

let remove_unreachable ?(from=BB_Entry) ?(consider_ind=true) g = 
  let reach,unreach = NP.partition_nodes g from in
    if (consider_ind && (NP.S.mem  BB_Indirect reach)) then
      ()
    else
      NP.S.iter 
	(fun x ->  
	   dprintf "Removing unreachable node %s" (bbid_to_string x);
	   g#remove_bb (g#find x)
	) unreach

(* There is a copy of this in ssa.ml. Fix any bugs there too *)
let well_defined g  = 
  let node_exists x  = 
    try (ignore(g#find x); true)
    with Not_found -> false 
  in
  let wfalse b str = b || (pwarn str; false) in
    wfalse (node_exists BB_Entry) "No BB_Entry in CFG"
    && wfalse (node_exists BB_Exit) "No BB_Exit in CFG"
    && wfalse (not(node_exists BB_Indirect)) "CFG contains BB_Indirect"
    && wfalse (NP.S.is_empty(NP.unreachable g BB_Entry)) "CFG contains unreachable nodes"


(* statements we give the chop_outside node when we create one *)
let chop_outside_stmts = [Label "CFG_CHOP_OUTSIDE"; 
			  Call(None,Name("chop_outside"), [])]

let default_rewriter newcfg (b,s) target chopend_id outside = 
  let ensure_edge cfg n1 n2 = 
    if not(cfg#has_edge n1 n2) then 
      cfg#add_edge n1 2 
  in
    if (b.id = chopend_id) && 
      target == outside then () 
    else
      ensure_edge newcfg b target 
	

    
    

(** Calculate the chop of a CFG.
    
    For now it only works on a [stmt list cfg], but that is only due to
    ocamlgraph's lack of polymorphism.
    
    Note that because of the cfg is imperative, you need to be careful about
    what is copied and what is not. Use the source.
*)
let chop  (cfg: 'a #cfg) a b outsideid  =
  let (ra,rb) = (real_bb cfg a, real_bb cfg b) in
  let made_temporary_edge =
    if cfg#has_edge rb ra then false
    else (cfg#add_edge rb ra; true) (* temporary backedge *)
  in
  let (_,scc) = Component.scc cfg in
  let group = scc a in
  let newcfg = new cfg 8 cfg#get_iter_labels_function cfg#vardecls in
  let (outside: 'a bb) =
    try 
      let oldoutside = cfg#find outsideid in
	newcfg#create_bb outsideid (cfg#get_info oldoutside)
    with Not_found -> newcfg#create_bb outsideid chop_outside_stmts
  in
  let addifgroup v =
    if scc v.id = group && v.id <> outsideid 
    then
      newcfg#add_bb (cfg#get_id v) (cfg#get_info v)
    else ()
  in
  let () = cfg#iter_bb addifgroup in
  let () =
    (* remove temporary backedge *)
    if made_temporary_edge then cfg#remove_edge rb ra 
  in
  let ensure_edge cfg a b = if not(cfg#has_edge a b) then cfg#add_edge a b in
  let chopend_id =  b in 
  let copy_edges b =
    if b == outside then () else
    let b' = cfg#find b.id in
      List.iter
	(fun s ->
	   let target = 
	     try 
	       newcfg#find s.id 
	     with Not_found ->  outside 
	   in
	     if (b.id = chopend_id) && 
	       target == outside then () 
	     else
	       ensure_edge newcfg b target  
	)
	(cfg#succ b')
  in
  let () = newcfg#iter_bb copy_edges in
  if (a <> cfg_entry) then (
    let () = add_bb newcfg cfg_entry [Comment("entry")] in
    let () = add_edge newcfg cfg_entry a in
    ()
   );
    newcfg



(** remove edges which trail outside the chop to the chop_outside
    node *)
let remove_chop_trailing_edges cfg chop_outside outside_label =
(*  let () = mk_lbl_tbl cfg
    (* FIXME: temporary hack to deal with inconsistent label table *)
  in 
*)
  let rename_jumps =
    let lab_not_in_chop l =
      try ignore(find_label cfg l); false with Not_found -> true
    in
    let vis = object
      inherit nop_vine_visitor
      method visit_exp = function
	  Name n when lab_not_in_chop n -> 
	    dprintf "remove_chop_trailing_edges: renaming '%s'" n;
	    ChangeTo(Name outside_label)
	| Name n ->
	    dprintf "remove_chop_trailing_edges: leaving '%s'" n;
	    DoChildren
	| _ -> DoChildren
    end
    in
      stmt_accept vis
  in
  let remove_trailing_edges blk =
    cfg#set_info blk (List.map rename_jumps (cfg#get_info blk))
  in
    List.iter remove_trailing_edges (cfg#pred (cfg#find chop_outside))
;;

(** Removes call and return stmts from a CFG. This is useful when, eg the
    CFG is the chop of a supergraph which you want to convert to IR. *)
let remove_call_returns cfg =
  let remove blk =
    blk.stmts <- List.filter (fun x -> not(is_call x || is_return x)) blk.stmts
  in
    cfg#iter_bb remove

(**
   Given a list of begin chop address and end chop addresses, we
   create a new graph where a virtual node cfg_chop_start is linked to all
   begin chop addresses, and all end addresses are link to a cfg_chop_end
   address.  
 
   @param cfg the cfg
   @param sids the list of start blk ids
   @param eids the list of end blk ids
   @return (virtual chop start node id, virtual chop end node id, cfg)
*)
let create_chop_virtual_cfg cfg sids eids = 
  let (chop_start, chop_end) = (cfg#newid, cfg#newid) in
  let () = add_bb cfg chop_start [] in 
  let () = add_bb cfg chop_end [] in 
    (* add a link from cfg_chop_start to each start addr *)
    List.iter
      (fun x -> 
	 let b = cfg#find x in 
	   (* This could remove edges that are within the chop. Do we
	      care? --aij *)
	   List.iter (fun predid ->cfg#remove_edge predid b) (cfg#pred b);
	   add_edge cfg chop_start x )
      sids;
    List.iter
      (fun x ->
	 let b = cfg#find x in
	   List.iter (cfg#remove_edge b) (cfg#succ b);
	   add_edge cfg x chop_end )
      eids;
    (chop_start, chop_end, cfg)
;;
  
let create_callgraph _ = failwith "create_callgraph not reimplemented yet"
(*
(** a callgraph is a graph from caller->callee.
    @param fl is a list of (function,cfg pairs)
    @return (cg, cgtbl) pair where cg is the callgraph and cgtbl maps
                     a cg blk id to a function name
*)
let create_callgraph fl = 
  let cfgtbl = Hashtbl.create (List.length fl) in 
  let () = List.iter (fun (f,cfg) ->
    match f with 
	Function(n,_,_,_,_) -> Hashtbl.add cfgtbl n cfg
      | _ -> failwith "Non-function passed in?") fl in 
  let cg = {
    blk_tbl=Hashtbl.create (Hashtbl.length cfgtbl);
    vardecls = [];
    exit_node = cfg_ret; (* updated at the end *)
    lbl_tbl = Hashtbl.create 0;
    dom_tbl = Hashtbl.create 0;
    idom_tbl = Hashtbl.create 0;
    dom_tree = Hashtbl.create 0;
  } in 
  let add_initial_nodes () =
    add_bb cg cfg_entry [];
    add_bb cg cfg_ret [];
    add_bb cg cfg_halt [];
    add_bb cg cfg_ijmp [];
    add_bb cg cfg_unknown []
  in
  let cgtbl = Hashtbl.create (Hashtbl.length cfgtbl) in 
    (* for one cfg, find all calls *)
  let get_calls fgraph =
    List.flatten 
      (Hashtbl.fold (fun blkid blk acc ->
	let call_stmts = List.filter is_call blk.stmts in 
	  call_stmts :: acc) fgraph.blk_tbl [])
  in
    add_initial_nodes ();
    (* add all functions to the cg *)
    let _ = Hashtbl.fold (fun fname fgraph blkno ->
      Hashtbl.add cgtbl fname blkno; 
      add_bb cg blkno []; (* add a block for this function to the cfg *)
      blkno+1) cfgtbl 1 in 
      (* add a link from caller -> callee *)
      Hashtbl.iter (fun fname fgraph ->
	let callerid = Hashtbl.find cgtbl fname in 
	let calls = get_calls fgraph in 
	  List.iter (fun call ->
	    let callee = (match call with 
		Call(_,n,_) ->n )
	    in
	    let calleeid = Hashtbl.find cgtbl callee in 
	      add_edge cg callerid calleeid
	  ) calls
      ) cfgtbl;
      (cg,cgtbl)
;;
*)

(** Adds returns for imlicit returns at the end of void functions

    FIXME: doesn't check the return type of functions
 *)
let add_returns cfg = 
  List.iter 
    (fun bb ->
       let blk = real_bb cfg bb in
       if not(is_jump(list_last blk.stmts))
       then blk.stmts <- blk.stmts @ [Return None]
    )
    (bb_pred cfg cfg_ret)
;;

(** Undoes the name mangling with a % that was done to make each function
    call site unique. *)
let function_unmangle name =
  try
    String.sub name 0 (String.rindex name '%')
  with Not_found -> name

(** takes in the supergraph and a mapping from 
    sid -> (function name, id) and returns a list of cfg's

    Argh, that isn't actually enough information, so we take the 
    original function CFGs too.
*)
let supergraph_to_cfgs (supergraph: 'a #cfg) sids_to_fids cfgs =
    (* we map every function name to its own cfg (funtbl). For correctness, I
       believe if you add a function multiple times (e.g., for context
       sensitivity), each instance should have its own name since each
       instance may have different code blocks in the slice.
    *)
  let sids_to_fid sid =
    try let (fn,fid) = sids_to_fids sid in
      dprintf "sid %s -> fid %s:%s"
	(bbid_to_string sid) fn (bbid_to_string fid);
      fid
    with Not_found ->
      dprintf "Warning: sid %s does not map to any fid. Mapping to itself"
	(bbid_to_string sid);
      sid
  in
  let oldcfgs = Hashtbl.create (List.length cfgs) in
  let () =
    List.iter (function (Function(f,_,_,_,_),cfg) -> 
		 Hashtbl.add oldcfgs f cfg
	       | _ -> ()
	      ) cfgs
  in
  let oldcfgs nm =
    try Hashtbl.find oldcfgs nm with Not_found -> supergraph
  in
  let funtbl = Hashtbl.create 13 in
    (* given an sid, we return the corresponding cfg, creating a new
       one if necessary *)
  let get_cfg_of sid =
    let nm =  (* to get a cfg, first resolve the sid to a name *)
      try fst(sids_to_fids sid)
      with Not_found -> "_superchop_abstract_nodes_"
    in (* then resolve the name to a cfg *)
      try
	Hashtbl.find funtbl nm
      with
	  Not_found -> (
	    let cfg = new cfg 13 vine_list_iter_labels [] in
	      Hashtbl.add funtbl nm cfg;
	      cfg
	  )
  in
  let add_superblock sblk =
    let sid = supergraph#get_id sblk in
    let cfg = get_cfg_of sid in
    let fid = sids_to_fid sid in
      cfg#add_bb fid (supergraph#get_info sblk)
  in
  let add_edges newcfg oldcfg =
    (* can't just iterate over all edges in oldcfg because
       oldcfg might be supergraph *)
    newcfg#iter_bb
      (fun newb ->
	 try
	 let oldb = oldcfg#find newb.id in
	   oldcfg#iter_succ
	     (fun succ ->
		let newsucc =
		  try newcfg#find (oldcfg#get_id succ)
		  with Not_found ->
		     try newcfg#find_label "CFG_CHOP_OUTSIDE" 
		     with Not_found ->
		       newcfg#create_bb (newcfg#newid) chop_outside_stmts
		in
		  newcfg#add_edge newb newsucc
	     )
	     oldb
	 with Not_found ->
	   (* newb isn't in the old cfg, it must be a chop_outside node *)
	   () (* newcfg#add_edge newb (newcfg#find BB_Exit) *)
      )
  in
    supergraph#iter_bb add_superblock;
    Hashtbl.iter
      (fun nm newcfg -> add_edges newcfg (oldcfgs nm))
      funtbl;
    funtbl
;;

(** Like supergraph_to_cfg, but cleans up partial functions *)
let superchop_to_cfgs superchop sids_to_fids cfgs snode enode outside =
  let funtbl = supergraph_to_cfgs superchop sids_to_fids cfgs in
(*  let sids_to_fid sid =
    try snd(sids_to_fids sid)
    with Not_found -> sid
  in *)
  let set_entry sid =
 	 let (func,fid) = sids_to_fids sid in
	 let cfg = Hashtbl.find funtbl func in
	   if fid <> cfg_entry then rename_bb cfg fid cfg_entry
  in
  let join_ret sid =
    let (func,fid) = sids_to_fids sid in
    let cfg = Hashtbl.find funtbl func in
      (try ignore(real_bb cfg cfg_ret)
       with Not_found ->
	 add_bb cfg cfg_ret  [Label "CFG_RET"; Comment "Added by superchop_to_cfgs"]);
      add_edge cfg fid cfg_ret

  in
  let add_nodes name cfg =
    (* add all required nodes to the function CFG *)
    (* remove_chop_trailing_edges cfg outside; *)
    if not(cfg_has_bb cfg cfg_entry) then (
	dprintf "Adding entry node for function %s" name;
	add_bb cfg cfg_entry [Label "MISSING_ENTRY"];
	(* add_edge cfg cfg_entry outside *)
    )
  in
  let enodes =
    (* FIXME: do we actually want to do this for pred outside? *)
    let all = List.rev_append (bb_pred superchop enode) (bb_pred superchop outside) in
    let all = list_unique all in
      List.filter ((<>) outside) all
  in
    List.iter set_entry (bb_succ superchop snode);
    Hashtbl.iter add_nodes funtbl;
    List.iter join_ret enodes;
    (* Hashtbl.iter (fun name -> add_returns) funtbl; *)
    funtbl

let cfgs_to_prog vardecls funtbl =
  let addcfg name cfg (stuff,funs) =
    let () = dprintf "cfgs_to_prog: translating %s.%!" name in
      (* FIXME: Hack: make empty functions for empty CFGs. Why do we have
	 empty CFGs now? *)
      try ignore(real_bb cfg cfg_entry);
    let (vars,stmts) = normal_cfg_to_prog cfg in
      match name with
	  "" -> (stmts, funs)
	| _ ->
	    let nm = function_unmangle name in
	      if nm = name then
		(stuff, Function(nm,None,[],false,Some(Block([],stmts)))::funs)
	      else (stuff, Comment("Skipping external function "^name)::funs)
      with Not_found ->
	(* should we really be outputing an external function for empty ones? *)
	(stuff, Function(name,None,[],true,None)::funs)
  in
  let (stuff,funs) = Hashtbl.fold addcfg funtbl ([],[]) in
    (vardecls, List.rev_append funs stuff)
;;

let supergraph_to_prog scfg mapping cfgs =
  let funtbl = supergraph_to_cfgs scfg mapping cfgs in
    cfgs_to_prog scfg#vardecls funtbl

let superchop_to_prog scfg mapping cfgs snode enode outside =
  let funtbl = superchop_to_cfgs scfg mapping cfgs snode enode outside in
    cfgs_to_prog scfg#vardecls funtbl

(**
   @param fl is a list of (function,cfg) pairs.

   FIXME: make this a subclass of cfg
   FIXME: build the supergraph from a program
*)
let create_supergraph (fl : (stmt * stmt list cfg) list) =
  let cfgtbl = Hashtbl.create (List.length fl) in 
  let vine_function_to_name f =
    match f with
	Function(n,_,_,_,_) -> n
      | _ -> failwith "Can't get name of non-function vine stmt"
  in 
  let vardecls = ref DeclSet.empty in
    (* FIXME: don't make function locals global --aij *)
  List.iter 
    (fun (f,cfg) ->
       List.iter 
         (fun d -> vardecls := DeclSet.add d !vardecls)
         cfg#vardecls;
       Hashtbl.add cfgtbl (vine_function_to_name f) cfg
    )
    fl;

  let is_extern f = List.exists (fun (x,g) ->
    match x with
	Function(f',_,_,true,_) when f = f' -> true
      | _ -> false) fl
  in
  (* our supergraph *)
  let scfg = new cfg 57 vine_list_iter_labels (DeclSet.elements !vardecls) in
    (* sblk maps (fun name,blkid) -> supergraph blk id *)
  let sblktbl = Hashtbl.create (List.length fl) in  
    (* Reference to next available blk no. Wanted to use non-folding
       code below for clarity, and this makes it easier for me. -djb *) 
    (* we start from 1 so we don't assume any particular node is the
       entry block *)
  (*let next_no = ref 1 in *)
    (* create a new block in the supergraph for
       each node in each function cfg *)
  let () = List.iter 
    (fun (fstmt,fcfg) ->
    let fname = vine_function_to_name fstmt in 
      fcfg#iter_bb
	(fun blk -> 
	   let b = scfg#newid in
	     scfg#add_bb b blk.stmts;
	     Hashtbl.add sblktbl (fname,blk.id) b )
  ) fl in 
    (* add default blocks *)
  let () = add_bb scfg BB_Entry [] in 
  let () = add_bb scfg BB_Exit [] in 
  let () = add_bb scfg BB_Error [] in 
  let () = add_bb scfg BB_Indirect [] in 
    (* for one cfg, add all its edges to scfg. *)
  let add_edges_one_graph fname fgraph =
    fgraph#iter_bb
      (fun blk ->
	 let blkid = fgraph#get_id blk in
	   match list_last_option (fgraph#get_info blk) with
	       Some(Call(_, Name(v), _)) -> 
		    (* in the cfg, the successor of a
		       call is the next statement. In the supergraph, 
		       it is not. Instead,  we link up the call node
		       with the entry point of the called function.
		       fname is caller, and the function for
		       v the callee.
		    *) 
		 let callee_entry_id = 
		      if is_extern v then (
			(* create copy of external node for context
			   sensitivity *) 
			let id = scfg#newid in 
			let _ = scfg#add_bb id [] in
			let newname = v^"%"^(bbid_to_string id) in 
			  Hashtbl.add sblktbl (newname,cfg_entry) id;
			  id
		      )
		      else
			try Hashtbl.find sblktbl (v,cfg_entry)
			with Not_found -> 
			  raise(Invalid_argument("Call to unknown function "^
						   v^"()"))
			  
		 in
		 let callee_return_id = 
		      if is_extern v then callee_entry_id else
			Hashtbl.find sblktbl (v,cfg_ret)
		 in
		 let caller_id = 
		      Hashtbl.find sblktbl (fname, blkid) 
		 in
		   fgraph#iter_succ
		     (fun f_succ ->
			let scfg_succ_id =
			  Hashtbl.find sblktbl (fname, f_succ.id)
			in 
			  (* link up caller node to callee entry *)
			  add_edge scfg caller_id callee_entry_id;
			  (* link up caller exit to callee successor *)
			  add_edge scfg callee_return_id scfg_succ_id
		     )
		     blk
	     | _ -> (* if our last statement isn't a call
			  node, then it's just an intra-procedural
			  edge.  We link up the node's successors in
			  the supergraph the same as in the cfg.
		       *)
		 let scfg_id = Hashtbl.find sblktbl (fname, blkid) in 
		   fgraph#iter_succ
		     (fun f_succ ->
			let scfg_succ_id =
			  Hashtbl.find sblktbl (fname, f_succ.id)
			in 
			  add_edge scfg scfg_id scfg_succ_id)
		     blk
		(*
                 -2 ->  (* make halt canonical *)
                   let sblkid' = Hashtbl.find sblktbl (fname,-2)
                   in add_edge scfg sblkid' cfg_halt
               | _ -> ()
		*)
      )
  in
    (* Function from supergraph id to name * original id *)
  let revmap =
    let lookup =
      lazy ( (* Is it worth making this lazy, or will it always be used? *)
	(* create inverse mapping *)
	let tbl = Hashtbl.create (Hashtbl.length sblktbl) in
	let () = Hashtbl.iter (fun k v -> Hashtbl.add tbl v k) sblktbl in
	  Hashtbl.find tbl
      )
    in
      fun sid -> Lazy.force lookup sid
  in
  Hashtbl.iter add_edges_one_graph cfgtbl;
  (scfg, sblktbl, revmap)
;;





let iter cfg v = 
  let lst = ref [] in 
  let lst2 = ref [] in 
  let () = 
    Bfs.iter_component (fun x -> 
      let rx = real_bb cfg x in 
	lst2 := rx.id :: !lst2;
      lst := x::!lst) 
      cfg v in 
    (!lst,!lst2)
;;



(******* Printing stuff *********)

let print_succ_lst (cfg: 'a cfg) os blk =
  cfg#iter_succ
    (fun s ->
       Printf.fprintf os "\t%s -> %s;\n"
	 (bbid_to_string blk.id) (bbid_to_string s.id)
    )
    blk

(** write out a cfg of nodes reachable from v as a dot file
    @param v is the node to start calculating reachability
    @param blk_tbl is the cfg
    @param os is the output channel
    @param printer is a function blk->unit used to print out
           node info, e.g., node names, etc.
 *)
let print_dot_reachable_cfg cfg name printer v os =
  Printf.fprintf os "digraph %s {\nnode [shape=box];\n%!" name ;
  Bfs.iter_component
    (fun x ->
       let blk = real_bb cfg x in 
       let () = print_succ_lst cfg os blk in
       let str = printer cfg blk in 
	  Printf.fprintf os "\t%s [label=\"%s\"];\n"
	    (bbid_to_string blk.id) (String.escaped str)
    ) cfg v;
  Printf.fprintf os "}\n%!"
;;

(** write out a cfg as a dot file
    @param blk_tbl is the cfg
    @param os is the output channel
    @param printer is a function blk->unit used to print out
           node info, e.g., node names, etc.
 *)
let print_dot_cfg cfg name printer (os:out_channel) : unit = 
  (* ptbl is the block id's for which we have printed out a label *)
  let print_blk_label blk = (
    let str = printer cfg blk in 
	Printf.fprintf os "\t%s [label=\"%s\"];\n" (bbid_to_string blk.id)
	  (String.escaped str)
  )
  in
    Printf.fprintf os "digraph \"%s\" {\nnode [shape=box];\n" (String.escaped name);
    cfg#iter_bb (fun blk -> print_succ_lst cfg os blk; print_blk_label blk);
    Printf.fprintf os "}\n";
    flush os
;;


(** strings for default blocks. Often one will only call this on a block
    id < 0 (so ENTRY isn't printed for the first block)
    @param blk is a block
*)
let default_blk_printer b = bbid_to_string b.id


(** printer for cg, i.e.
    @param ftbl table from blk id -> name
    @param blk -> a block passed in during graph printing
    @return a string 
*)
let function_name_printer ftbl _ blk = 
  match blk.id with
      BB _ -> 
	(try 
	  Hashtbl.find ftbl blk.id 
	with
	    Not_found -> "unknown "^bbid_to_string blk.id)
    | id -> bbid_to_string id



(** print out the statements in a block. can be passed to print_dot_cfg *)
let stmt_printer _ blk =
  let re = Str.regexp "[\t\n]" in 
    bbid_to_string blk.id^"\n"
    ^ List.fold_left (fun r s -> 
		      let x = stmt_to_string s in 
		      let x' = Str.global_replace re " " x in 
		      let x' = String.escaped x' in 
		      r^"\n"^x') "" blk.stmts

(** print out the statement labels in a block. can be passed to
    print_dot_cfg *)
let label_printer cfg blk = 
  match cfg#get_info blk with
      [] -> bbid_to_string (cfg#get_id blk)
    | stmts -> 
	List.fold_left
	  (fun acc -> function
	       Label x -> acc^"\n"^x
	     | _ -> acc )
	  "" stmts
;;


(** print out the comments in the blk *)
let comment_printer cfg blk = 
  match cfg#get_info blk with
      [] -> bbid_to_string (cfg#get_id blk)
    | stmts -> 
	List.fold_left
	  (fun acc -> function
	       Comment(x) -> acc^"\n"^x
	     | _ -> acc )
	  "" stmts
;;


(**
   @param stbl maps a superblk id to (function name,funid)
   @param ftbl maps a function name to a function cfg
*)
let supergraph_name_printer stbl ftbls _ blk = 
    try
	let (funname,fblkid) = stbl blk.id in
	  (try
	    let fcfg = Hashtbl.find ftbls funname in 
	    let blk = fcfg#find fblkid in 
	    let str = label_printer fcfg blk in 
	      funname^":"^str
	  with Not_found ->
	    Printf.sprintf "%s:%s" funname (bbid_to_string fblkid)
	  )
    with Not_found -> bbid_to_string blk.id



(**
   @param stbl maps a superblk id to (function name,funid)
   @param ftbl maps a function name to a function cfg
*)
let supergraph_small_name_printer  stbl ftbls a blk = 
  let lbllst = 
    List.fold_left  (fun acc s -> match s with
			 Label(l) -> l::acc
		       | _ -> acc )
      [] blk.stmts
  in 
    match lbllst with
	[] -> supergraph_name_printer stbl ftbls a blk
      | _::[] -> List.hd lbllst
      | _ -> 
	  let x = List.hd lbllst in 
	  let y  = List.hd (List.rev lbllst) in 
	    (y^"-"^x)


let supergraph_bbid_printer  stbl ftbls a blk = 
  bbid_to_string blk.id^"="^
    try
      let (funname,fblkid) = stbl blk.id in
	Printf.sprintf "%s:%s" funname (bbid_to_string fblkid)
    with Not_found -> "??"
      


let print_backedges cfg start = 
  let h = Hashtbl.create 65537 in 
  let backedge_detect n1 n2 = 
    if (Hashtbl.mem h n2.id) then (
      Printf.printf "Found backedge: %s %s\n"
	(bbid_to_string n1.id) (bbid_to_string n2.id);
      true
    ) else false
  in
  let rec visit v = 
    Hashtbl.add h v.id ();
    cfg#iter_succ
      (fun succ -> if backedge_detect v succ then () else visit succ)
      v
  in
    visit (cfg#find start)



(** [split_list_cfg oldcfg default_stmt iter_labels] splits an 'a list cfg
    into an 'a cfg.

    
 *)
let split_list_cfg (oldcfg : 'a list cfg) default_stmt iter_labels =
  let map = Hashtbl.create oldcfg#length in
  let cfg = new cfg 65537 iter_labels (oldcfg#vardecls)  in
  let newblk s = cfg#create_bb cfg#newid [s] in
  let translate_block b =
    let oid = oldcfg#get_id b in
    let blocks = match oldcfg#get_info b with
	[] ->
	  let b = newblk default_stmt in
	    (b,b)
      | s::ss ->
	  let start = newblk s in
	  let fin =
	    List.fold_left
	      (fun last s ->
		 let b = newblk s in
		 let () = cfg#add_edge last b in
		   b)
	      start
	      ss
	  in
	    (start,fin)
    in
      Hashtbl.add map oid blocks
  in
  let bstart = fst <@  Hashtbl.find map in
  let bend = snd <@  Hashtbl.find map in
  let add_succ b =
    let src = bend (oldcfg#get_id b) in
      oldcfg#iter_succ (fun d -> cfg#add_edge src (bstart(oldcfg#get_id d))) b
  in    
  let () = oldcfg#iter_bb translate_block in
  let () = oldcfg#iter_bb add_succ in
    (* restore canonical nodes *)
  let tryrename id finder =
    try cfg#rename_bb (finder id) id
    with Not_found -> ()
  in
  let () = tryrename BB_Entry bstart in
  let () = tryrename BB_Exit bend in
  let () = tryrename BB_Indirect bstart in
  let () = tryrename BB_Error bstart in
    cfg
      
let add_indirect_jumps cfg newedges =
  let add_dynedge (src, dst) = try (    
    let s = cfg#find_label (addr_to_label src)
    and d = cfg#find_label (addr_to_label dst) 
    in cfg#add_edge s d) 
  with Not_found -> ()
  in List.iter add_dynedge newedges
       
       
