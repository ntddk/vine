(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Functions for dataflow analysis of VinE programs.
    
    Stuff here is based on the {i Data-Flow Analysis} chapter from
    {Advanced Compiler Design & Implemenation} by Steven Muchnick.
    
    TODO: provide some way to make working with BBs with lists of statements
    as easy as working with BBs with a single statement.
*)

open Vine
open Vine_cfg
open Vine_util
module VH = Vine.VarHash
module VM = Vine.VarMap
module VHU = Vine_util.HashUtil(VH)

module D = Debug.Make(struct let name = "Vine_dataflow" and default=`Debug end)


open Dataflow


(** Builds a transfer function on nodes from a trasfer function
    on statements and a way to get the statements out of
    a node.
    
    Don't forget that get_stmts should  return the statements in the same order
    as you intend to iterate through the CFG.
 *)
let transfer_stmt_to_block f get_stmts node latice =
  List.fold_left (fun l n -> f n l) latice (get_stmts node)

let fwd_transfer_stmt_to_block f g node latice =
  List.fold_left (fun l n -> f n l) latice (bb_stmts g node)

let rev_transfer_stmt_to_block f g node latice =
  List.fold_left (fun l n -> f n l) latice (List.rev(bb_stmts g node))



(** Transfer function for live variables *)
let live_vars_transfer_stmt s l =
  let findvars stmt =
    (* FIXME: this will not deal correctly with having blocks inside 
       the stmt list, but that is ok for now, because we don't allow
       blocks in the CFG. *)
    let vis = object
      inherit nop_vine_visitor
      val mutable refs = []
      val mutable assigned = []
      val mutable cond = false
      method get_refs = refs
      method get_assigned = assigned
      method get_cond = cond
      method visit_exp e = refs <- freerefs_exp e @ refs; SkipChildren
	(* Note: visit_alvalue will not be called for Let expressions
	   because visit_exp returns SkipChildren *)
      method visit_alvalue l = assigned <- l::assigned ; DoChildren
      method visit_stmt s =
	match s with (* this feels like a bit of DCE seeping in *)
	    Move _ -> DoChildren
	  | _ -> cond <- true; DoChildren
    end
    in
    let _ = stmt_accept vis stmt in
    let to_names = List.map (function Temp n|Mem(n,_,_) -> n) in
    let temp_to_names = list_map_some (function Temp n-> Some n |_-> None) in
    let use = to_names (vis#get_refs) in
    let gen = to_names (vis#get_assigned) in
    let kill = temp_to_names (vis#get_assigned) in
      (kill,use,gen, vis#get_cond)
  in
  let (kill,use,gen, cjmp) = findvars s in
    (* FIXME: list_intersection, list_union, etc. use slow comparisons *)
    if not(cjmp) && list_intersection gen l = []
    then l (* assigned variables are not live, so don't mark referenced ones *)
    else list_union use (list_difference l kill)


let live_variables ?(globals=[]) cfg = 
  let transfer_fun = transfer_stmt_to_block
    live_vars_transfer_stmt (fun id -> List.rev (bb_stmts cfg id))
  in
    worklist_iterate_in ~nodes:(cfg_nodes cfg) ~entry:(exit_node cfg)
      ~f_t:transfer_fun
      ~init:globals
      ~pred:(bb_succ cfg) ~succ:(bb_pred cfg) ~top:[] ~meet:list_union
      ~eq:list_set_eq

(** Performs dead code ellimination on a CFG. The CFG is modified in place.
    If the optional argument [globals] is specified, it should contain a list
    of variables that should be considered live "by definition" at the exit
    node. This is useful eg. when you care about the final value of a variable,
    even though the given code doesn't use it again.
 *)
let do_dce ?(globals=[]) cfg =
  let live = live_variables ~globals:globals cfg in
  let has_changed = ref false in 
  let update_bb id =
    let bb = cfg#find id in
    let map_stmt vars s =
      let not_live lval =
	not(List.mem (match lval with Temp n|Mem(n,_,_)->n) vars)
      in
	match (remove_stmt_attrs s) with
	    Move(lv,_) when not_live lv -> has_changed := true;
	      D.dprintf "removing %s: %s\n" (bbid_to_string id) (stmt_to_string s);
	      []
	  | s ->
	      D.dprintf "keeping %s: %s\n" (bbid_to_string id) (stmt_to_string s);
	      [s]
    in
    let map_stmts s =
      fst(List.fold_left
	    (fun (ss,vars) s ->
	       (map_stmt vars s::ss, live_vars_transfer_stmt s vars) )
	    ([],live id)
	    (List.rev (cfg#get_info bb))
	 )
    in
      cfg#set_info bb (List.flatten(map_stmts (cfg#get_info bb)))
  in
    List.iter update_bb (cfg_nodes cfg);
    !has_changed


module G = Vine_cfg.MakeG(struct type t = stmt list end);;


module type CPLANG =
sig
  type stmt
  type const
  type cptype = Unknown | NonConst | Const of const
  val stmt_transfer : cptype VH.t -> stmt -> unit
end

(** Helper to make constant propagation for both Vine.stmt list cfg and
    Ssa.stmt list cfg.
    This is still kind of hackish, since some things are being used from this
    module simply because they were being used from what it is replacing.
 *)
module MakeConstantProp (Lang:CPLANG) =
struct
  module Debug = Debug.Make(struct let name = "ConstantProp" and default=`NoDebug end)
  include Lang

  module CP =
  struct 
    module G = Vine_cfg.MakeG(struct type t = Lang.stmt list end)
    module L =
    struct
      type t = Top | Ht of (cptype VH.t) 
      let top = Top

      let cpmeet v1 v2 = 
	match v1,v2 with
            Unknown, Unknown ->  Unknown
	  | Unknown,Const c -> v2
	  | Const(c), Unknown -> v1
	  | Const(c1), Const(c2)  when c1 = c2 -> v1
	  | _ -> NonConst
	    
      let lookup ht k = 
	try 
	  VH.find ht k 
	with
            Not_found -> Unknown
	      
      let t_to_ht t = 
	match t with
            Top -> VH.create 7
	  | Ht(h1) -> h1
	      
      let meet v1 v2 = 
	let (h1:cptype VH.t) = t_to_ht v1 in
	let (h2:cptype VH.t) = t_to_ht v2 in
	let h2 = VH.copy h2 in
	  VH.iter 
            (fun k v1 ->
               let v2 = lookup h2 k in
               let nv = cpmeet v1 v2 in 
		 VH.replace h2 k nv
            ) h1; Ht(h2)


      let equal x y =
	match (x,y) with
	    (Top,Top) -> true
	  | (Ht ht1, Ht ht2) -> VHU.hashtbl_eq ht1 ht2
	  | _ -> false

    end
    open L

    let transfer_function (graph:G.t) node t  = 
      let stmts = graph#get_info (graph#find node) in 
      let ht = VH.copy (t_to_ht t) in 
      let f = Lang.stmt_transfer ht in 
	List.iter (f) stmts;
	Ht(ht)
	  
      
    let s0 = Vine_cfg.BB_Entry
      
    let init = Top
      
    let dir = Dataflow.Forward
      
  end
  include CP

  module This = Dataflow.Make(CP)
  include This
end




module VineSpecificConstantPropStuff =
struct
  type stmt = Vine.stmt
  type const = Vine.exp
  type cptype = Unknown | NonConst | Const of const
    	  
  let stmt_transfer ht stmt = 
      match stmt with
	| Move(Temp(x), v)  -> (
	    match v with
		Constant _ ->
		  VH.replace ht x (Const(v))
	      | Lval(Temp variable) ->
		  (try VH.replace ht x (VH.find ht variable)
		   with Not_found -> () )
	      | _ -> 
		  (* Should this be Const when the operands are constant? *)
		  VH.replace ht x NonConst
	  )
	| Call(Some(Temp x),_,_) ->
	    VH.replace ht x NonConst
	| _ -> ()
end
  
module ConstantProp = MakeConstantProp(VineSpecificConstantPropStuff)


let constant_propagation ?(init=ConstantProp.L.top) graph = 
  let (in_f,out_f)  = ConstantProp.worklist_iterate ~init:init graph in
    (in_f,out_f)

(** [constant_propagation_and_folding graph simplifer] perform
    substitution for constants on a stmt list cfg [graph].  This
    function propagates that information through the node, replacing
    each variable used with the constant.  [simplifier] is a
    simplifying function from stmt -> stmt, e.g.,
    Vine_opt.simplify_stmt, or id if you want to do nothing. 

    We return true iff constant propagation replaces something with a
    constant. We do not check whether or not the smiplifier you passed
    in simplified the statement.

    Note: this assumes variables are created properly, e.g., shadowed
    variables are unique even though the textual name may be the same.
*)

let constant_propagation_and_folding ?(init=ConstantProp.L.top) graph simplify  = 
  let (in_f, _) = constant_propagation ~init:init graph in 
  let has_changed = ref false in 
  let vis = object(self)
    inherit nop_vine_visitor
      
    val mutable ctx = VH.create 13 
 
    method set_ctx c =
      ctx <- c

    method get_ctx = ctx

    (* visit_exp does the actual substitution. It looks in the current
       context, and if it finds a single mapping for an temp to a
       constant, replaces that temp with the constant. *)
    method visit_exp e =
      match e with
	  Lval(Temp(t)) -> (
	    let dataflow_value = ConstantProp.L.lookup ctx t in
	      match dataflow_value with
		  ConstantProp.Const(x) ->  has_changed := true;
		    ConstantProp.Debug.dprintf "replace %s with %s"
		      (Vine.var_to_string t)
		      (Vine.exp_to_string x);
		    ChangeTo(x)
		| _ -> DoChildren
	  )
	| _ -> DoChildren

  end
  in
    graph#iter_bb 
      (fun bb -> 
         let () = ConstantProp.Debug.dprintf "BLOCK %s"
           (Vine_cfg.bbid_to_string (graph#get_id bb)) in
         let stmts = graph#get_info bb in 
         let intbl = in_f (graph#get_id bb) in 
         let ht = ConstantProp.L.t_to_ht intbl in
         let stmts' = List.map
           (fun s -> 
              vis#set_ctx ht;
              let s' = stmt_accept vis s in
	      let s' = simplify s' in 
                ConstantProp.stmt_transfer (vis#get_ctx) s;
                s'
           ) stmts 
         in
           graph#set_info bb stmts'
      );
      !has_changed


(** Dataflow for SSA *)
module SsaDataflow = 
struct

  open Ssa
  open Dataflow
  open ExtList

  module G = Vine_cfg.MakeG(struct type t = Ssa.stmt list end);;

  module AvailableExpressions =
  struct
    module AE_DF_Prob =
    struct
      module G = G
      module L =
      struct
	type t = Top | Map of Ssa.exp VM.t
	let top = Top
	let meet x y = match (x,y) with
	  | (Top, v) | (v, Top) -> v
	  | (Map x, Map y) -> (* intersection of both *)
	      Map(VM.fold
		    (fun k _v res ->
		       if VM.mem k y then res
		       else VM.remove k res
		    )
		    x x
		 )
	      
	let equal x y = match (x,y) with
	  | (Map m1, Map m2) -> VM.equal (=) m1 m2
	  | (Top, Top) -> true
	  | (Top, Map _) | (Map _, Top) -> false

	let add k v = function
	  | Top -> Top
	  | Map x -> Map(VM.add k v x)
      end
	

      let transfer_stmt s l = match s with
	| Ssa.Move(v, e) -> L.add v e l
	  (* maybe special case x=y type stmts later *)
	| _ -> l

      let transfer_function =
	fwd_transfer_stmt_to_block transfer_stmt

      let s0 = BB_Entry
      let init = L.Map VM.empty
      let dir = Forward
    end
    module AE_DF = Dataflow.Make(AE_DF_Prob)


  end (* AvailableExpressions *)


  (** this is a limited form of available expressions. It creates a
      list of available binops of the form (A op C) where C is a
      constant and A is a scalar *)
  module ScalarAvailable = 
  struct

    module G  = G;;

    module SASetT =
      struct
	type t = (Ssa.var * Ssa.exp)
	let compare (t1,_) (t2,_) = Vine.Var.compare t1 t2
      end

    module SASet = Set.Make(SASetT)

    module L =
      struct
	type t = SASet.t
	
	let top = SASet.empty
	  
	(* FIXME: Intersection isn't quite right if top = empty *)
	let meet lst1 lst2 =
	  SASet.inter lst1 lst2 
	    (* Vine_util.list_intersection lst1 lst2 *)
	    
	let equal lst1 lst2 = 
	  (SASet.compare lst1 lst2) = 0
	
      end

    module Debug = Debug.Make(struct let name = "ScalarAvailable" and default=`NoDebug end);;
    
    let print_lst lst = 
      List.iter (fun (lhs,e) ->
		   Debug.dprintf "%s = %s" (Vine.var_to_string lhs)
		     (Ssa.exp_to_string e)
		) (SASet.elements lst)
	  
    let likely_to_simplify op = 
      match op with
	  DIVIDE | SDIVIDE | MOD | SMOD | LSHIFT
	| RSHIFT | ARSHIFT -> false
	| _ -> true
	
    let rec stmt_transfer lst stmt = 
      match stmt with
	  Move(lhs, (BinOp(op, Lval((_,_,rhstype)),Int(_)) as e)) 
	| Move(lhs, (BinOp(op, Int(_), Lval((_,_,rhstype))) as e))
	    -> if is_integer_type rhstype &&
	      likely_to_simplify op
	    then
	      SASet.add (lhs,e)  lst 
		(* Vine_util.list_union lst [(lhs,e)] *)
	    else
	      lst
	| Attr(s,_) -> stmt_transfer lst s
	| _ -> lst
	      
    let transfer_function (graph:G.t) node lst = 
      (* in ssa no definition is ever killed, so we need not do
	 anything. *)
      (*Debug.dprintf "%s" (Vine_cfg.bbid_to_string node);*)
      lst
  
    let s0 = Vine_cfg.BB_Entry
      
    let init = SASet.empty
      
    let dir = Dataflow.Forward
      
      
  end (* end module AffineSubst *)
    
  module ScalarAvailableDF = Dataflow.Make(ScalarAvailable)

  let scalar_available_dataflow ?(init=ScalarAvailable.L.top) graph = 
    let init = 
      ScalarAvailable.G.fold_vertex
	(fun bb acc ->
	   let stmts = graph#get_info (graph#find bb) in 
	     List.fold_left (ScalarAvailable.stmt_transfer) acc stmts 
	) graph init
    in
    let (in_f, out_f) = 
      ScalarAvailableDF.worklist_iterate ~init:init graph  in
      (in_f, out_f)
  ;; 

  (** simplifies based upon scalar available expressions.  *)
  let scalar_available_simplify ?(init=ScalarAvailable.L.top) graph = 
    let (in_f, _) = scalar_available_dataflow ~init:init graph in 
    let changed = ref false in 
    let translate_from_vine (lhs:Ssa.lvalue) (e:Vine.exp) s = 
      match e with
	  Vine.BinOp(op, 
		     Vine.Constant(Vine.Int(it,iv)), 
		     Vine.Lval(Vine.Temp(t))) ->
	    ScalarAvailable.Debug.dprintf "Simplified %s" (stmt_to_string s);
	    changed := true;
	    Move(lhs,Ssa.BinOp(op,Ssa.Int(iv,it),Ssa.Lval(t)))
	| Vine.BinOp(op, 
		     Vine.Lval(Vine.Temp(t)), 
		     Vine.Constant(Vine.Int(it,iv))) ->
	    ScalarAvailable.Debug.dprintf "Simplified %s" (stmt_to_string s);
	    changed := true;
	    Move(lhs,Ssa.BinOp(op,Ssa.Lval(t),Ssa.Int(iv,it)))
	| _ -> (* 
		  simplification did not produce smaller result, e.g.,
		  e = (a + c) << c2
		  cannot be simplified further.
	       *)
		  s 
    in
    let rec f lst s = 
      match s with
	  Move(lhs, BinOp(op, Ssa.Int(iv,it), Lval(rhs))) when
	    ScalarAvailable.likely_to_simplify op ->
	    (
	      try 
		let (_,e') = List.find 
		  (fun (lhs,_) -> lhs = rhs) lst in
		let e' = Ssa.exp2vine e' in 
		let e = Vine.BinOp(op, Vine.Constant(Vine.Int(it,iv)), e') in 
		let e = Vine_opt.simplify  e in 
		let s' = translate_from_vine lhs e s in 
		  if s <> s' then (
		    ScalarAvailable.Debug.dprintf "%s -> %s" 
		      (Ssa.stmt_to_string s)
		      (Ssa.stmt_to_string s');
		    s')
		  else s
	      with
		  Not_found -> s
	    )
	| Move(lhs, BinOp(op, Lval(rhs), Ssa.Int(iv,it))) when
	    ScalarAvailable.likely_to_simplify op ->
	    (
	      try 
		let (_,e') = List.find 
		  (fun (lhs,_) -> lhs = rhs) lst in
		let e' = Ssa.exp2vine e' in 
		let e = Vine.BinOp(op, e', Vine.Constant(Vine.Int(it,iv))) in 
		let e = Vine_opt.simplify  e in 
		let s' = translate_from_vine lhs e s in 
		  if s <> s' then (
		    ScalarAvailable.Debug.dprintf "%s -> %s" 
		      (Ssa.stmt_to_string s)
		      (Ssa.stmt_to_string s');
		    s')
		  else s
	      with
		  Not_found -> s
	    )
	| Attr(s,a) -> Attr(f lst s, a)
	| _ -> s
    in
      graph#iter_bb 
	(fun bb ->
	   let () = ScalarAvailable.Debug.dprintf "Block %s"
	     (Vine_cfg.bbid_to_string (graph#get_id bb)) in 
	   let stmts = graph#get_info bb in 
	   let lst = in_f (graph#get_id bb) in 
	   let lst = ScalarAvailable.SASet.elements lst in 
	   let stmts = List.map (f lst) stmts in
	     graph#set_info bb stmts
	);
      !changed

  module SsaSpecificConstantPropStuff =
  struct
    type stmt = Ssa.stmt
    type const = Ssa.value
    type cptype = Unknown | NonConst | Const of const

    let rec eval_exp ht x e = 
      match e with
	  BinOp(Vine.BITAND, _, (Int(0L,_) as i))
	| BinOp(Vine.BITAND, (Int(0L,_) as i), _) ->
	    VH.replace ht x (Const(i))
	| Val(Int _ as v)
	| Val(Name _ as v)
	| Val(Str _ as v) ->
	    VH.replace ht x (Const(v))
	| Val(Lval(variable)) -> (
	    try VH.replace ht x (VH.find ht variable)
	    with Not_found -> ()
	  ) 
	| _ -> ()

    let rec stmt_transfer ht stmt = 
      match stmt with
	  Jmp _ 
	| CJmp _ 
	| Label _
	| Comment _
	| Return _
	| Call _ -> ()
	| Move(x, e) -> eval_exp ht x e
	(* | Move(x, Val(v)) -> (
	    match v with
		Int _ | Name _ | Str _ ->
		  VH.replace ht x (Const(v))
	      | Lval(variable) ->
		  try VH.replace ht x (VH.find ht variable)
		  with Not_found -> () 
	  )
	| Move(x, BinOp(Vine.BITAND, _, (Int(0L,_) as i))) 
	| Move(x, BinOp(Vine.BITAND, (Int(0L,_) as i),_)) ->
	    VH.replace ht x (Const(i))
	| Move(x, BinOp(Vine.XOR, v1, v2)) when v1 = v2 ->
	    VH.replace ht x (Const(Int(0L, typeof_var x))) 
	| Move _ -> () *)
	| Assert _ -> ()
	| Halt _ -> ()
	| Attr(s,_) -> stmt_transfer ht s      
  end
    
  module ConstantProp = MakeConstantProp(SsaSpecificConstantPropStuff)
  
  let constant_propagation ?(init=ConstantProp.L.top) graph = 
    let (in_f,out_f)  = ConstantProp.worklist_iterate ~init:init graph in
      (in_f,out_f)
	
	
  let constant_propagation_and_folding ?(init=ConstantProp.L.top) graph
    simplify =
    let has_changed = ref false in 
    let (in_f, _) = constant_propagation ~init:init graph in 
    let vis = object(self)
      inherit nop_ssa_visitor
	
      val mutable ctx = VH.create 7
	
      method set_ctx c = ctx <- c
	
      method get_ctx  = ctx
	
      method visit_value v =
	match v with
	    Lval(v') when Vine.is_not_memory v' ->  (
	      (* ConstantProp.Debug.dprintf "Visiting %s"(Vine.var_to_string v'); *)
	      match ConstantProp.L.lookup ctx v' with
		| ConstantProp.Const(c) -> 
		    ConstantProp.Debug.dprintf "Replacing %s with %s"
		      (Vine.var_to_string v')
		      (Ssa.value_to_string c);
		    has_changed := true;
		    Vine.ChangeTo(c)
		| ConstantProp.Unknown ->
		    (* ConstantProp.Debug.dprintf "Not replacing unknown %s"
		      (Vine.var_to_string v'); *)
		    Vine.DoChildren
		| ConstantProp.NonConst ->
		    (* ConstantProp.Debug.dprintf "Not replacing nonconst %s"
		      (Vine.var_to_string v'); *)
		    Vine.DoChildren
	    )
	  | _ ->
	      (*ConstantProp.Debug.dprintf "Ignoring value %s"(Ssa.value_to_string v);*)
	      Vine.SkipChildren
		    
    end
    in
      graph#iter_bb 
	(fun bb -> 
	   let () = ConstantProp.Debug.dprintf "BLOCK %s"
	     (Vine_cfg.bbid_to_string (graph#get_id bb)) in
	   let stmts = graph#get_info bb in 
	   let intbl = in_f (graph#get_id bb) in 
	   let ht = ConstantProp.L.t_to_ht intbl in
	     (* This mutates the old hashtable, but that is ok, since we never
		use it again. *)
	   let stmts' = List.map
	     (fun s -> 
		vis#set_ctx ht;
		let s' = stmt_accept vis s in 
		let s' = simplify s' in 
		  ConstantProp.stmt_transfer (vis#get_ctx) s;
		  s'
	     ) stmts 
	   in
	     graph#set_info bb stmts'
	);
      !has_changed

  (* XXX assuming only one var can be defined by a stmt.
     Previous implementation seems to imply otherwise? *)
  module DeadCode =
  struct
    type site = (Ssa.stmt list Vine_cfg.bb) * Ssa.stmt
	
    module Debug = Debug.Make(struct let name = "DeadCode" and default=`NoDebug end)
      
    (* return list of lvals defined and used by this stmt *)
    (* XXX doesn't match behavior of previous implementation
       w.r.t. Set. Not sure which behavior is correct. *)
    (* not performing union on used vars; shouldn't be needed
       for correctness, since double-counting will be symmetrical
       when incrementing and decrementing use-counts *)
    let def_uses s =
      let lv = 
        match s with
        | Move (lv, _) ->
            [lv]
        | _ -> []
      in
      let uses = ref [] in
      let vis =  object(self)
 	inherit nop_ssa_visitor
 	val mutable ctx = []
 	method get_ctx = ctx
	method visit_rvar v = uses := v :: !uses;
 	  Vine.DoChildren
      end
      in
      ignore (Ssa.stmt_accept vis s);
      (match s with
       | Move(_, Set(Lval(lv2),_,_,_)) ->
	   (* FIXME: Unneeded? This should have already been added by the
	      visitor. --aij *)
           uses := lv2 :: !uses
       | Move(_, Set(_,_,_,_)) -> failwith "malformed set"
       | _ -> ()
      );
      lv, !uses
	
    (** in SSA, a variable is live at its definition site iff its list of
	uses is not empty. Therefore, calculating live variables is really
	just a matter of calculating whether or not a variable has any
	uses. (p445  ML Tiger book ) *)
    let do_dce ?(globals=[]) graph =
      let (var_to_deps:(Ssa.var, Ssa.var list) Hashtbl.t) = Hashtbl.create 57 in
      let (var_to_defsite:(Ssa.var, site) Hashtbl.t) = Hashtbl.create 57 in
      let (var_to_usecounts:(Ssa.var, int) Hashtbl.t) = Hashtbl.create 57 in

      (* get initial mappings *)
      graph#iter_bb
	(fun bb ->
	   let stmts = graph#get_info bb in
	   List.iter
	     (fun s ->
		let site = (bb, s) in
                let defs, deps = def_uses s in
                
                (* iterate over defs, updating maps *)
                List.iter
                  (fun defd_var ->
                     assert(not (Hashtbl.mem var_to_deps defd_var));
                     Hashtbl.add var_to_deps defd_var deps;
                     assert(not (Hashtbl.mem var_to_defsite
                                   defd_var));
                     Hashtbl.add var_to_defsite defd_var site;
                
                     if not (Hashtbl.mem var_to_usecounts defd_var)
                     then
                       Hashtbl.add var_to_usecounts defd_var 0
                  )
                  defs;
                
                (* update var_to_usecounts mapping *)
                List.iter
                  (fun used_var ->
                     let old_count = 
                       try Hashtbl.find var_to_usecounts used_var
                       with Not_found -> 0
                     in
                     Hashtbl.replace 
                       var_to_usecounts 
                       used_var
                       (old_count + 1))
                  deps;
	     )
             stmts
	);

      (* increment use-counts for globals *)
      List.iter
        (fun global ->
           try
             let count = Hashtbl.find var_to_usecounts global in
             Hashtbl.replace var_to_usecounts global (count+1)
           with Not_found ->
             Debug.wprintf 
               "Global var %s not found"
               (Vine.var_to_string global)
        )
        globals;

      (* initialize kill list with unused defs *)
      let vars_to_kill = ref [] in
      Hashtbl.iter
        (fun v count ->
           if count = 0 then
             vars_to_kill := v :: !vars_to_kill)
        var_to_usecounts;

      (* we'll end up eliminating dead code iff vars_to_kill is non-empty *)
      let has_changed = !vars_to_kill <> [] in
      
      (* iteratively kill stmts defining dead vars, adding newly dead vars
         to var_to_kill, until vars_to_kill is empty *)
      let (dead_sites:(site, unit) Hashtbl.t) = Hashtbl.create 57 in
      let blocks_to_update = Hashtbl.create 5 in
      while (!vars_to_kill <> []) do 
        let var_to_kill = list_pop vars_to_kill in

        if Hashtbl.mem var_to_defsite var_to_kill then (
          (* add defining stmt to kill list *)
          let site_to_kill = Hashtbl.find var_to_defsite var_to_kill in
          let bb, stmt = site_to_kill in
          Debug.dprintf "Removing from %s dead stmt: %s"
	    (Vine_cfg.bbid_to_string (graph#get_id bb))
	    (Ssa.stmt_to_string stmt);
          assert(not (Hashtbl.mem dead_sites site_to_kill));
          Hashtbl.add dead_sites site_to_kill ();
	  Hashtbl.replace blocks_to_update bb ();

          (* decrement uses of used vars, 
             adding to kill list if uses is now 0 *)
          List.iter
            (fun used_var ->
               let use_count = Hashtbl.find var_to_usecounts used_var in
               assert(use_count > 0);
               let use_count = use_count - 1 in
               Hashtbl.replace var_to_usecounts used_var use_count;
               if use_count = 0 then
                 vars_to_kill := used_var :: !vars_to_kill;
            )
            (Hashtbl.find var_to_deps var_to_kill)
        ) else (
          Debug.dprintf 
            "Dead var %s is undefined"
            (Ssa.value_to_string (Ssa.Lval(var_to_kill)))
        );
      done;
      
      (* go over graph to remove dead sites *)
      Debug.dprintf "Deleting %d dead stmts" (Hashtbl.length dead_sites);
      Hashtbl.iter
	(fun bb () ->
           let stmts = graph#get_info bb in
           let stmts' =
             List.filter
               (fun s ->
                  not (Hashtbl.mem dead_sites (bb,s)))
               stmts
           in
           graph#set_info bb stmts'
	)
	blocks_to_update;

      has_changed
  end


    (** in SSA, a variable is live at its definition site iff its list of
	uses is not empty. Therefore, calculating live variables is really
	just a matter of calculating whether or not a variable has any
	uses. (p445  ML Tiger book ) *)      
  let do_dce = DeadCode.do_dce



  module Dom = Dominator.Make(Ssa.G)
    
  (** [node_sdom cfg a b] returns true if position [a] strictly dominates
      position [b]. (where a position is a bbid * distance into the BB) *)
  let pos_sdom cfg =
    let {Dom.sdom=sdom;} = Dom.compute_all cfg BB_Entry in
      (fun (a_bb, a_i) (b_bb, b_i) ->
	 sdom a_bb b_bb || (a_bb = b_bb && a_i < b_i)
      )

  let defsite cfg =
    let defsites = VH.create 5700 in
    let () =
      cfg#iter_bb
	(fun b ->
	   let bbid = cfg#get_id b in
	   let rec addone i s = match s with
	     | Move(l,_)
	     | Call(Some l, _, _) ->
		 VH.add defsites l (bbid,i)
	     | Attr(s,_) ->
		 addone i s
	     | _ -> ()
	   in
	     List.iteri addone (cfg#get_info b)
	)
    in
      (fun x -> 
	 try VH.find defsites x
	 with Not_found -> (BB_Entry,-1) (* globals come from before BB_Entry *)
      )

    
  module GVN =
  struct
    module D = Debug.Make(struct let name = "GVN" and default=`NoDebug end)
    open D
    
    module SSA = Ssa
    module V = Vine
      
    type content = Val of SSA.value
		   | BinOp of SSA.binop_type
		   | UnOp of SSA.unop_type
		   | Cast of SSA.cast_type * SSA.typ
		   | Unknown of string
		   | Get of typ
		   | Set of typ 
		   | Phi
		   | Fun of SSA.lvalue * SSA.label
		       
			    
    (** A value graph node type as discussed in Muchnick p.349. *)
    type vgnode' = {
      mutable label : content;
      mutable ops : vgnode list;
      mutable lvs : Ssa.var list; (* the variables pointed to this node.
					  The first one is the direct pointed lv *)
      pos : bbid * int (* position in the CFG *)
    }
	(** A value graph node type as discussed in Muchnick p.349. *)
    and vgnode = vgnode' ref
	
    (**
       A datatype containing value graph and global value numbering information 
    *)
    type vginfo = {
      lval2node :  vgnode Vine.VarHash.t; 
      (* those referenced by content_lval *)
      mutable nodelist : vgnode list; (* all except empty node *)
      mutable partitions : vgnode list ref list;
    }

    let new_vginfo n = {
      lval2node = VH.create n;
      nodelist = [];
      partitions = [];
    }

(*
    (**
       [get_v2n_tbl vginfo] returns a hashtable that maps an SSA variable
       to its correspondent node in a value graph represented in [vginfo].
    *)
    let get_v2n_tbl vginfo = 
      let old_tbl = vginfo.lval2node in
      let new_tbl = Hashtbl.create (Hashtbl.length old_tbl) in
      let () = Hashtbl.iter (fun x y -> match x with
				(* adding is ok here because each
				   variable is assigned only once *)
				Temp v -> Hashtbl.add new_tbl v y
			      | _ -> () ) old_tbl in
	new_tbl
	
  
    let pp_content_lval pr lv =
      match lv with
	  Temp t -> SSA.pp_lval pr t
	| Mem(v1,v2,t) -> 
	    SSA.pp_value pr v1; pr "["; SSA.pp_value pr v2; pr "]:";
	    SSA.pp_type pr t
*)
	    
    (**
       Pretty-print for value graph node.
       For example, x=1+2; y=3+4; z=x+y; will print z as x+y;
       Casted expression is printed like the expression inside it
    *)
    let rec pp_vgnode pr vgnode =
      match (!vgnode.label, !vgnode.ops) with
	| (Val v, []) -> SSA.pp_value pr v
	| (BinOp op, [n1;n2]) ->
	    pp_vgnode pr n1;
	    pr (V.binop_to_string op);
	    pp_vgnode pr n2
	| (UnOp op, [n]) ->
	    pr (V.unop_to_string op);
	    pp_vgnode pr n
	| (Cast(ct,t), [n]) ->
	    pp_vgnode pr n
	| (Unknown s, []) ->
	    pr "Unknown \""; pr s; pr "\""
	| (Get t, [n1;n2]) ->
	    pp_vgnode pr n1;
	    pr "[";
	    pp_vgnode pr n2;
	    pr "]:";
	    pp_typ pr t
	| (Set(t), [v1;v2;v3]) -> 
	    pp_vgnode pr v1;
	    pr " with [";
	    pp_vgnode pr v2;
	    pr "]:";
	    pp_typ pr t;
	    pr " = ";
	    pp_vgnode pr v3
	| (Phi, nl) ->
	    pr "Phi([";
	    List.iter (fun n -> pp_vgnode pr n; pr ", ") nl;
	    pr "])"
	| (Fun (lv,l), nl) ->
	    pr l;
	    pr "(";
	    List.iter (fun n -> pp_vgnode pr n; pr ", ") nl;
	    pr ")"
	| _ ->
	    pr "Malformed node"
		
    let pp_lvalmap ps vginfo =
      ps "\n Lval_mapping { \n";
      VH.iter
	(fun a b ->
	   ps "   ";
	   pp_var ps a;
	   ps " = ";
	   pp_vgnode ps b;
	   ps " ; \n")
	vginfo.lval2node;
      ps " } \n"
	
    let pp_part ps vginfo =
      ps "\n Partitions { \n";
      List.iteri (fun i a ->
		    ps ("   " ^ (string_of_int i) ^ " = { \n");
		    List.iter (fun n ->
				 ps "     ";
				 pp_vgnode ps n;
				 ps " ; \n";) !a) vginfo.partitions;
      ps " } \n"    
	
    (**
       [pp_vginfo] uses a printer function [pr] to do pretty-print for 
       the value graph information [vginfo]. The printed information includes
       the value number table that shows value numbers and expressions that 
       match.
    *)
    let pp_vginfo ps vginfo =
      pp_lvalmap ps vginfo;
      ps "\n";
      pp_part ps vginfo
	
    let findi f =
      let rec findi' f index = function
	  [] -> -1
	| x::l -> if (f x) then index else findi' f (index+1) l
      in
	findi' f 0
	  
    (**
       [get_gvn_of_value vginfo value] returns the global value number 
       of [value] as specified by [vginfo] or -1 if it doesnot belong to
       [vginfo].
    *)
    let get_gvn_of_value vginfo =
      fun v ->
	findi (fun cur_part -> 
	  List.exists (fun node -> !node.label = Val v) !cur_part)
	  vginfo.partitions

    (**
       Whether a b is surely equivalent
    *)
    let eq_vgnode a b =
      !a == !b || (!a.label = !b.label && 
	  List.length (!a.ops) + List.length (!b.ops) = 0)
	
    (**
       Not support yet
    *)
    let get_gvn_of_exp _ = failwith "Unimplemented"
      
    let get_nodelist vginfo =
      vginfo.nodelist
	
    let update_vginfo ginfo = 
      let addnref node =
	ginfo.nodelist <- node :: ginfo.nodelist;
	node
      in
      let get_content_lval_node = VH.find ginfo.lval2node
      in
      let get_lval_node p lv =
	(try get_content_lval_node lv
	 with Not_found -> (* empty node *)
	   let new_gnode = 
	     (* temporary node, only lvs field is valid *)
	     ref { label = Val(SSA.Lval lv); pos = p;
		   ops = [] ; lvs = [lv]}
	   in
	     VH.add ginfo.lval2node lv new_gnode;
	     new_gnode
	)
      in
      let node_of_value p v =
	match v with
	    SSA.Lval lv -> get_lval_node p lv
	  | _ -> 
	      let new_gnode = ref { label = Val v; ops = []; lvs = []; pos = p}  in
		addnref(new_gnode)
      in
      let rec node_of_stmt p =
	function
	    SSA.Jmp v -> ignore(node_of_value p v)
	  | SSA.CJmp (v1, v2, v3) ->
	      ignore(node_of_value p v1);
	      ignore(node_of_value p v2);
	      ignore(node_of_value p v3)
	  | SSA.Move(lv, e) -> (* in SSA form, this happens at most once per lv *)
	      let lv_node = get_lval_node p lv in
	      let e_node = node_of_exp p e in
	      let lvs = (!lv_node).lvs in
	      let e_lvs = (!e_node).lvs in
		(!e_node).lvs <- e_lvs @ lvs;
		List.iter (fun nlv -> 
			     let n = get_content_lval_node nlv in 
			       n := !e_node) lvs
	  | SSA.Return(Some v) -> ignore(node_of_value p v)
	  | SSA.Call(None, _, vl) -> 
	      List.iter (fun v -> ignore(node_of_value p v)) vl
	  | SSA.Call(Some lv, Name(l), vl) ->
	      let lv_node = get_lval_node p lv in
	      let e_node = ref {label = Fun (lv,l); pos = p;
				ops = List.map (node_of_value p) vl; lvs = [] }
	      in
	      let lvs = (!lv_node).lvs in
	      let e_lvs = (!e_node).lvs in
		(!e_node).lvs <- e_lvs @ lvs;
		List.iter (fun nlv -> 
			     let n = get_content_lval_node nlv in 
			       n := !e_node) lvs
	  | SSA.Comment _ | SSA.Label _ ->
	      ()
	  | s ->
	      wprintf "node_of_stmt: unhandled stmt: %s" (SSA.stmt_to_string s)
      and node_of_exp p e =
	match e with
	    SSA.BinOp (btyp, v1, v2) ->
	      addnref(ref {label = BinOp(btyp); pos = p; lvs = [];
			   ops = [node_of_value p v1; node_of_value p v2];
			  })
	  | SSA.UnOp (utyp, v) ->
	      addnref(ref {label = UnOp(utyp); pos = p;
			   ops = [node_of_value p v]; lvs = []; 
			  })
	  | SSA.Val v -> node_of_value p v
	  | SSA.Cast(ct,t,v) ->
	      addnref(ref 
			{label = Cast(ct,t); pos = p;
			 ops = [node_of_value p v]; lvs = [];
			})
	  | SSA.Unknown s ->
	      addnref(ref{label = Unknown s; pos = p;
			  ops = [];lvs = [];})
	  | SSA.Get(v1,v2,t) ->
	      addnref(ref{label = Get(t); pos = p;
			  ops = [node_of_value p v1; node_of_value p v2];
			  lvs = []; 
			 })
	  | SSA.Set(v1,v2,v3,t) ->
	      addnref(ref{label = Set(t); pos = p;
			  ops = [node_of_value p v1; node_of_value p v2;
				 node_of_value p v3;];
			  lvs = []; 
			 })
	  | SSA.Phi vl ->
	      addnref(ref{label = Phi; pos = p;
			  ops = List.map (get_lval_node p) vl; lvs = [];
			 })
(*
      and get_mem_node p v1 v2 t = (* only Get(v1,v2) *)
	let wrap_mem = Mem (v1,v2,t ) in
	let vnode1 = node_of_value p v1 in
	let vnode2 = node_of_value p v2 in
	let rec mem_node vnode = 
	  match !vnode with
	    | {label=Set(t); ops=[mnode;vnode3;vnode4]}
		when eq_vgnode vnode2 vnode3 ->
		vnode4
	    | _ -> 
		ref {label = Get; ops = [vnode1; vnode2];
		     lvs = [wrap_mem]; pos = p;}
	in
	  (try Hashtbl.find ginfo.lval2node wrap_mem
	   with Not_found ->
	     let new_gnode = mem_node (node_of_value p v1) in
	       Hashtbl.add ginfo.lval2node wrap_mem new_gnode;
	       new_gnode
	  )
*)
      in
	fun bbid -> List.iteri (fun i s -> node_of_stmt (bbid,i) s)
	  
    let similar_vgnode a b =
      !a.label = !b.label && List.length (!a.ops) = List.length (!b.ops)
      
    (* FIXME: check that a+b = b+a too *)
    let eq_op_vgnode i a =
      let aith = List.nth (!a.ops) i in
      let alab = !aith.label in
	match alab with
	    Val x -> (fun b ->
			match !(List.nth (!b.ops) i).label with
			    Val y -> x = y
			  | _ -> false)
	  | _ -> (fun b -> !aith == !(List.nth (!b.ops) i))
	      
    let gvn_init vginfo =
      let worklist = ref [] in
      let () =
	List.iter
	  (fun node ->
	     try
	       let b_i = 
		 List.find (fun l -> similar_vgnode node (List.hd !l)) 
		   vginfo.partitions
	       in
		 b_i := node :: !b_i;
		 if !node.ops <> [] && not (List.exists ((==) b_i) !worklist)
		 then worklist :=  b_i :: !worklist
	     with Not_found ->
	       vginfo.partitions <- ref ([node]) :: vginfo.partitions
	  )
	  vginfo.nodelist
      in
	worklist
	  
    (* not very efficient now *)
    (*
      Assumption: all value graph nodes only have one level of successors.
  This is true because SSA.exp is not a recursive type and all its child
      is equivalent to a value graph node.
    *)
    let gvn_part vginfo =
      let pop q =
	match !q with 
	    [] -> failwith "Dequeuing an empty queue."
	  | h::t -> q := t; h
      and push q v =
	q := v :: !q
      in
      let worklist = gvn_init vginfo in
	dprintf "gvn_part: gvn_init is done.";
	dprintf "gvn_part: worklist size=%d" (List.length !worklist);
	while !worklist <> [] do
	  let b_i = pop worklist in
	  let m = List.hd !b_i in
	  let arity = List.length (!m.ops) in
	    for i = 0 to arity-1 do
	      let (b1,b2) = List.partition (eq_op_vgnode i m) !b_i in
		b_i := b1;
		if (b2 <> [])
		then (
		  let b_p = ref(b2) in
		    vginfo.partitions <- b_p :: vginfo.partitions;
		    push worklist b_p
		)
	    done
	done
	  


    (** modifies nodes in partitions, replacing some expressions with
	references to previously defined variables 
    *)
    let frob_partitions cfg vginfo =
      let node_sdom =
	let psdom = pos_sdom cfg in
	  fun na nb -> psdom (!na.pos) (!nb.pos)
      in
      let rec find_best n cur rest =
	match rest with
	  | [] -> cur
	  | n'::tl -> 
	      match !n'.lvs with
		|  [b] when node_sdom n' n ->
		      find_best n' (Some b) tl
		| _ -> find_best n cur tl
      in
      let frob_partition p =
	List.iter
	  (fun n ->
	     match !n.label with
	       | Val _ -> ()
	       | _ ->
		   match find_best n None p with
		     | Some b ->
			 !n.label <- Val(SSA.Lval b);
			 !n.ops <- []
		     | None -> ()
	  )
	  p
      in
	List.iter (fun p -> frob_partition !p) vginfo.partitions


    let rec vgnode2exp origexp node =
      let content = !node.label in
      let ops = !node.ops in
(*	try ( *)
	  match content with
	      Val v -> SSA.Val v
	    | BinOp b ->
		(match List.map vgnode2val ops with
		   | [v1;v2] ->
		       SSA.BinOp(b,v1,v2)
		   | _ -> failwith "wrong number of ops for binop"
		)
	    | UnOp u ->
		(match List.map vgnode2val ops with
		   | [v] ->
		       SSA.UnOp(u,v)
		   | _ -> failwith "wrong number of ops for unop"
		)
	    | Cast (ct,t) ->
		(match List.map vgnode2val ops with
		   | [v] ->
		       SSA.Cast(ct,t,v)
		   | _ -> failwith "wrong number of ops for binop"
		)
	    | Get t ->
		(match List.map vgnode2val ops with
		   | [v1;v2] ->
		       SSA.Get(v1,v2,t)
		   | _ -> failwith "Invalid get"
		)
	    | Set(t) ->
		(match List.map vgnode2val ops with
		   | [v1;v2;v3] ->
		       SSA.Set(v1,v2,v3,t)
		   | _ -> failwith "wrong number of ops for set"
		)
	    | Phi ->
		let vl =
		  List.map
		    (fun opt ->
		       match !opt with
			 | {label=Val(SSA.Lval x)} | {lvs=x::_} -> x
			 | _ ->
			     failwith "bad Phi")
		    ops
		in
		  SSA.Phi vl
	    | Unknown _ -> origexp
	    | Fun (lv,_) ->
		SSA.Val(SSA.Lval lv)
(*	)
	with _ -> (* print debug warning here *)
	  pwarn "Something bad happened in GVN. FIXME: Ignoring...";
	  origexp
*)
    and vgnode2val node =
      let rec get_lval lvs =
	match lvs with
	  | hd_v :: _ -> SSA.Lval(hd_v)
	  | _ -> failwith "not a value"
      in
	match !node.label with
	    Val v -> v
	  | Fun (lv,_) -> SSA.Lval lv
	  | _ -> get_lval (!node.lvs)
	      
    (**
       [get_vginfo_of_ssa ssa] returns the value graph information of 
       the given [ssa].
    *)
    let get_vginfo_of_ssa ssa =
      let vginfo = new_vginfo 10000 in
      let () = ssa#iter_bb
	(fun b ->
	   update_vginfo vginfo (ssa#get_id b) (ssa#get_info b))
      in
	dprintf "Starting gvn_part";
	dprintf "Nodelist size = %d" (List.length vginfo.nodelist);
	let () = gvn_part vginfo in
	  vginfo
	    
    (**
       [gvn_replacer ssa] replace variables and expressions in [ssa] with
       the value associate to their global value number. The input [ssa] will be
       changed.
       
       For example: Consider
       b = a; c = b; d = b + 1; e = c + 1;
       
       Global value number for a,b,c = 1; d, e, b+1, c+1 = 2
       
       The final cfg after calling [gvn_replacer] is 
       b = a; c = a; d = a + 1; e = d;
       
       Suppose we're interested only e at the end, doing Deadcode elimination
       will reduce the IR code into
       d = a + 1; e = d;
       
       @return vginfo value graph information
    *)
    let gvn_replacer ssa =
      let vginfo = get_vginfo_of_ssa ssa in
      (* let () = pp_vginfo print_string vginfo in *)
      let () = frob_partitions ssa vginfo in
      let vis = object
	inherit nop_ssa_visitor
	method visit_stmt = function
	  | SSA.Move(lv, e) ->
	      let node = VH.find vginfo.lval2node lv in
		ChangeTo(SSA.Move(lv, vgnode2exp e node))
	  | _ -> DoChildren
	method visit_exp = function
	  | SSA.Phi _ -> SkipChildren (* don't replace vars in phi expression *)
	  | _ -> DoChildren
	method visit_value = function
	  | Ssa.Lval v ->
	      ChangeTo(vgnode2val (VH.find vginfo.lval2node v))
	  | _ -> SkipChildren
      end
      in
	ssa#iter_bb (fun b ->
		       let stmts = ssa#get_info b in
			 ssa#set_info b (SSA.stmts_accept vis stmts)
		    );
	vginfo
	  
	  
  end

  let do_gvn arg = ignore(GVN.gvn_replacer arg)


  (** Strongly connected component based value numbering.
      
      Currently we only implement the RPO algorithm, described in
      "SCC-Based Value Numbering" by Keith Cooper and Taylor Simpson.
      http://citeseer.ist.psu.edu/41805.html
  *)
  module SCCVN =
  struct
    module D = Debug.Make(struct let name = "SCCVN" and default=`NoDebug end)
    open D

    type hash = Top | Hash of Ssa.var
    let top = Top
    type expid = 
      | Const of value
      | Bin of binop_type * hash * hash
      | Un of unop_type * hash
      | Cst of cast_type * typ * hash
      | Unique of var
      | Gt of hash * hash * typ
      | St of hash * hash * hash * typ
      | Ph of hash list
	  
    type rpoinfo = { (* private to the SCCVN module *)
      vn_h : hash VH.t; (* maps vars to value numbers *)
      eid2vn : (expid, hash) Hashtbl.t; (* maps expids to value numbers *)
      vn2eid : (hash, expid) Hashtbl.t; (* inverse of eid2vn *)
    }

    
    let add_eid info eid =
      let h = Hash(newvar "constant" TString (* meaningless type *)) in
	Hashtbl.add info.eid2vn eid h;
	Hashtbl.add info.vn2eid h eid;
	h

    let add_hash info var eid =
      let h = Hash var in
	Hashtbl.add info.eid2vn eid h;
	Hashtbl.add info.vn2eid h eid;
	VH.replace info.vn_h var h;
	h

    let get_expid info =
      let vn = function
	| (Int _ | Str _ | Name _) as v -> (
	    try Hashtbl.find info.eid2vn (Const v)
	    with Not_found -> 
	      add_eid info (Const v)
	  )
	| Lval x -> (
	    try VH.find info.vn_h x
	    with Not_found ->
	      failwith("get_expid: unknown var: "^var_to_string x)
	  )
      in
	fun var -> function
	  | Val(Lval _ as v) ->
	      Hashtbl.find info.vn2eid (vn v) 
	  | Val v -> Const v
	  | BinOp((PLUS|TIMES|BITAND|BITOR|XOR|EQ|NEQ) as op,v1,v2) ->
	      let (h1,h2) = (vn v1, vn v2) in
		if v1 < v2 then Bin(op, h1, h2) else Bin(op, h2, h1)
	  | BinOp(op,v1,v2) -> Bin(op, vn v1, vn v2)
	  | UnOp(op, v) -> Un(op, vn v)
	  | Cast(ct, t, v) -> Cst(ct,t, vn v)
	  | Unknown _ -> Unique var
	  | Get(m,i,t) -> Gt(vn m, vn i, t)
	  | Set(m,i,v,t) -> St(vn m, vn i, vn v, t)
	  | Phi vars -> Ph(List.map (fun v -> vn (Lval v)) vars)
	      
    let lookup info var exp =
      try
	let eid = get_expid info var exp in
	  try Hashtbl.find info.eid2vn eid
	  with Not_found ->
	    match eid with
	      | Const(Lval _) -> top
	      | _ ->
		  add_hash info var eid
      with Not_found -> (* no VNs for subexpressions yet *)
	top
	  


    module Dfs = Graph.Traverse.Dfs(G)

    let fold_postfix_component f g v i=
      let acc = ref i in
	Dfs.postfix_component (fun x -> acc := f x !acc) g v;
	!acc

      
    let rpo cfg =
      let info = {
	vn_h = VH.create 57;
	eid2vn = Hashtbl.create 57;
	vn2eid = Hashtbl.create 57;
      }
      in
	(* Contrary to the paper, only assigned SSA variables should have
	   their hashes set to Top. Otherwise, uninitialized variables are
	   all equivalent. *)
      let rec filter l = function
	| Move(v,e) ->
	    VH.add info.vn_h v top;
	    (v,e)::l
	| Call(Some v, _, _) ->
	    ignore(add_hash info v (Unique v));
	    l
	| Attr(s, _) -> filter l s
	| _ -> l
      in
      let moves = (* extract the moves only once *)
	fold_postfix_component
	  (fun b l ->
	     List.fold_left filter l (List.rev(cfg#get_info(cfg#find b)))
	  )
	  cfg BB_Entry []
      in
      let () = (* add all other uninitialized vars as unique *)
	let vis = object
	  inherit nop_ssa_visitor
	  method visit_rvar x =
	    if not(VH.mem info.vn_h x) then (
	      dprintf "Adding uninitialized variable %s" (var_to_string x);
	      ignore(add_hash info x (Unique x));
	    );
	    DoChildren
	end
	in
	  cfg#iter_bb (fun b -> ignore(stmts_accept vis (cfg#get_info b)));
      in
      let vn x = 
	try VH.find info.vn_h x
	with Not_found -> failwith("vn: Unknown var: "^var_to_string x)
      in
      let lookup = lookup info in
      let changed = ref true in
	while !changed do
	  changed := false;
	  List.iter
	    (fun (v,e) ->
	       let oldvn = vn v in
	       let temp = lookup v e in
		 if oldvn <> temp && temp <> top then (
		   changed := true;
		   VH.replace info.vn_h v temp
		 ) )
	    moves
	done;
	(******** END OF ALGORITHM FROM PAPER ******)
	let inverse = Hashtbl.create (VH.length info.vn_h) in
	let () = VH.iter (fun k v -> Hashtbl.add inverse v k) info.vn_h in
	let hash2equiv = Hashtbl.find_all inverse in
	let vn2eid = Hashtbl.find info.vn2eid in
	  (*	let () =
	  if debug then (
	    List.iter
	      (fun (v,_) -> pdebug (List.fold_left (fun s v -> s^var_to_string v^" ") "[" (hash2equiv(vn v)) ^"]"))
	      moves
	  )
	in *)
	  (vn,hash2equiv,vn2eid)


    let hash_replacement hash2equiv vn2eid defsite psdom =
      let remove_dominated vars =
	let lt (_,d) (_,d') = psdom d d' in
	let rec extract_roots found = function
	  | [] -> found
	  | first::rest ->
	      let (min,rest) =
		List.fold_left
		  (fun (m,r) x -> if lt m x then (m,x::r) else (x,m::r))
		  (first,[]) rest
	      in
		if List.exists (fun x -> lt x min) found then
		  found
		else
		  extract_roots (min::found) rest
	in
	let var_defsites = List.rev_map (fun x -> (x, defsite x)) vars in
	  List.map fst (extract_roots [] var_defsites)
      in
      (* cache the variables that are not dominated by an equivalent variable *)
      let myequiv_ht = Hashtbl.create 5700 in
      let hash2equiv x =
	try Hashtbl.find myequiv_ht x
	with Not_found ->
          let res = remove_dominated (hash2equiv x) in
            Hashtbl.add myequiv_ht x res;
            res
      in
	fun pos hash ->
	  let rec find_best p rest =
	    match rest with
	      | [] -> None
	      | v'::tl ->
		  let p' = defsite v' in
		    if psdom p' p then
		      Some v'
		    else find_best p tl
	  in
	    match vn2eid hash with
	      | Unique v when psdom (defsite v) pos ->
		  Some(Lval v)
	      | Const c ->
		  Some c
	      | _ ->
		  match find_best pos (hash2equiv hash) with
		    | Some v -> Some(Lval v)
		    | None -> None


    let replacer cfg =
      let () = pdebug "Running rpo algorithm" in
      let (vn,hash2equiv,vn2eid) = rpo cfg in
      let () = pdebug "Compting dominators" in
      let psdom = pos_sdom cfg in
      let () = pdebug "Computing defsites" in
      let defsite = defsite cfg in
      let hash_replacement = hash_replacement hash2equiv vn2eid defsite psdom in
      let changed = ref false in
      let vis = object
	inherit nop_ssa_visitor
	val mutable pos = (BB_Entry,0)
	method set_pos p = pos <- p
	method visit_value = function
	  | Ssa.Lval v ->
	      (match hash_replacement pos (vn v) with
		 | Some(Ssa.Lval var) when v == var -> SkipChildren
		 | Some v' ->
		     changed := true;
		     dprintf "Replacing var %s with %s" (var_to_string v) (value_to_string v');
		     ChangeTo v'
		 | None -> SkipChildren
	      )
	  | _  -> SkipChildren

	method visit_stmt = function
	  | Ssa.Move(_,Val _) -> (* visit value will handle that properly *)
	      DoChildren
	  | Ssa.Move(v,e) -> (
	      match hash_replacement pos (vn v) with
		| Some vl ->
		    changed := true;
		    dprintf "Replacing exp %s with %s" (exp_to_string e) (value_to_string vl);
		    ChangeTo(Move(v, Val vl))
		| None -> DoChildren
	    )
	  | _ -> DoChildren
      end
      in
      let replace b =
	let bid = cfg#get_id b in
	let stmts = 
	  List.mapi
	    (fun i s ->
	       vis#set_pos (bid,i);
	       stmt_accept vis s
	    )
	    (cfg#get_info b)
	in
	  cfg#set_info b stmts
      in
	pdebug "Doing replacement";
	cfg#iter_bb replace;
	!changed


    let aliased cfg =
      let (vn, _, _) = rpo cfg in
	fun x y -> match (x,y) with
	  | (Int(i,_), Int(i',_)) ->
	      Some(i = i')
	  | (Name x, Name y) when x = y ->
	      Some true
	  | (Lval x, Lval y) when vn x = vn y ->
	      Some true
	  | _ -> 
	      (* We could also check whether an lval was assigned a constant,
	       * but running SCCVN.replace would get rid of any references to
	       * such variables anyways. *)
	      None

  end (* SCCVN *)


  (** Removes CJmps where the conditional is a constant.
      @note This can create unreachable code, which should be removed
      separately.
  *)
  module ConstantCJmp =
  struct
    
    let replace_one cfg b =
      match List.rev (cfg#get_info b) with
	| CJmp(Int(1L,REG_1), taken, Name(not_taken))::revstmts
	| CJmp(Int(0L,REG_1), Name(not_taken), taken)::revstmts ->
	    let newstmts = match taken with
	      | Name _ -> List.rev revstmts
	      | _ -> Jmp taken :: List.rev revstmts
	    in
	      cfg#remove_edge b (cfg#find_label not_taken);
	      cfg#set_info b newstmts;
	      true
	| _ ->
	    false
	      
    let replace (cfg: Ssa.stmt list Vine_cfg.cfg) =
      cfg#fold_bb (fun b changed -> replace_one cfg b || changed) false

  end (* ConstantCJmp *)

  let simplify_graph cfg ?(globals=[]) (n:int) =
    if D.debug && not(well_defined cfg) then
      raise(TypeError "simplify_graph: given cfg not well defined");
    let k = ref 0 in 
    let changed = ref true in
      while(!k <> n && !changed) do
	k := !k+1;
	D.dprintf "simplify_graph: iteration %d: starting SCCVN" (!k);
 	let x1 = SCCVN.replacer cfg in
	(* let () = do_gvn cfg in *)
	D.dprintf "simplify_graph: iteration %d: starting constant prop" (!k);
	let (x2:bool) = constant_propagation_and_folding cfg
	  (Ssa.simplify_stmt)  in
	D.dprintf "simplify_graph: iteration %d: replacing cjmps" (!k);
	let x5 = ConstantCJmp.replace cfg in
	let () = if x5 then  Ssa.remove_unreachable cfg in
	D.dprintf "simplify_graph: iteration %d: scalar available" (!k);
	let (x4:bool) = scalar_available_simplify cfg in
	D.dprintf "simplify_graph: iteration %d: DCE" (!k);
 	let (x3:bool) = do_dce ~globals:globals cfg in 
	changed :=  x1 || x2 || x3 || x4 || x5
      done;
      (cfg,!changed)

end


let simplify_graph cfg ?(globals=[]) n = 
  let () = Vine_cfg.coalesce_bb cfg in 
  let ssacfg = Ssa.cfg2ssa  cfg in 
  let (ssacfg,changed) = SsaDataflow.simplify_graph ssacfg n in
(*  let () = Ssa.rm_phis ssacfg in 
  let (cfg:Vine.stmt list Vine_cfg.cfg) = Ssa.cfg2vine ssacfg in  *)
  let cfg = Ssa.to_vine ssacfg in 
  let () = cfg#iter_bb 
    (fun bb -> 
       let stmts = cfg#get_info bb in
       let stmts = Vine_opt.coalesce_stmts stmts 1 in 
	 cfg#set_info bb stmts
    ) in
    (cfg,changed)


module AbsVar =
struct
  type t = RegOrTemp of Vine.var | LocalVar of int64 * int64

  let hash x = 
      match x with 
	  RegOrTemp(i,_,_) -> i
	| LocalVar(i,j) -> (Int64.to_int i)
    
  let equal x y =
    x == y ||
      match (x,y) with
	  RegOrTemp(x), RegOrTemp(y) -> (
	    match (x,y) with
	      | ((x,_,_), (y,_,_)) when x == y ->
		  true
	      | _ -> false
	  )
	| LocalVar (x1,x2), LocalVar (y1,y2) -> (
	    if ((x1 == y1) && (x2 == y2))
	    then true else false
	  )
	| _ , _ -> false
	    

  (* is it faster to use the generic compare, or < and = ? 
  let compare x y = 
    match (x,y) with
	RegOrTemp(x,_,_ ), RegOrTemp(y,_,_) -> (
	  compare x y
	)
      | LocalVar (x1,x2), LocalVar (y1,y2) -> (
	    if ((x1 == y1))
	    then compare x2 y2 
	    else compare x1 y1
	)
      | RegOrTemp (x) , _ -> -1
      | _, _ -> 1
	
  *)
end

module AbsVarHash = Hashtbl.Make(AbsVar)
module AbsVarHU = Vine_util.HashUtil(AbsVarHash)

let find_var_for_reg regname decls =
  let vardecl = try List.find (fun (num, name, typ) -> 
				 if ((String.compare regname name) == 0)
				 then true else false) decls
  with Not_found ->  failwith ("Could not find requested Variable in Vardecls\n")
  in
    vardecl
      
module StackOffProp =
struct

  module Debug = Debug.Make(struct let name = "StackOffProp" and default=`Debug end)

  module G = G

  module VH = AbsVarHash

  type rangetype = IntVal of int64 | Infinity 
   
  type staddrtype = ConstStackAddr of int64 | RangeStackAddr of rangetype * rangetype
    
  type stofftype =  NonConstOff | ConstOff of int64   

  type cptype = Unknown | OffType of stofftype | StackAddr of staddrtype | Top  

  let spreg_var = ref (AbsVar.RegOrTemp (1, "R_ESP", REG_32))

  module L =
  struct
    type t = Ht of (cptype VH.t) 


    let top = 
      let ht = (VH.create 1) in 
      let ()  = VH.add ht (!spreg_var) Top in
	Ht (ht)

    let t_to_ht (Ht h1) = h1
    let lookup ht k = 
      (try VH.find ht k 
       with Not_found -> Unknown)

    let cpmeet k v1 v2 = 
      match v1,v2 with
	  Unknown , _ -> Unknown 
	| _ , Unknown -> Unknown
	| Top , _ -> v2
	| _ , Top -> v1
	| StackAddr (ConstStackAddr (s1)), StackAddr (ConstStackAddr (s2)) -> 
	    if s1 = s2 
	    then v1 
	    else StackAddr (RangeStackAddr (Infinity, Infinity))
	| StackAddr (ConstStackAddr (s1)), StackAddr (RangeStackAddr (s2, s3)) -> v2
	| StackAddr (RangeStackAddr (s1, s2)), StackAddr (ConstStackAddr (s3)) -> v1
	| StackAddr (RangeStackAddr (s1, s2)), StackAddr (RangeStackAddr (s3, s4)) -> v1
	    
	| OffType (NonConstOff), OffType (s1) -> v1
	| OffType (s1), OffType (NonConstOff) -> v2
	| OffType (ConstOff (c1)), OffType (ConstOff (c2)) -> if c1 = c2 then v1 else OffType (NonConstOff)
	    
	| StackAddr (s1), OffType (s2) -> StackAddr (RangeStackAddr (Infinity , Infinity))
	| OffType (s1), StackAddr (s2) -> StackAddr (RangeStackAddr (Infinity , Infinity))
	    
    let cpequal v1 v2 = 
      match v1,v2 with
      	  Unknown , Unknown -> true
	| Top , Top -> true
	| StackAddr (ConstStackAddr (s1)), StackAddr (ConstStackAddr (s2)) -> if s1 = s2 then  true else false 
	| StackAddr (RangeStackAddr (s1, s2)), StackAddr (RangeStackAddr (s3, s4)) -> true
	| OffType (ConstOff (c1)), OffType (ConstOff (c2)) -> if c1 = c2 then true else false
	| OffType (NonConstOff) , OffType (NonConstOff) -> true
	| _ ->   false 
	    
    let equal (Ht ht1) (Ht ht2) =
      AbsVarHU.hashtbl_eq ~eq:cpequal  ht1 ht2


    let keep_only_registers ht = 
      ht
(*      let newht = VH.create 7 in
	VH.iter (fun k v -> match k with
	(x,_, _) -> 
	if x < 9 then VH.add newht k v 
	else ())
	ht;
	VH.clear ht;
	newht
*)


    let meet v1 v2 = 
      let (h1:cptype VH.t) = t_to_ht v1 in
      let (h2:cptype VH.t) = t_to_ht v2 in
      let h22 = VH.copy (h2) in 
	VH.iter 
          (fun k v1 ->
             let v2 = lookup h22 k in
             let nv = cpmeet k v1 v2 in 
               VH.replace h22 k nv
          ) h1; Ht(keep_only_registers h22)

  end (* module L *)

  open L


  let print_cptype v =
    match v with
	Unknown -> Printf.printf "Unknown"
      | OffType (NonConstOff) -> Printf.printf "NonConst"
      | OffType (ConstOff (l)) -> Printf.printf "%s" (Int64.to_string l)
      | StackAddr (ConstStackAddr (l)) -> Printf.printf "SPADDR (%s)" (Int64.to_string l)
      | StackAddr (RangeStackAddr (_, _)) -> Printf.printf "StackRange"
      | Top -> Printf.printf "Top"

  let addvals func a1 a2 =
    match a1,a2 with

      | StackAddr (s1), Unknown -> StackAddr (RangeStackAddr (Infinity, Infinity))
      | Unknown, StackAddr (s1) -> StackAddr (RangeStackAddr (Infinity, Infinity))
      |	StackAddr (s1), Top -> StackAddr (RangeStackAddr (Infinity, Infinity))
      | Top, StackAddr (s1) -> StackAddr (RangeStackAddr (Infinity, Infinity))
      | StackAddr (ConstStackAddr (s1)), StackAddr (ConstStackAddr (s2)) -> Unknown
      | StackAddr (ConstStackAddr (s1)), StackAddr (RangeStackAddr (s2, s3)) -> Unknown
      | StackAddr (RangeStackAddr (s1, s2)), StackAddr (ConstStackAddr (s3)) -> Unknown
      | StackAddr (RangeStackAddr (s1, s2)), StackAddr (RangeStackAddr (s3, s4)) -> Unknown
      	  
      | Unknown, _  -> Unknown
      | _ , Unknown -> Unknown
	  
      | Top, _ -> Unknown
      | _ , Top -> Unknown

      | OffType (NonConstOff), OffType (s1) -> a1
      | OffType (s1), OffType (NonConstOff) -> a2
      | OffType (ConstOff (c1)), OffType (ConstOff (c2)) -> OffType (ConstOff (func c1 c2))
	  
      | StackAddr (s1), OffType (s2) -> 
	  (match s1, s2 with
	       ConstStackAddr (x1), ConstOff (x2) -> 
		 StackAddr (ConstStackAddr (func x1 x2))
	     | _ -> StackAddr (RangeStackAddr (Infinity , Infinity))
	  )
	    
      | OffType (s2), StackAddr (s1) -> 
	  (match s1, s2 with
	       ConstStackAddr (x1), ConstOff (x2) -> 
		 StackAddr (ConstStackAddr (func x1 x2))
	     | _ -> StackAddr (RangeStackAddr (Infinity , Infinity))
	  )
	  
      
  let lookup_exp ht e =
    match e with
	Lval (Temp (x)) -> lookup ht (AbsVar.RegOrTemp (x))
      | Constant (Int (a, b)) -> OffType (ConstOff (b))
      | _ -> Unknown
	  

  let simplify_arith (ht : (cptype VH.t)) (b : binop_type) (v1 : Vine.exp) (v2 : Vine.exp) =
    let absval1 = lookup_exp ht v1 in
    let absval2 = lookup_exp ht v2 in
      match b with 
	  PLUS -> addvals Int64.add absval1 absval2  
	| MINUS -> addvals Int64.sub absval1 absval2
	| BITAND -> addvals Int64.logand absval1 absval2
	| BITOR -> addvals Int64.logor absval1 absval2
	| _ -> Unknown
	    
  let stmt_transfer newht stmt = 
      match stmt with
	  Move(Temp(x), v)  -> ( 
				  match v with
				      Constant (Int (t, cval)) ->
					(try VH.replace newht (AbsVar.RegOrTemp (x)) (OffType (ConstOff (cval)))
					 with Not_found -> ()
					)
				    | Lval (Temp(t')) ->
					(try VH.replace newht (AbsVar.RegOrTemp (x)) (VH.find newht (AbsVar.RegOrTemp (t')))
					 with Not_found -> () 
					)
				    | BinOp (binop, v1, v2) ->
					(try VH.replace newht (AbsVar.RegOrTemp (x)) (simplify_arith newht binop v1 v2)
					 with Not_found -> ()
					)
				    | _ -> 
					(try VH.replace newht (AbsVar.RegOrTemp (x)) (Unknown)
					 with Not_found -> ()
					)

			       )
	| Call ( _ , _ , _ ) -> ( 
(*				  let newval = (simplify_arith newht PLUS (Lval (Temp (!spreg_var))) 
						  (const_of_int REG_64 4)) in
				    VH.replace newht !spreg_var newval; *)
				    ())
	| _ -> ()
	    
	    

  let init_sp ctx = 
    let newctx = ctx in
      VH.remove newctx !spreg_var;
      VH.add newctx !spreg_var (StackAddr (ConstStackAddr (Int64.of_int 0)));
      newctx 

  let s0 = Vine_cfg.BB_Entry

  let init = 
    let inited = Ht (init_sp (VH.create 7)) in
      inited
	
  let graph_specific_init graph =
    spreg_var := AbsVar.RegOrTemp (find_var_for_reg "R_ESP" Asmir.x86_regs);
    init
      
  let transfer_function graph node t  = 
    let stmts = graph#get_info (graph#find node) in 
    let ht = VH.copy (t_to_ht t) in 
    let retht =  List.fold_left ( fun htt s -> stmt_transfer htt s ; htt ) ht  stmts in
      Ht (keep_only_registers retht)
	
  let dir = Dataflow.Forward


end

module SPProp = Make(StackOffProp);;


let stackoff_propagation ?(init=StackOffProp.init) graph = 
  let (in_f,out_f)  = SPProp.topological_worklist_iterate ~init:(StackOffProp.graph_specific_init graph) graph  in
    (in_f,out_f)
      

let stackoff_propagation_and_folding ?(init=StackOffProp.init) graph  = 
  let (in_f, _) = stackoff_propagation ~init:init graph in 
    graph#iter_bb 
      (fun bb -> 
         let () = StackOffProp.Debug.dprintf "\n======BLOCK %s======\n"
           (Vine_cfg.bbid_to_string (graph#get_id bb)) in
	 
	 let print_blk_label bb = (
	   let str = Vine_cfg.label_printer graph bb in 
	     Printf.printf "\t%s [%s];\n" (Vine_cfg.bbid_to_string (graph#get_id bb))
	       (String.escaped str)
	 )
	 in
	 let () = print_blk_label bb in
         let intbl = in_f (graph#get_id bb) in 
	 let ht = StackOffProp.L.t_to_ht intbl in
	   StackOffProp.print_cptype (StackOffProp.L.lookup ht (!StackOffProp.spreg_var))        
      );

    ()

let get_max_stack_depth ?(init=StackOffProp.init) graph  = 
  let max_depth = ref (Int64.of_int 0) in
  let (in_f, _) = stackoff_propagation ~init:init graph in 
    graph#iter_bb 
      (fun bb -> 
         let stmts = graph#get_info bb in 
         let intbl = in_f (graph#get_id bb) in 
	 let ht = StackOffProp.L.t_to_ht intbl in
	 let current_ht = StackOffProp.VH.copy (ht) in
           List.iter
             (fun s -> 
		let () = StackOffProp.stmt_transfer current_ht s in
		  match StackOffProp.L.lookup current_ht !StackOffProp.spreg_var with
		      StackOffProp.StackAddr (StackOffProp.ConstStackAddr (b)) -> 
			if (!max_depth < b) then () 
			else max_depth := b; ()
		    | StackOffProp.Top -> ()
		    | StackOffProp.StackAddr (StackOffProp.RangeStackAddr (_,_)) -> ()
		    | _ -> 
			max_depth := Int64.of_int (-10000); ()
             ) stmts 
      );
  !max_depth


module SetOfInt64 =  Set.Make(Int64);;

(* Returns a set of all stack offsets seen in memory reads or writes,
   within this function.  This does not deal with passing of addresses
   through registers or memory to be accessed in other functions.
*)

let get_local_vars ?(init=StackOffProp.init) graph  =

  let fpset = ref (SetOfInt64.empty) in

  let vis = object(self)
    inherit nop_vine_visitor

    val mutable ctx = StackOffProp.VH.create 7

    (* Not functional yet -- this flag is to isolate those variables
       that have addresses copied to memory. This is used to
       accumulate those addresses that are ever written to memory. *)
    val mutable writing_to_mem = false
      
    method set_ctx c = ctx <- c
      
    method get_ctx  = ctx

    method set_writ c = writing_to_mem <- c
      
    method get_writ = writing_to_mem 
      
    method visit_rlvalue e =
      match e with 
	  Mem (x, y, z) ->
	    (match y with 
		 Lval (Temp (t)) | BinOp ( _, Lval (Temp (t)), _) 
	       | BinOp ( _, _, Lval(Temp (t))) ->
		   (match (StackOffProp.L.lookup (ctx) (AbsVar.RegOrTemp (t))) with
			StackOffProp.StackAddr (StackOffProp.ConstStackAddr (b)) ->
			  fpset := SetOfInt64.add ((Int64.of_int (Int64.to_int b))) !fpset; ()
		      | _ -> ()
		   )
	       | _ -> 
		   ()
	    ) ;      
	    DoChildren    
	| _ -> 
	    DoChildren
	      
	      
    method visit_alvalue e =
      match e with 
	  Mem (x, y, z) ->
	    (match y with 
		 Lval (Temp (t)) | BinOp ( _, Lval (Temp (t)), _) 
	       | BinOp ( _, _, Lval(Temp (t))) ->
		   (match (StackOffProp.L.lookup (ctx) (AbsVar.RegOrTemp (t))) with
			StackOffProp.StackAddr (StackOffProp.ConstStackAddr (b)) ->
			  fpset := SetOfInt64.add ((Int64.of_int (Int64.to_int b))) !fpset; ()
		      | _ -> ()
		   )
	       | _ -> 
		   ()
	    ) ;      
	    DoChildren    
	| _ -> DoChildren
	    
    method visit_exp e =
      (match e with
	 | Lval (Mem (x, y, z)) -> 
	     (match y with 
		  Lval (Temp (t)) | BinOp ( _, Lval (Temp (t)), _) 
		| BinOp ( _, _, Lval(Temp (t))) ->
		    (match (StackOffProp.L.lookup (ctx) (AbsVar.RegOrTemp (t))) with
			StackOffProp.StackAddr (StackOffProp.ConstStackAddr (b)) ->
			  fpset := SetOfInt64.add ((Int64.of_int (Int64.to_int b))) !fpset; ()
		       | _ -> ()
		    )
		| _ -> 
		    ()
	     ) ;      

	 | Lval (Temp (t)) ->
	     (match (StackOffProp.L.lookup (ctx) (AbsVar.RegOrTemp (t))) with
		  StackOffProp.StackAddr (StackOffProp.ConstStackAddr (b)) ->
		    fpset := SetOfInt64.add ((Int64.of_int (Int64.to_int b))) !fpset; ()
		| StackOffProp.Unknown -> ()
		| _ -> ()
	       )
	 | _ -> ());
      DoChildren
  end in
    
  let check_operand ht stmt = 
    (
      vis#set_ctx ht;
      (match stmt with
	 | Move(Mem (_,_,_), exp)  -> 
	     vis#set_writ true;
	 | Call(Some(Temp x),_,_) ->
	    ()
	 | _ -> ());
      
      let _ =  stmt_accept vis stmt in
	vis#set_writ false;
	();
	
    )  in
    
  let (in_f, _) = stackoff_propagation ~init:init graph in
    graph#iter_bb
      (fun bb ->
(*	 let print_blk_label bb = (
	   let str = Vine_cfg.label_printer graph bb in 
	     Printf.printf "\tInside block %s [%s];\n" (Vine_cfg.bbid_to_string (graph#get_id bb))
	       (String.escaped str)
	 )
	 in
	 let () = print_blk_label bb in
*)
         let stmts = graph#get_info bb in
         let intbl = in_f (graph#get_id bb) in
	 let ht = StackOffProp.L.t_to_ht intbl in
	 let current_ht = StackOffProp.VH.copy (ht) in
           List.iter
             (fun s ->
		StackOffProp.stmt_transfer current_ht s;
		check_operand current_ht s 
             ) stmts
      );
    !fpset
      
