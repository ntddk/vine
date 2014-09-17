(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Guarded Command Language

    Type declarations for the Guarded Command Language, and a function
    to traslate a VinE trace into GCL.
    
    Other generally useful functions that deal with the GCL should be
    added here.  
*)

open Vine
open Vine_cfg
open Vine_util

module D = Debug.Make(struct let name = "GCL" and default=`Debug end)
open D

type lvalue = Vine.lvalue
type exp = Vine.exp


(** A GCL expression.
    [Skip] does nothing.
    [Assign] assigns a value to an lvalue.
    [Seq(a,b)] evaluates [a] and then moves on to [b].
    [Choice(a,b)] 
    [Assume] goes on to the next expression in the sequence if it is true.
    [Assert] doesn't start when the condition is not true.
*)
type gcl = Assume of exp
	   | Assign of lvalue * exp
	   | Assert of exp
	   | Choice of gcl * gcl
	   | Seq of gcl * gcl
	   | Skip 
	  
type t = gcl

(** Function for removing jmps in a trace that just go to the next
    stmt. This was added so that of_straightline works correctly and
    doesn't just ignore jumps. *)
let  remove_trace_jmps trace =
  let rec f acc rest = 
    match rest with
	Jmp(Name(x))::Label(y)::ys when x = y -> f acc ys
      | Jmp(Name(x))::Label(y)::ys -> 
	  raise (Invalid_argument "Trace jump not to next stmt")
      | x::ys -> f (x::acc) ys
      | _ -> acc
  in
    List.rev (f [] trace)
;;

(** Convert a straightline trace into GCL.

    A straightline trace cannot have any CJmps, and any Jmps it might have must
    jump to the label following them. (Ie, any jump must be a no-op)
*)
let rec of_straightline ?(acc=Skip) (trace: Vine.stmt list) =
  let rec prepend g s =
    match s with
	Move(l,e) -> Seq(Assign(l,e), g)
      | CJmp _ -> raise(VineError "found a CJmp in a trace")
      | Jmp _ ->
	  raise (Invalid_argument "found Jmp in straightline GCL")

      | Comment _
      | Label _ ->
	  g
      | ExpStmt _ -> 
	  wprintf "Skipping ExpStmt";
	  g
	    (* Seq(Skip, g) *)
      | Halt _ 
      | Special _ 
      | Function _ 
      | Return _
      | Call _  -> 
	  raise (Invalid_argument ("Found special,function,halt,or return in"^
		   "straightline code"))
      | Block(_, sl) -> of_straightline ~acc:g sl
      | Attr(s,_) ->
	  pwarn "Removing attribute during gcl creation";
	  prepend g s
      | Vine.Assert(e) -> Seq(Assert(e), g) 
  in
    (* fold_left of reversed list, rather than fold_right, because
     * fold_right is not tail recursive *)
    List.fold_left prepend acc (List.rev (remove_trace_jmps trace))




module RevCFG =
struct
  type t = G.t
  module V = G.V
  let iter_vertex = G.iter_vertex
  let iter_succ = G.iter_pred
  let in_degree = G.out_degree
end

module Toposort = Graph.Topological.Make(RevCFG);;

(* type used internaly by of_cfg *)
type cfg_gcl =
    CAssign of bbid
  | CChoice of exp * cfg_gcl * cfg_gcl (* bb with cjmp, true  and false branches *)
  | Cunchoice of cfg_gcl * cfg_gcl (* unfinished choice *)
  | CSeq of cfg_gcl list



(** [of_cfg cfg exit_node] will compute a function from entry node to 
    gcl between the entry node and the exit node. [cfg] must be acyclic. *)
let of_cfg (cfg:stmt list Vine_cfg.cfg) (exit_node:bbid) =
  (* our latice isa list option of GCL expressions to be put in sequence *)
  let meet p1_gcl p2_gcl = 
    match ( p1_gcl, p2_gcl ) with
	  (None, g2) -> g2
	| (g1,None) -> g1
	| (Some [], _) -> failwith "meet [] _"
	| (_, Some []) -> failwith "meet _ []"
	| (Some l1, Some l2) ->
	    let (su, g1, g2) = split_common_suffix l1 l2 in
	      Some(Cunchoice(CSeq g1, CSeq g2)  :: su)
  in
    (* a skip in this context is a CSeq(CSeq [],..) and the like *)
  let rec remove_skips g = 
    match g with
	CAssign _ -> g
      | CSeq [] -> g
      | CSeq sl ->
	  (* <@ is the composition operator from vine_util. This
	     recursively goes through sl and calls remove_skips on
	     each list item, along with the filtering. *)
	  CSeq(list_filter_some
		 ((function CSeq[] -> None | x -> Some x) <@ remove_skips) sl )
      | CChoice(c,s1,s2) -> CChoice(c, remove_skips s1, remove_skips s2)
      | Cunchoice(s1,s2) -> Cunchoice(remove_skips s1, remove_skips s2)
  in
    (* finds first assignment *)
  let rec find_first s =
    match remove_skips s with
	CAssign b -> b
      | CSeq(h::_) -> find_first h
      | CSeq [] -> failwith "shouldn't ever get here tnh99btcn"
      | CChoice _ -> failwith "shouldn't ever get here tnh9rh203"
      | Cunchoice _ -> failwith "shouldn't ever get here tnh982h9o"
  in
    (* find cjmp in a block. assumes it is the last statement in the
       block *)
  let rec find_cjmp = function
      [] -> None
    | stmts ->
	match list_last stmts with
	    CJmp(c,t,f) -> Some(c,t,f)
	  | Attr(s,_) -> find_cjmp [s]
	  | s -> None
  in
  let find_target e =
    match e with
	Name(l) -> Vine_cfg.find_label cfg l
      | _ -> failwith "indirect jump not supported yet"
  in
    (* transfer function *)
  let f_t n l =
    let r =
    match l with
	Some(Cunchoice(bb1, bb2)::rest as exp) -> 
	  (match find_cjmp (bb_stmts cfg n) with
	       Some(cond,tt,ft) ->
	  let (bbt,bbf) =
	    match (find_first (CSeq(bb1::rest)), find_first (CSeq(bb2::rest)),
		   find_target tt, find_target ft) with
		(b1,b2,bt,bf) when b1 = bt && b2 = bf -> (bb1,bb2)
	      | (b1,b2,bt,bf) when b2 = bt && b1 = bf -> (bb2,bb1)
	      | (b1,b2,bt,bf) ->
		  failwith(Printf.sprintf "choice seems to not correspond to cjmp %s %s %s %s at %s"
			     (bbid_to_string b1) (bbid_to_string b2)
			     (bbid_to_string bt) (bbid_to_string bf)
			     (bbid_to_string n) )
	  in
	    Some(CAssign n::CChoice(cond, bbt, bbf)::rest)
	     | None -> (* No CJmp found *)
		 dprintf "Warning: CJmp expected but not found at end of %s." (bbid_to_string n);
		 Some(CAssign n::exp)
	  )
      | Some exp -> Some(CAssign n::exp)
      | None -> None
    in
    let () = match r with
	Some _ -> dprintf "f_t: %s: have GCL" (bbid_to_string n)
      | None -> dprintf "f_t: %s: still at None" (bbid_to_string n)
    in
      r
  in
  (*
  let nodes = List.rev(Toposort.fold (fun x y -> x::y) cfg []) in
  *)
  let nodes = Vine_cfg.G.fold_vertex (fun x y -> x::y) cfg [] in
  let gcl_map =
    Dataflow.worklist_iterate_in ~nodes:nodes ~entry:exit_node
      ~f_t:f_t ~init:(Some [])
      ~pred:(bb_succ cfg) ~succ:(bb_pred cfg) ~top:None ~meet:meet ~eq:(=)
  in
  let rec cgcl_to_gcl e =
    match e with 
	CAssign b -> 
	  let bb_s = bb_stmts cfg b in (
	      match (List.rev bb_s) with
		| [] ->
		    Skip
		| last::rest ->
		    let stmts =
		      match (remove_stmt_attrs last) with
			| Call _
			| Return _
			| Jmp _
			| CJmp _  ->
			    List.rev rest
			| _ -> bb_s
		    in
		      of_straightline stmts
			
	    )
      | CChoice(cond, e1, e2) ->
	  Choice(Seq(Assume cond, cgcl_to_gcl e1),
		 Seq(Assume(exp_not cond), cgcl_to_gcl e2) )
      | Cunchoice(e1, e2) ->
	  Choice(cgcl_to_gcl e1, cgcl_to_gcl e2)
      | CSeq(e'::es) ->
	  List.fold_left (fun a b -> Seq(a,cgcl_to_gcl b)) (cgcl_to_gcl e') es
      | CSeq [] ->
	  Skip
    
  in
  let in_fun n = f_t n (gcl_map n) in
    (fun n ->
       match in_fun n with
	   Some x -> cgcl_to_gcl (CSeq(x))
	 | None -> failwith "Gcl.of_cfg got Bottom. Does the CFG have non-terminating nodes?"
    )

;;


let of_trace (trace : Vine.program) = 
  let cfg = Vine_cfg.prog_to_cfg trace in
    of_cfg cfg (Vine_cfg.exit_node cfg) (Vine_cfg.entry_node cfg)

let of_trace_ssa (trace : Vine.program) = 
  let cfg = Vine_cfg.prog_to_cfg trace in
  let ssa = Ssa.cfg2ssa cfg in
  let () = Ssa.rm_phis ssa in
  let cfg = Ssa.cfg2vine ssa in 
    of_cfg cfg (Vine_cfg.exit_node cfg) (Vine_cfg.entry_node cfg)

let rec pp_gcl pr gcl = 
  match gcl with
      Assume e ->
	pr "ASSUME "; Vine.pp_exp pr e
    | Assert e ->
	pr "ASSERT "; Vine.pp_exp pr e
    | Assign(l,e) ->
	Vine.pp_lval pr l; pr " = "; Vine.pp_exp pr e
    | Choice(g1,g2) ->
	pr "("; pp_gcl pr g1; pr " [=] "; pp_gcl pr g2; pr ")"
    | Seq(g1,g2) ->
	pp_gcl pr g1; pr "; "; pp_gcl pr g2
    | Skip ->
	pr "SKIP"
	  

let rec apply_f  (f:gcl -> gcl) g = 
  match g with
      Assume _
    | Assert _
    | Assign _
    | Skip -> f g
    | Choice(g1,g2) -> Choice( (apply_f f g1), (apply_f f g2))
    | Seq(g1,g2) -> Seq((apply_f f g1), (apply_f f g2))


let rec remove_skips gcl  =
  let are_both_skips a b t =
    match a,b with
	Skip,Skip -> Skip
      | _ -> t
  in
  match gcl with
      Assume _
    | Assert _
    | Assign _
    | Skip -> gcl
    | Choice(g1,g2) -> 
	let g1' = remove_skips g1 in 
	let g2' = remove_skips g2 in 
	  are_both_skips g1' g2' gcl
    | Seq(g1,g2) -> (
	let g1' = remove_skips g1 in 
	let g2' = remove_skips g2 in
	  match g1',g2' with
	      Skip, Skip -> Skip
	    | Skip, x -> x
	    | x, Skip -> x
	    | _ -> Seq(g1',g2')
      )


  
