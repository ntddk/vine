(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** 
    Static Single Assignment form
*)

open ExtList
open Vine
open Vine_cfg
open Vine_util

module D = Debug.Make(struct let name = "SSA" and default=`Debug end)
open D

module V = Vine
module VH = Vine.VarHash

(** raised by errors in SSA *)
exception SsaError of string

let ssa_err s = raise(SsaError s)

type label = Vine.label
type typ = Vine.typ
type cast_type = Vine.cast_type
type binop_type = Vine.binop_type
type unop_type = Vine.unop_type

(* Since Vine variables now have an int, we use that for SSA too *)
type var = Vine.var
type decl = Vine.decl

type value =
    Int of int64 * typ
  | Str of string
  | Lval of var
  | Name of label (** The address of a label *)


type exp = BinOp of binop_type * value * value
	   | UnOp of unop_type * value
	   | Val of value
	   | Cast of cast_type * typ * value 
	       (** Cast to a new type. *)
	   | Unknown of string (* FIXME: * register_type *)
	   | Get of value * value * typ
	       (** Get(x,y) |-> x[y] *)
	   | Set of value * value * value  * typ
	       (** Set(x,y,z) |-> x with [y] := z *)
	   | Phi of var list 
	       (** Joins variables that were assigned over different paths *)

type lvalue = var

type stmt = Jmp of value (** Jump to a label/address *)
	    | CJmp of value * value * value (** Conditional jump.
					  If e1 is true, jumps to e2,
					  otherwise jumps to e3 *)
	    | Move of lvalue * exp (** Assign the value on the right to the
				       var on the left *)
	    | Label of label (** A label we can jump to *)
	    | Comment of string (** A comment to be ignored *)
	    | Return of value option (** Return a value from a function *)
	    | Call of lvalue option * value * value list 
		(** A call to a function. *)
	    | Assert of value
	    | Halt of value
	    | Attr of stmt * Vine.attribute (** A statment with attributes *)
(* See how to add these later.
	    | Special of string (** A "special" statement. (does magic) *)
	    | ExpStmt of exp (** An expression which is to be ignored *)

	    | Function of string * typ * decl list * stmt (** Umm, a function. Not supported yet. *)

*)

type 'a visit_action = 'a Vine.visit_action

class type ssa_visitor = object
  (** Called when visiting an expression *)
  method visit_exp: exp -> exp visit_action

  (** Called when visiting a declaration *)
  method visit_decl : decl -> decl visit_action
    
  (** Called when visiting a statement *)
  method visit_stmt : stmt -> stmt visit_action

  (** Called when visiting a value *)
  method visit_value : value -> value visit_action

  (** Called when visiting a referenced variable *)
  method visit_rvar : var -> var visit_action

  (** Called when visiting assigned variable.
      Note that in an assignment, referenced variables will be visited first, so
      that this can be used to add the assigned variable to your context.
  *)
  method visit_avar : var -> var visit_action
end

class nop_ssa_visitor : ssa_visitor = object
  method visit_exp _ = DoChildren
  method visit_value _ = DoChildren
  method visit_decl _ = DoChildren
  method visit_stmt _ = DoChildren
  method visit_avar _ = DoChildren
  method visit_rvar _ = DoChildren
end

let rec exp_accept visitor expression = 
  let rec vis exp = 
    match visitor#visit_exp exp with
	SkipChildren -> exp
      | ChangeTo e -> e
      | DoChildren -> vischil exp
      | ChangeDoChildrenPost(e,f) -> f (vischil e)
  and vischil exp = 
    let exp' = 
      match exp with
	  BinOp(bop, v1, v2) -> 
	    let v1' = value_accept visitor v1 in 
	    let v2' = value_accept visitor v2 in 
	    BinOp(bop, v1', v2')
	| UnOp(up, v) -> 
	    let v' = value_accept visitor v in 
	    UnOp(up, v')
	| Val(v) -> 
	    let v' = value_accept visitor v in 
	      Val(v')
	| Cast(ct, t, v) ->
	    let v' = value_accept visitor v in 
	      Cast(ct,t,v')
	| Unknown(s) -> exp
	| Get(v1,v2, t) -> 
	    let v1' = value_accept visitor v1 in 
	    let v2' = value_accept visitor v2 in 
	      Get(v1',v2', t)
	| Set(v1,v2,v3,t ) ->
	    let v1' = value_accept visitor v1 in 
	    let v2' = value_accept visitor v2 in 
	    let v3' = value_accept visitor v3 in 
	      Set(v1',v2',v3',t)
	| Phi(vl) ->
	    let vl' = List.map (rvar_accept visitor) vl in  
	      Phi(vl')
    in
      if (exp = exp') then exp else exp'
  in
    vis expression

and avar_accept visitor v = 
  match visitor#visit_avar v with
      SkipChildren | DoChildren -> v
    | ChangeTo v' -> v'
    | ChangeDoChildrenPost(v', f) -> f v'
and rvar_accept visitor v = 
  match visitor#visit_rvar v with
      SkipChildren | DoChildren -> v
    | ChangeTo v' -> v'
    | ChangeDoChildrenPost(v', f) -> f v'

and value_accept visitor value =
  let rec vis v = 
    match visitor#visit_value v with
	SkipChildren -> v
      | ChangeTo v' -> v'
      | DoChildren -> vischil v
      | ChangeDoChildrenPost(v', f) -> f (vischil v')
  and vischil v = 
    let v' = 
      match v with
	  Lval(var) -> Lval(rvar_accept visitor var)
	| _ -> v
    in
      (* Why do we compare here, but no where else *)
      if v' = v then v else v'
  in
    vis value

and decl_accept visitor d = 
  match visitor#visit_decl d with
      SkipChildren -> d
    | ChangeTo d' -> d'
    | DoChildren -> d
    | ChangeDoChildrenPost(d',f) -> f d'

and stmt_accept visitor stmt = 
  let rec vis s = 
    match visitor#visit_stmt s with
	SkipChildren -> s
      | ChangeTo s' -> s'
      | DoChildren -> vischil s
      | ChangeDoChildrenPost(s', f) -> f (vischil s')
  and vischil s = 
    match s with
	Jmp(l) -> Jmp(value_accept visitor l) 
      | CJmp(c, l1, l2) -> 
	  CJmp(value_accept visitor c,
	       value_accept visitor l1,
	       value_accept visitor l2)
      | Move(lv, e) ->
	  let e = exp_accept visitor e in
	  let lv = avar_accept visitor lv in
	  Move(lv, e)
      | Label _ -> s
      | Comment _ -> s
      | Return(None) -> s
      | Return(Some(v)) -> Return(Some(value_accept visitor v))
      | Call(lv, value, vl) -> 
	  let vl = List.map (value_accept visitor) vl in
	  let lv = option_map (avar_accept visitor) lv in
	  let value = value_accept visitor value in 
	    Call(lv, value, vl)
      | Assert(e) -> Assert(value_accept visitor e)
      | Halt(e) -> Halt(value_accept visitor e)
      | Attr(s,a) -> Attr(stmt_accept visitor s, a)
  in
    vis stmt

let stmts_accept vis stmts =
  List.map (stmt_accept vis) stmts

let cfg_accept vis (cfg: 'a #cfg) = 
  cfg#iter_bb (fun b ->
		 let stmts = cfg#get_info b in
		 let stmts' = stmts_accept vis stmts in
		   if List.exists2 (!=) stmts stmts' then
		     cfg#set_info b stmts'
	      )


(** Create a new "unused" variable with the given name as a base. *)
let newssavar = renewvar

let newvar = newvar

let pp_type pr t = pr(Vine.type_to_string t)



let rec pp_value pr v =
  match v with
      Int(i,t) ->
	pr(Int64.to_string i); pr ":"; pp_type pr t
    | Str s ->
	pr "\""; pr s; pr "\""
  | Lval l ->
      pp_var pr l
  | Name lab ->
      pr "name("; pr lab; pr ")"


let pp_lval = Vine.pp_var

let pp_exp pr e =
  let ppv = pp_value pr in
  match e with
      Val v ->
	pp_value pr v
    | Phi([]) ->
	pr "Phi(***ERROR: empty list***)";
    | Phi(vl) ->
	pr "Phi([";
	pr (list_join (fun x y -> x^", "^y)
	      (List.map (var_to_string) vl) );
	pr "])"
    | BinOp(op,x,y) ->
	ppv x; pr (Vine.binop_to_string op); ppv y
    | UnOp(op,v) ->
	pr(Vine.unop_to_string op); ppv v
    | Cast(ct,t,v) ->
	pr "cast("; ppv v;
	pr (")"^casttype_to_string ct^":"^Vine.type_to_string t)
    | Unknown u ->
	pr "unknown \""; pr u; pr "\""
    | Get(arr,idx,t) ->
	ppv arr; pr "["; ppv idx; pr "]:";
	pp_type pr t
    | Set(arr,idx,v, t) ->
	ppv arr; pr " with ["; ppv idx; pr "] := "; ppv v;
	pr ":"; pp_type pr t

let rec pp_stmt pr s =
  let ppv = pp_value pr in
    match s with
	 Jmp v ->
	   pr "jmp "; ppv v; pr ";\n"
       | CJmp(v1,v2,v3) ->
	   pr "cjmp(";ppv v1; pr ", "; ppv v2; pr ", "; ppv v3; pr ");\n"
       | Move(l,e) ->
	   pp_lval pr l; pr " = "; pp_exp pr e; pr ";\n"
       | Label l ->
	   pr "label "; pr l; pr ":\n"
       | Comment s ->
	   pr "/*"; pr s; pr "*/\n"
       | Return None ->
	   pr "return;\n"
       | Return(Some v) ->
	   pr "return "; pp_value pr v; pr ";\n"
       | Call(r,f,args) ->
	   let () = 
	     match r with
		 None -> ()
	       | Some lv ->
		   pp_lval pr lv; pr " = "
	   in
	   let () = ppv f; pr "("; in
	   let  () =
	     match args with
		 x::xs ->
		   pp_value pr x;
		   List.iter (fun v -> pr ", "; pp_value pr v) xs
	       | [] -> ()
	   in
	     pr ");\n"
       | Assert(e) ->
	   pr "assert("; pp_value pr e; pr ");\n"
       | Halt(e) ->
	   pr "halt("; pp_value pr e; pr ");\n"
       | Attr(s,a) ->
	   pr (attr_to_string a);
	   pp_stmt pr s


let pp2string pp x =
  let buf = Buffer.create 1000 in
  let () = pp (Buffer.add_string buf) x in
    Buffer.contents buf

let stmt_to_string = pp2string pp_stmt

let exp_to_string = pp2string pp_exp 

let value_to_string = pp2string pp_value


(** print out the statements in a block. can be passed to print_dot_cfg *)
let stmt_printer cfg blk =
  bbid_to_string(cfg#get_id blk)^"\n"
  ^ List.fold_left (fun r s -> r^"\n"^
		      (String.escaped (stmt_to_string s))) 
    "" (cfg#get_info blk)


type ctx = var VH.t * var VH.t * (var*var) Stack.t Stack.t
let new_ctx() = (VH.create 570, VH.create 570, Stack.create())
let ctx_lookup (vh,_,_) var =
  try VH.find vh var
  with Not_found -> var

let ctx_extend (vh,to_oldvar,stacks) v v' =
  Stack.push (v,v') (Stack.top stacks);
  VH.add vh v v';
  VH.add to_oldvar v' v

(** Called to add a let variable to the context *)
let ctx_letextend (vh,to_oldvar,_) v v' =
  VH.add vh v v'
  (* FIXME: We didn't used to add these to to_oldvar, do we want to now? *)
  (* VH.add to_oldvar v' v *)

(** Called to remove a let variable from the context *)
let ctx_letunextend (vh,_,_) v =
  VH.remove vh v

let ctx_push (_,_,stacks) =
  Stack.push (Stack.create()) stacks

let ctx_pop (vh,_,stacks) =
  let myvars = Stack.pop stacks in
    Stack.iter (fun (v,_) -> VH.remove vh v) myvars

(* share the strings in the variable names we create, to save memory *)
let ssa_temp_name = "temp"
let ssa_towrite_name = "towrite"
let ssa_retval_name = "retval"

(** @return a reversed lits of SSA stmts and an exp that is equivalent to
    the Vine expression *)
let rec exp2ssaexp (ctx:ctx) ~(revstmts:stmt list) e : stmt list * exp =
  match e with 
      Vine.BinOp(op, e1, e2) -> 
	let (revstmts, v1) = exp2ssa ctx revstmts e1 in
	let (revstmts, v2) = exp2ssa ctx revstmts e2 in
	  (revstmts, BinOp(op,v1,v2))
    | Vine.UnOp(op, e1) ->
	let (revstmts, v1) = exp2ssa ctx revstmts e1 in
	  (revstmts, UnOp(op,v1))
    | Vine.Constant(Vine.Int(t, i)) ->
	  (revstmts, Val(Int(i,t)))
    | Vine.Constant(Vine.Str s) ->
	(revstmts, Val(Str s))
    | Vine.Lval(Vine.Temp name) -> 
	(revstmts, Val(Lval(ctx_lookup ctx name)))
    | Vine.Lval(Vine.Mem((_,_,Array(t2,_)) as name, ei, _)) ->
	let (revstmts, vi) = exp2ssa ctx revstmts ei in
	  (revstmts, Get(Lval(ctx_lookup ctx name), vi, t2))
    | Vine.Lval(Vine.Mem((_,_,Vine.TMem _) as name, ei, t2)) ->
	let (revstmts, vi) = exp2ssa ctx revstmts ei in
	  (revstmts, Get(Lval(ctx_lookup ctx name), vi,t2))	
    | Vine.Lval(Vine.Mem _) ->
	failwith "SSA: malformed mem variable"
    | Vine.Name label ->
	(revstmts, Val(Name label))
    | Vine.Cast(ct,t,e1) ->
	let (revstmts, v1) = exp2ssa ctx revstmts e1 in
	  (revstmts, Cast(ct,t,v1))
    | Vine.Unknown _ ->
	ssa_err "encountered Unknown while translating to SSA"
    | Vine.Let(l, e1, e2) ->
	let Temp v | Mem(v,_,_) = l in
	let v' = newssavar v in
	let revstmts =
	  match l with
	      Temp _ ->
		let (revstmts,e1) = exp2ssaexp ctx revstmts e1 in
		  Move(v',e1)::revstmts
	    | Mem(_,i,t) ->
		let (revstmts,i) = exp2ssa ctx revstmts i in
		let (revstmts,e1) = exp2ssa ctx revstmts e1 in
		  Move(v',Set(Lval v, i, e1,t))::revstmts
	in
	let () = ctx_letextend ctx v v' in
	let (revstmts,e2) = exp2ssaexp ctx revstmts e2 in
	let () = ctx_letunextend ctx v in
	  (revstmts, e2)



(** @return a reversed lits of SSA stmts and a value that is equivalent to
    the Vine expression *)
and exp2ssa ctx ~(revstmts:stmt list) e : stmt list * value =
  (** Make an SSA value for an SSA expression by adding an assignment to
      revstmts if needed *)
  let exp2val (revstmts, exp) =
    match exp with
	Val v -> (revstmts, v)
      | _ ->
	  let t = Vine_typecheck.infer_type None e in
	let l = newvar ssa_temp_name t in
	  (Move(l, exp)::revstmts, Lval l)
  in
    exp2val(exp2ssaexp ctx revstmts e)


(** @return a reversed list of SSA stmts *)
let rec stmt2ssa ctx ~(revstmts: stmt list) s =
  (* heplper function for assigned lvalues.
     Returns revstmts and a function to generate the move given the expression
     to assign.
  *)
  let alval2ssa revstmts l =
    let name = match l with Vine.Temp n | Vine.Mem(n,_,_) -> n in
    let nname = renewvar name in
      match l with
	  Vine.Mem(m, ei,t) ->
	    let (revstmts, vi) = exp2ssa ctx revstmts ei in
	    let assign rs e =
	      let (rs,v2) =
		match e with
		    Val v -> (rs,v)
		  | _ -> 
		      let l = newvar ssa_towrite_name t in
			(Move(l, e)::revstmts, Lval l)
	      in
	      let nm = ctx_lookup ctx m in
		ctx_extend ctx name nname;
		Move(nname, Set(Lval nm, vi,v2,t))::rs
	    in
	      (revstmts, assign)
	| Vine.Temp(name) ->
	    let assign rs e =
	      ctx_extend ctx name nname;
	      Move(nname,e)::rs
	    in
	      (revstmts, assign)
	(* | Vine.Mem _ -> failwith "Malformed mem" *)
  in
  match s with
      Vine.Jmp e1 ->
	let (revstmts,v1) = exp2ssa ctx revstmts e1 in
	  Jmp v1 :: revstmts
    | Vine.CJmp(e1,e2,e3) ->
	let (revstmts,v1) = exp2ssa ctx revstmts e1 in
	let (revstmts,v2) = exp2ssa ctx revstmts e2 in
	let (revstmts,v3) = exp2ssa ctx revstmts e3 in
	  CJmp(v1,v2,v3) :: revstmts
    | Vine.Move(l, e2) ->
	let (revstmts, v2) = exp2ssaexp ctx revstmts e2 in
	let (revstmts, assign) = alval2ssa revstmts l in
	  assign revstmts v2
    | Vine.Call(lo, e, args) ->
	let (revstmts, add_assign, lo ) =  match lo with
	    None -> (revstmts, id, None)
	  | Some l ->  (
		(* We could avoid this temporary assignment for the case when
		   the lval is a temp, but later optimizations should be
		   removing it anyways *)

	      match l with
		  Temp(_,_,t) -> 
		    let l' = newvar ssa_retval_name t in
		    let (rs,assgn) = alval2ssa revstmts l in
		      (rs, (fun rs -> assgn rs (Val(Lval l'))), Some l')
		| Mem(_,_,t') ->
			  let l' = newvar ssa_retval_name t' in
			  let (rs,assgn) = alval2ssa revstmts l in
			    (rs, (fun rs -> assgn rs (Val(Lval l'))), Some l')
	    )
	in
	let (revstmts,revargs) =
	  List.fold_left
	    (fun (revstmts,revargs) arg -> 
	       let (revstmts,arg) = exp2ssa ctx revstmts arg in
		 (revstmts,arg::revargs)
	    )
	    (revstmts,[])
	    args
	in
	let (revstmts,callee) = exp2ssa ctx revstmts e in 
	  add_assign(Call(lo, callee, List.rev revargs)::revstmts)


    | Vine.Return(Some e) ->
	let (revstmts,v) = exp2ssa ctx revstmts e in
	  Return(Some v)::revstmts
    | Vine.Label label ->
	Label label :: revstmts
    | Vine.Comment s ->
	Comment s::revstmts
    | Vine.Return None ->
	Return None::revstmts

    | Vine.ExpStmt _
	(* We can throw this out, since keeping it is meaningless. *)
    | Vine.Function _ ->
	(* Drop function definitions and declarations. If you want functions,
	   build a supergraph rather than declaring/defining them inside basic
	   blocks.
	*)
	revstmts
    | Vine.Special _ -> 
	raise (Invalid_argument "SSA: Impossible to handle specials. They should be replaced with their semantics.")
    | Vine.Block _ ->
	raise (Invalid_argument "SSA: Block not allowed")
    | Vine.Attr(s,a) ->
	if true
	then (* copy attributes to all statements generated from this one *)
	  let myrevstmts = stmt2ssa ctx ~revstmts:[] s in
	  let mystmts = List.rev_map (fun s -> Attr(s,a)) myrevstmts in
	    List.rev_append mystmts revstmts
	else (* Only put the attribute on the last statement generated. *)
	  (match stmt2ssa ctx ~revstmts:revstmts s with
	       [] -> []
	     | s::ss -> Attr(s,a)::ss
	  )
    | Vine.Assert(e) ->
	let (revstmts,v) = exp2ssa ctx revstmts e in
	  Assert(v)::revstmts
    | Vine.Halt(e) ->
	let (revstmts, v) = exp2ssa ctx revstmts e in 
	  Halt(v)::revstmts
	

(** Translates a list of Vine statements that get executed sequentially to SSA.
*)
let stmts2ssa ctx ss =
  let revstmts = List.fold_left (fun rs s -> stmt2ssa ctx rs s) [] ss in
    List.rev revstmts


let ssa_iter_labels f = function
    Label l -> f l
  | _ -> ()

let ssa_list_iter_labels f sl =
  List.iter (ssa_iter_labels f) sl


module G = Vine_cfg.MakeG (struct type t = stmt list end);;
module Toposort = Graph.Topological.Make(G);;
module Dom = Dominator.Make(G)

module StmtG = Vine_cfg.MakeG(struct type t = stmt end)


let defsites cfg =
  let h = VH.create 57 in
  let defs stmts =
    let res = ref [] in
    let vis = object
      inherit nop_vine_visitor
      method visit_stmt = function
	| V.Move((V.Temp v | V.Mem(v,_,_)), _) 
	| V.Call(Some(V.Temp v | V.Mem(v,_,_)), _, _) ->
	    res := v :: !res;
	    SkipChildren
	| V.Attr _ | V.Block _ -> DoChildren
	| _ -> SkipChildren
    end
    in
      ignore(Vine.stmts_accept vis stmts);
      !res
  in
  let () =
      cfg#iter_bb
	(fun b ->
	   let id = cfg#get_id b in
	   let vars = list_unique  (defs (cfg#get_info b)) in
	     List.iter (fun v -> VH.add h v id) vars
	)
    in
      (* add globals as being defined at the entry node. *)
    let () = List.iter (fun v -> VH.add h v BB_Entry) cfg#vardecls in
      VH.find_all h
	(* FIXME: maybe avoiding find_all will make it faster *)


(** Translates a CFG into SSA form.
    Returns the new SSA CFG and two maps. One from SSA variables to the
    variables they originally came from, and the other from the original
    variables to what they map to at the end of the exit node. Both raise
    Not_found for variables that don't map to anything. (Eg, for temporary
    variables introduced by SSA, or variables that weren't assigned.)
 *)
let rec trans_cfg cfg =
  pdebug "Translating to SSA";
  if debug && not(Vine_cfg.well_defined cfg) then
    raise(TypeError "Ssa.trans_cfg: given cfg not well defined");
  let () = pdebug "Creating new cfg" in
  let ssa =
    Vine_cfg.map
      (fun n -> [] (*stmts2ssa (empty_ctx()) (bb_stmts cfg n)*))
      cfg
      ssa_list_iter_labels
  in
  let () = pdebug "Computing defsites" in
  let defsites = defsites cfg in
    (* keep track of where we need to insert phis *)
  let phis : (bbid * var, var * var list) Hashtbl.t = Hashtbl.create 57 in
  let () = pdebug "Computing dominators" in
  let {Dom.dom_tree=dom_tree; Dom.dom_frontier=df} =
    Dom.compute_all ssa BB_Entry
  in
  let add_phis_for_var v =
    (* Note that contrary to the book, we don't need membership testing at all
       for the worklist, since the only time we try to add a node is when it
       couldn't be in the worklist in the first place. --aij
       (* TODO: Double check this and send errata. *)
    *)
    (* let () = dprintf "Adding phis for variable '%s'" (var_to_string v) in *)
    let rec do_work = function
	[] -> ()
      | n::worklist ->
	  let worklist =
	    List.fold_left
	      (fun toadd y ->
		 if not(Hashtbl.mem phis (y,v))
		 then (Hashtbl.add phis (y,v) (v,[]);
		     if List.mem y (defsites v) then toadd else y::toadd )
		 else toadd
	      )
	      worklist
	      (df n)
	  in
	    do_work worklist
    in
      do_work (defsites v)
  in
  let () = dprintf "Adding phis" in
  let () = List.iter add_phis_for_var ssa#vardecls in
  let () = dprintf "Added %d phis" (Hashtbl.length phis) in
    (* we now have an entry in phis for every phi expression
       we need to add, although we still don't have the RHS and LHS. *)
  let () = dprintf "Grouping phis by block" in
  let blockphis =
    (* returns the phis for a given block *)
    let h = Hashtbl.create 57 in
    let () = Hashtbl.iter (fun (n,v) _ -> Hashtbl.add h n v) phis in
      Hashtbl.find_all h
  in
  let exitctx = VH.create 57 in (* context at end of exit node *)
  let (vh_ctx,to_oldvar,stacks) as ctx = new_ctx() in
  let lookup = ctx_lookup ctx in
  let extend = ctx_extend ctx in
  let rec rename_block bbid =
    dprintf "Translating block %s" (bbid_to_string bbid);
    let b = ssa#find bbid
    and cfgb = cfg#find bbid in
    let () = ctx_push ctx in
    let () =
      (* create variables for our phis *)
      List.iter
	(fun v ->
	   let v' = newssavar v in
	   let (v'',vs) = Hashtbl.find phis (bbid,v) in
	     assert(v'' == v);
	     Hashtbl.replace phis (bbid,v) (v',vs);
	     extend v v'
	)
	(blockphis bbid)
    in
    let () = 
      (* rename variables *)
      let stmts = cfg#get_info cfgb in
      let () = dprintf "translating stmts" in
      let stmts' = stmts2ssa ctx stmts in
	dprintf "ssa#set_info";
	ssa#set_info b stmts'
    in
    let () = dprintf "going on to children" in
      (* rename children *)
    let () = List.iter rename_block (dom_tree bbid) in
    let () =
      (* Update any phis in our successors *)
      List.iter
	(fun s ->
	   List.iter
	     (fun v ->
		try 
		  let (p,vs) = Hashtbl.find phis (s,v) in
		  let v' = try lookup v with Not_found -> v  in
		    Hashtbl.replace phis (s,v) (p, v'::vs)
		with Not_found ->
		  failwith("phi for variable "^Vine.var_to_string v
			   ^" not found in "^bbid_to_string s)
	     )
	     (blockphis s)
	)
	(G.succ ssa bbid)
    in
    let () = (* save context for exit node *)
      if bbid = BB_Exit then
	VH.iter (fun k v -> VH.replace exitctx k v) vh_ctx
    in
      (* restore context *)
      ctx_pop ctx
  in
  let () = rename_block BB_Entry in
  let () = dprintf "Adding %d phis to the CFG" (Hashtbl.length phis) in
  let rec split_labels revlabels stmts =
    match stmts with
      | ((Label _ | Comment _) as s)::ss ->
	  split_labels (s::revlabels) ss
      | _ -> (revlabels, stmts)
  in
  let () =
    (* actually add all our phis to the CFG *)
    ssa#iter_bb
      (fun b ->
	 let bbid = ssa#get_id b in
	 let vars = blockphis bbid in
	 let (revlabs,stmts) = split_labels [] (ssa#get_info b) in
	 let stmts =
	   List.fold_left
	     (fun s v ->
		let (p,vs) = Hashtbl.find phis (bbid,v) in
		  Move(p,Phi(vs))::s )
	     stmts
	     vars
	 in
	   ssa#set_info b (List.rev_append revlabs stmts)
      )
  in
  let () = dprintf "Done translating to SSA" in
    (ssa, VH.find to_oldvar, VH.find exitctx)

(** Translates a CFG into SSA form.
 *)
let cfg2ssa cfg =
  let (ssa,_,_) = trans_cfg cfg in
    ssa

let rm_phis (cfg: stmt list #cfg) =
  (* maps variables to the BBs where they were defined *)
  let assn = Hashtbl.create 1000 in
  let () =
    cfg#iter_bb
      (fun b ->
	 List.iter
	   (function
		Move(v,_) | Call(Some v,_,_) -> Hashtbl.add assn v b
	      | _->())
	   (cfg#get_info b)
      )
  in
  let entry = cfg#find BB_Entry in
    (* fake assignments for globals at the entry node *)
  let () = List.iter (fun v -> Hashtbl.add assn v entry) cfg#vardecls in
  let phis =
    (* Remove all the phis from all the BBs *)
    (* FIXME: make this readable *)
    cfg#fold_bb
      (fun b phis ->
	 let (ps,rs) =
	   List.fold_left
	     (fun (ps,rs) -> function
		| Move(l, Phi _) as s ->
		    dprintf "rm_phis: removing phi for %s"(Vine.var_to_string l);
		    (s::ps, rs)
		| s ->
		    (ps, s::rs)
	     )
	     (phis,[])
	     (cfg#get_info b)
	 in
	   (* Note that the statements in the block are now reversed *)
	   cfg#set_info b rs;
	   ps
      )
      []
  in
  let append_move b l p=
    (* note that since stmts are reversed, we can prepend
       instead of appending. We must still be careful to not put
       assignmenst after a jump. *)
    let move = Move(l,Val(Lval p)) in
      cfg#set_info b
	(match cfg#get_info b with
	     (Jmp _ as j)::stmts
	   | (CJmp _ as j)::stmts ->
	       j::move::stmts
	   | stmts ->
	       move::stmts )
  in
  let () =
    (* assingn the variables the phi assigns at the end of each block a variable
       the phi references is assigned. *)
    List.iter
      (function (Move(l, Phi vars)) -> 
	 dprintf "rm_phis: adding assignments for %s" (Vine.var_to_string l);
	 List.iter (fun p -> append_move (Hashtbl.find assn p) l p) vars
       | _ -> failwith "non phi found in list of phis"
      )
      phis
  in
    (* put statements back in forward order *)
    cfg#iter_bb (fun b -> cfg#set_info b (List.rev(cfg#get_info b)))
    

let var2vine x = x

let rec lval2vine x =
  Vine.Temp x

and value2vine = function
    Int(i,t) -> Vine.Constant(Vine.Int(t, i))
  | Str s -> Vine.Constant(Vine.Str s)
  | Lval l -> Vine.Lval(lval2vine l)
  | Name l -> Vine.Name l

let exp2vine = function
    BinOp(bo,v1,v2) -> Vine.BinOp(bo, value2vine v1, value2vine v2)
  | UnOp(uo, v) -> Vine.UnOp(uo, value2vine v)
  | Val v -> value2vine v
  | Cast(ct,t,v) -> Vine.Cast(ct, t, value2vine v)
  | Unknown s -> Vine.Unknown s
  | Get(Lval n,i, t) ->
      Vine.Lval(Vine.Mem(n, value2vine i,t))
  | Get _ -> ssa_err "malformed Get"
  | Set(Lval m, i, v, t) ->
      Vine.Let(Vine.Mem(m, value2vine i,t),  value2vine v,
               Vine.Lval(Vine.Temp m) )
  | Set _ -> ssa_err "malformed Set"
  | Phi _ -> ssa_err "exp2vine cannot translate Phi expressions"

(** Translates an SSA stmt back to Vine *)
let rec stmt2vine = function
    Jmp t -> Vine.Jmp(value2vine t)
  | CJmp(c,tt,tf) -> Vine.CJmp(value2vine c, value2vine tt, value2vine tf)
      
  | Move(l,e) -> Vine.Move(lval2vine l, exp2vine e)
  | Label l -> Vine.Label l
  | Comment s -> Vine.Comment s
  | Return r -> Vine.Return (option_map value2vine r)
  | Call(r,callee,args) ->
      Vine.Call(option_map lval2vine r, value2vine callee, 
		List.map value2vine args)
  | Assert(t) ->
      Vine.Assert(value2vine t)
  | Halt(t) ->
      Vine.Halt(value2vine t)
  | Attr(s,a) ->
      Vine.Attr(stmt2vine s, a)

(** Convert an ssa cfg back to a vine cfg *)
let cfg2vine (cfg:stmt list #Vine_cfg.cfg) =
  Vine_cfg.map (fun n -> List.map stmt2vine (bb_stmts cfg n)) cfg
    Vine_cfg.vine_list_iter_labels

let rec to_vine (cfg:stmt list #Vine_cfg.cfg) = 
  let vis = object(self)
    inherit nop_vine_visitor

    val ctx = Hashtbl.create 97883

    method get_ctx () = ctx

    method private extend_ctx lv = 
      match lv with
	  Temp(v) -> Hashtbl.replace ctx v None;
	| Mem(v,_,_) -> Hashtbl.replace ctx v None;

    method visit_alvalue lv = self#extend_ctx lv;
      DoChildren

    method visit_rlvalue lv = self#extend_ctx lv;
      DoChildren

    method visit_stmt s =
      match s with
	  Vine.Move(lv,_) -> self#extend_ctx lv; DoChildren
	| _ -> DoChildren

  end
  in
  let () = rm_phis cfg in 
  let (newcfg: Vine.stmt list #Vine_cfg.cfg) = cfg2vine cfg in 
  let () = newcfg#iter_bb
    (fun n -> 
       let (stmts:Vine.stmt list) = newcfg#get_info n in
	 List.iter (fun s -> ignore(Vine.stmt_accept vis s)) stmts)
  in
  let vars = Hashtbl.fold (fun k v a ->
			     k::a) (vis#get_ctx ()) []
  in
    newcfg#set_vardecls vars;
    newcfg



let ssalist_to_ssa (oldcfg: stmt list Vine_cfg.cfg) : (stmt list Vine_cfg.cfg) = 
  Vine_cfg.split_list_cfg oldcfg (Comment "Empty bb") ssa_list_iter_labels

let simplify_exp e : exp = 
  match e with
      BinOp(bop, Int(i1,t1), Int(i2,t2)) -> (
	let (c:Vine.exp) = 
	  Vine_opt.constant_fold (fun _ -> None)
	    (Vine.BinOp(bop,Vine.Constant(Vine.Int(t1,i1)),
			Vine.Constant(Vine.Int(t2,i2))))
	in
	    match c with
		Vine.Constant(Vine.Int(t,i)) -> 
		  Val(Int(i,t))
	      | _ -> e
      )
    | UnOp(uop, Int(i1,t1)) -> (
	let c = 
	  Vine_opt.constant_fold (fun _ -> None)
	    (Vine.UnOp(uop,Vine.Constant(Vine.Int(t1,i1))))
	in
	    match c with
		Vine.Constant(Vine.Int(t,i)) -> 
		  Val(Int(i,t))
	      | _ -> e
      )
	(* FIXME: we could do more with let *)
    | _ -> e

let simplify_stmt s = 
  let vis = object(self)
    inherit nop_ssa_visitor

    method visit_exp e = ChangeDoChildrenPost(simplify_exp e, id)
  end
  in
    stmt_accept vis s

module NP = Vine_cfg.NodePartition.Make(G);;

let remove_unreachable ?(from=BB_Entry) g = 
  NP.iter_unreachable (fun x -> G.remove_vertex g x) g from
  
(* There is a copy of this in vine_cfg.ml. Fix any bugs there too *)
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



let uninitialized_vars cfg =
  let refd = VH.create 100 
  and assnd = VH.create 100 in
  let vis = object
    inherit nop_ssa_visitor
    method visit_avar x = VH.replace assnd x (); DoChildren
    method visit_rvar x = VH.replace refd x (); DoChildren
  end
  in
    cfg#iter_bb (fun b -> ignore(stmts_accept vis (cfg#get_info b)));
    VH.iter (fun k _ -> VH.remove refd k) assnd;
    VH.fold (fun k _ a -> k::a) refd []


(** Translate a trace into a trace where each variable is assigned only once.

    This will leave all let variables alone, and leave uninitialized references
    as refering to the original variables. Memory writes will also not create
    new variables.
*)
let trace_ssa (dl,sl) =
  let ctx = VH.create 570
  and vars = ref dl in
  let extend x =
    let x' = renewvar x in
      VH.replace ctx x x';
      vars := x':: !vars;
      x'
  in
  let vis = object(self)
    inherit nop_vine_visitor
    method visit_stmt = function
      | (V.Move(V.Temp v, _) as stmt) ->
	  let f = function
	    | V.Move(_,e) ->  V.Move(Temp(extend v), e)
	    | _ ->            failwith "impossible a02093h0uh"
	  in
	    ChangeDoChildrenPost(stmt, f)
      | (V.Call(Some(V.Temp v), _,_) as stmt) ->
	  let f = function
	    | V.Call(_,f,args) ->  V.Call(Some(Temp(extend v)), f, args)
	    | _ ->                 failwith "impossible 9o8e9oonet"
	  in
	    ChangeDoChildrenPost(stmt, f)
      | V.Jmp _ | V.CJmp _ ->
	  failwith "trace_ssa: No jumps allowed in trace"
      | _ ->
	  DoChildren
	    
    method visit_rlvalue = function
      | V.Temp v -> (
	  try ChangeTo(V.Temp(VH.find ctx v))
	  with Not_found -> DoChildren )
      | V.Mem(v,i,t) ->
	  try ChangeDoChildrenPost(V.Mem(VH.find ctx v, i, t), id)
	  with Not_found -> DoChildren
    method visit_alvalue = function
      | V.Mem _ as m -> self#visit_rlvalue m
      | V.Temp _ -> DoChildren
  end in
  let sl = V.stmts_accept vis sl in
    (!vars, sl)
