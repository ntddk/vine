(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(**
   Alias analysis.
*)

open Vine
open Ssa
module VH = Vine.VarHash

module D = Debug.Make(struct let name = "Vine_alias" and default=`Debug end)
open D

type is_aliased = Ssa.value -> Ssa.value -> bool option

let typeof_ssaval = function
  | Int(_,t) | Lval(_,_,t) -> t
  | Name _ -> Vine.addr_t
  | Str _ -> TString

let esp = [List.find (fun (_,n,_) -> n = "R_ESP") Asmir.x86_regs]

let vsa_alias ?(init=esp) cfg =
  let init = Vsa.AlmostVSA.DFP.init_vars init in
  let input,output = Vsa.AlmostVSA.DF.worklist_iterate ~init:init cfg in
  let ae = input Vine_cfg.BB_Exit in
    fun x y ->
      (* FIXME: check that x and y have the same types? *)
      let k = bits_of_width(typeof_ssaval x) in
	if k <>  bits_of_width(typeof_ssaval y) then
	  (pwarn "vsa_alias: tried to compare incompatible types";None)
	else
	  let r = Vsa.VS.eq k (Vsa.AbsEnv.ssaval2vs ae x) (Vsa.AbsEnv.ssaval2vs ae y) in
	    if r = Vsa.VS.yes then Some true
	    else if r = Vsa.VS.no then Some false
	    else None
	    


let rec is_array = function
  | Array _ -> true
  | TAttr(t,_) -> is_array t
  | _ -> false

let var_is_array (_,_,t) = is_array t


let make_store_ht cfg =
  let mems = VH.create 100 in
  let rec add_stores = function
    | Move(v, Set(m,i,d,t)) when var_is_array v ->
	VH.add mems v (m,i,d,t)
    | Attr(s,_) -> add_stores s
    | _ -> ()
  in
  let () = cfg#iter_bb (fun b -> List.iter add_stores (cfg#get_info b)) in
    mems


let alias_replace alias_func cfg =
  let mems = make_store_ht cfg in
  let rec replace_loads = function
    | (Move(v, Get(Lval m,i,t)) as stmt) when var_is_array m -> (
	try
	  let (sm,si,sd,st) = VH.find mems m in
	    match alias_func i si with
	      | Some true ->
		  Move(v, Val sd)
	      | Some false ->
		  replace_loads (Move(v, Get(sm,i,t)))
	      | _ ->
		     stmt
	with Not_found ->  stmt
      )
    | (Attr(s,a) as stmt) ->
	let r = replace_loads s in
	  if s == r then stmt else Attr(r,a)
    | x -> x
  in
  let changed = ref false in
  let replace_bb b =
    let stmts = cfg#get_info b in
    let newstmts = List.map replace_loads stmts in
      if List.exists2 (!=) stmts newstmts then (
	cfg#set_info b newstmts;
	changed := true
      )
  in
    cfg#iter_bb replace_bb;
    !changed



let deadwrite_replacement = Comment "removed dead write here"

let remove_dead_stores  alias_func ?(live_ends=fun _ -> None) cfg =
  let reads = VH.create 100 in
  let mmoves = VH.create 100 in
  let rec add_moves = function
    | Move(v, Get(Lval m,i,t)) when var_is_array m ->
	VH.add reads v (m,i,t)
    | Move(v, x) when var_is_array v ->
	VH.add mmoves v x
    | Attr(s,_) -> add_moves s
    | _ -> ()
  in
  let () = cfg#iter_bb (fun b -> List.iter add_moves (cfg#get_info b)) in
  let () = dprintf "found %d mmoves, %d reads" (VH.length mmoves) (VH.length reads) in
  let used = VH.create (VH.length mmoves)
  and unused = VH.create (VH.length mmoves) in
  let rec walkback f var =
    try match VH.find mmoves var with
      | Get _ ->
	  failwith "unsupported get of mem from mem"
      | Set(Lval m,i,_,_) -> (
	  match f i with
	    | Some true ->
		VH.replace used var ()
	    | Some false ->
		walkback f m
	    | None ->
		VH.replace used var ();
		walkback f m
	)		
      | Val(Lval m) ->
	  walkback f m
      | Phi vars ->
	  List.iter (walkback f) vars
      | _ ->
	  failwith "remove_dead_stores: unsupported memory operation"
    with Not_found -> ()
  in
  let () =
    (* mark used mem vars *)
    VH.iter (fun _ (m,i,_) -> walkback (alias_func i) m) reads;
    VH.iter
      (fun m _ -> match live_ends m with
	 | None -> ()
	 | Some f -> walkback f m )
      reads;
  (* map unused writes to the previous mem *)
    VH.iter
      (fun v x ->
	 if not(VH.mem used v) then match x with
	   | (Set(Lval m,_,_,_)) -> VH.add unused v m
	   | _ -> ()
      )
      mmoves
  in
  let () = dprintf "found %d used, %d unused" (VH.length used) (VH.length unused) in
  let changed = ref false in
  let rec get_replacement v =
    let r = VH.find unused v in
      try
	let r' = get_replacement r in
	  VH.replace unused v r'; (* shortcut for next time *)
	  r'
      with Not_found -> r
  in
  let vis = object
    inherit nop_ssa_visitor
    method visit_rvar var =
      try let rep = get_replacement var in
	changed := true;
	ChangeTo rep
      with Not_found -> SkipChildren
    method visit_stmt = function
      | Move(lv, Set _) ->
	  (try let _ = get_replacement lv in
	    ChangeTo deadwrite_replacement
	  with Not_found -> DoChildren )
      | _ -> DoChildren
	    
  end in
  if VH.length unused <> 0 then (cfg_accept vis cfg; !changed)
  else false

;;
