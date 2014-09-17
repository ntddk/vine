(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Convert memory style accesses to array accesses.
    
    This module is for converting broken IR code, which accesses an array as if
    it were memory, into code that accesses the array as an array.

    Why arrays and memory are different:
    Memory is byte addressed.
    One write to memory can overwrite multiple cells.
    When this happens, what data gets stored in what cell is implementation
    dependant. (big endian vs. little endian)

*)

module H = Hashtbl;;

(* type endian = BigEndian | LittleEndian *)

open Vine
open Vine_util
open ExtList

module D = Debug.Make(struct let name="Vine_memory2array" and default=`NoDebug end)
open D


(* module LooseVar =
struct
  type t = var
  let equal (num,_,_) (num',_,_) = num = num'
  let hash (num,_,_) = num
end
module LVH = Hashtbl.Make(LooseVar)
*)

(** [memory2array_visitor mem  arr] creates a visitor that converts array
    access to the variable [mem] (not an array) into accesses into the array
    [arr].
*)
class memory2array_visitor
  mem arr
=
  let addr_typ,endian = match mem with
      (_,_,TMem(atyp, endian)) -> atyp,endian
    | _ -> failwith "Invalid mem argument"
  in
  let (idxt,elemt,arrt) = match arr with
      ((_,_,(Array(elemt,idxt) as arrt))) -> 
	let addrsize = array_idx_type_to_size addr_typ in 
	  if addrsize <= idxt then 
	    (idxt,elemt,arrt)
	  else
	    raise (VineError ("array not large enough to hold memory"))
    | _ -> raise (Invalid_argument ("memory2array_visitor:"^
		    " given array is not an array"))
  in


  let elemb = bits_of_width elemt in
  let idx2shift targ_width idx =
    match endian with
	Big -> targ_width - (idx+1)*elemb
      | Little -> idx*elemb
  in
  let eq_mem = Vine.Var.equal mem in (* LooseVar.equal mem  in *)
(*  let check_mem ((x,y,z) as m) =
    match (Vine.unwind_type z) with
	Array(ti,t) -> (
(*	((_,_,Array(ti,t)) as m) = *)
	  if not(eq_mem m)  || t = elemt then false
	  else if ti <> idxt
	  then raise(TypeError("mem2array: index type in mem does not match"
			       ^"index type of desired array."))
	  else if bits_of_width t < elemb
	  then failwith "memory2array doesn't support converting to larger words yet"
	  else true
	)
      | _ -> raise (Invalid_argument "Check mem expects mem")
  in *)
  let low_mask = Int64.pred(Int64.shift_left 1L elemb) in
  let split_writes idx ti vl t =
    let tb = bits_of_width t in
    let get_nth_write  n = 
      let targ = Mem(arr, exp_plus (Lval idx) (const_of_int ti n), elemt) in
      let shift = idx2shift tb n in
      let mask = const_of_int64 t (Int64.shift_left low_mask shift)  in
      let data = Cast(CAST_LOW, elemt,
		      BinOp(RSHIFT, exp_and (Lval vl) mask,
			    const_of_int t shift))
      in
	(targ, data)
    in
      mapn get_nth_write (tb/elemb-1)
  in
object (self)
    inherit nop_vine_visitor

    method visit_alvalue lv = 
      match lv with
	  Temp(t) when eq_mem t ->
	    ChangeTo(Temp(arr))
	| _ -> DoChildren
	    
    method visit_rlvalue lv = 
      match lv with
	  Temp(t) when eq_mem t ->
	    ChangeTo(Temp(arr))
	| _ -> DoChildren

    method visit_exp e =
      match e with
	  Lval(Mem(m,i,t)) when eq_mem m && t = elemt ->
	    (* if mem write is same size as element, no need to cast
	       into several array read/writes *)
	    ChangeDoChildrenPost(Lval(Mem(arr,i,t)), id)

	| Lval(Mem(m, i, t)) when eq_mem m (* when check_mem m *) ->
	    let ti = addr_typ in 
	    let tb = bits_of_width t in
	    let (idx,letfun) = match i with
		Lval(Temp _) | Constant _ -> (i,id)
	      | _  -> let idx = Temp(newvar "idx" ti) in
		  (Lval(idx), fun x -> Let(idx, i, x))
	    in
	    let read_nth_byte n =
	      let byte = Mem(arr, exp_plus idx (const_of_int ti n), elemt) in
	      let casted = Cast(CAST_UNSIGNED, t, Lval byte) in
		BinOp(LSHIFT, casted, const_of_int t (idx2shift tb n))
	    in
	    let words = mapn read_nth_byte (tb/elemb-1) in
	    let newexp =
	      letfun(List.fold_left exp_or (List.hd words) 
		       (List.tl words))
	    in
	      ChangeDoChildrenPost(newexp, id)
	| Let(Mem(m,i,t),v,e) when eq_mem m && t = elemt ->
	    (* mem read/write is same size as array element type *)
	    ChangeDoChildrenPost(Let(Mem(arr,i,t),v,e), id)
	| Let(Mem(m, i, t), v, e) when eq_mem m  ->
	    let ti = addr_typ in (* type of index is memory address typ *)
	    let (idx,vl) = (Temp(newvar "idx" ti), Temp(newvar "val" t)) in
	    let writes = split_writes idx ti vl t in
	    let i = exp_accept self i in
	    let v = exp_accept self v in
	    let e = exp_accept self e in
	    let exp =
	      Let(idx, i,
		  Let(vl, v,
		      List.fold_left (fun e' (x,y) -> Let(x,y,e')) e writes))
	    in
	      ChangeTo exp
	| _ -> DoChildren

    method visit_stmt s =
      match s with
	  (Move(Mem(m, i, t), _)
	  | Call(Some(Mem(m,i,t)), _,_) ) when eq_mem  m  ->
	    let ti = addr_typ in 
	    let (idx_name,vl_name) = (newvar "idx" ti, newvar "val" t) in
	    let (idx,vl) = (Temp idx_name, Temp vl_name) in
	    let writes = split_writes idx ti vl t in
	    let i = exp_accept self i in
	    let s =
	      match s with
		| Move(_,e) -> Move(vl,e)
		| Call(_,f,args) -> Call(Some vl, f, args)
		| _ -> failwith "no wei"
	    in
	      ChangeDoChildrenPost
		(s,
		 fun stmt ->
		   Block([idx_name; vl_name],
			 Move(idx, i):: stmt ::
			   List.map(fun(x,y)->Move(x,y)) writes
		   ) )
	| _ -> DoChildren
end

let coerce_stmt mem arr s = 
  let vis = new memory2array_visitor mem arr in 
    stmt_accept vis s 




(* we shouldn't have broken memories anymore *)

(*
let rec lval_broken_mems gamma lv =
(** returns a list of mem variables that need rewriting in @param lv *) 
  match lv with
      Temp _ -> []
    | Mem(v, e) -> 
	let r1 = exp_broken_mems gamma e in 
	let v' = LVH.find gamma v in 
	  if v != v' then v':: r1 else r1
and exp_broken_mems gamma e =
  (** returns a list of mem variables that need rewriting in @param e *)
  match e with
    BinOp(bop, e1, e2) -> 
      let r1 = exp_broken_mems gamma e1 in 
      let r2 = exp_broken_mems gamma e2 in 
	r1 @ r2
    | UnOp(uop, e) -> exp_broken_mems gamma e
    | Constant(t,v) -> []
    | Lval(lv) -> lval_broken_mems gamma lv 
    | Name _ -> []
    | Cast(ct, t, e) -> exp_broken_mems gamma e
    | Unknown _ -> []
    | Let(lv, e1, e2) -> 
	let r1 = lval_broken_mems gamma lv  in 
	let r2 = exp_broken_mems gamma e1 in 
	let r3 = exp_broken_mems gamma e2 in 
	  r1 @ r2 @ r3
;;




(** returns a new statement in which broken mem's (i.e., mem's whose
  type annotations do not match their declared type) are rewritten.
 *)
let rec coerce_stmt  gamma s = 
  let rec mk_new_stmt s (cl:Vine.var list) =
    (* helper to return the new statement given a list @param cl of
       broken memorys in the statement *)
    match cl with
	[] -> s
      | v::rest ->
	  let mem = LVH.find gamma v in
	  let vis = new memory2array_visitor v Little mem in
	    mk_new_stmt (Vine.stmt_accept vis s) rest
  in 
  match s with 
      Jmp(e) -> mk_new_stmt s (exp_broken_mems gamma e)
    | CJmp(e,e1,e2) ->
	let  r1  = exp_broken_mems gamma e in 
	let r2 = exp_broken_mems gamma e1 in 
	let r3 = exp_broken_mems gamma e2 in 
	  mk_new_stmt s (r1 @r2@r3)
    | Move(lv, e) -> 
	let r1 = lval_broken_mems gamma lv in 
	let r2 = exp_broken_mems gamma e in 
	  mk_new_stmt s (r1 @ r2)
    | Special _ -> s
    | Label _ -> s
    | ExpStmt(e) -> mk_new_stmt s (exp_broken_mems gamma e)
    | Comment _ -> s
    | Block(dl,sl) ->
	List.iter (fun v -> LVH.add gamma v v) dl;
	let newsl = List.map (coerce_stmt  gamma) sl in
	  List.iter (LVH.remove gamma) dl;
	  Block(dl, newsl)
    | Function(v,t,dl,b,Some(s')) ->
	List.iter (fun v' -> LVH.add gamma v' v') dl;
	let s'' = coerce_stmt gamma s' in 
	  List.iter (LVH.remove gamma) dl;
	  Function(v,t,dl,b,Some(s''))
    | Function(v,t,dl,b,None) -> s
    | Return(None) -> s
    | Return(Some(e)) -> mk_new_stmt s (exp_broken_mems gamma e)
    | Call(Some(lv), v, el) ->
	let r1 = lval_broken_mems gamma lv in 
	let r2 = List.fold_left (fun acc e -> 
				   let e' = exp_broken_mems gamma e in 
				     e' @ acc) [] el in 
	  mk_new_stmt s (r1 @ r2 )
    | Call(None, v, el) ->
	let r1 = List.fold_left (fun acc e ->
				   let e' = exp_broken_mems gamma e in
				     e' @acc) [] el in 
	  mk_new_stmt s r1
    | Attr(s', a) -> let s'' = coerce_stmt gamma s' in 
	Attr(s'', a)
    | Assert(e) -> mk_new_stmt s (exp_broken_mems gamma e)

let coerce_prog (dl,sl) = 
  let gamma = LVH.create 57 in 
    List.iter (fun v -> LVH.add gamma v v) dl;  
    let newsl = List.map (coerce_stmt  gamma) sl in    
      (dl, newsl)

*)


let coerce_program_variable mem arr (dl,sl) = 
  (* assumes arr is part of dl already *)
  (dl, List.map (coerce_stmt mem arr) sl)


(*   let mem = List.find (fun (_,_,t) -> *)
(* 			 match t with *)
(* 			     Vine.TMem _ -> true *)
(* 			   | _ -> false) dl in *)
(*   let memtyp = (match mem with *)
(* 		  | (_,_,Vine.TMem(t, _)) -> t *)
(* 		  | _ -> failwith "Bad mem type") in  *)
(*   let arr = Vine.newvar "memarr"  *)
(*     (Vine.Array(Vine.REG_8,  *)
(* 		(Vine.array_idx_type_to_size memtyp))) in  *)
(*     coerce_program mem arr (arr::dl,sl) *)


(** deend your average program. Returns new program where all memory
    broken down to byte-level reads and writes using array variables
    with the same name as the old memory variables.  *)
let coerce_prog (dl,sl) = 
  let mems,nonmems = List.partition
    (function (_,_,Vine.TMem _) -> true | _ -> false) dl
  in 
  let rec recurse_mem lst (dl,sl) = 
      match lst with
	  ((_,name,Vine.TMem(r,_)) as mem)::ys -> 
	    let arr =
	      newvar name (Vine.Array(Vine.REG_8, Vine.array_idx_type_to_size r))
	    in 
	      D.dprintf "Deendizing memory for %s |-> %s" (var_to_string mem)
		     (var_to_string arr);
	      recurse_mem ys 
		(coerce_program_variable mem arr (arr::dl, sl))
	| x::ys -> failwith "Shouldn't get here"
	| [] -> (dl,sl)
  in
  let (dl,sl) = recurse_mem mems (nonmems,sl) in
  let othermems =
    let vh = VarHash.create 570 in
    let vis = object
      inherit nop_vine_visitor
      method visit_alvalue (Temp v | Mem(v,_,_)) =
	match v with
	  | (_,_,TMem _) ->
	      VarHash.replace vh v ();
	      DoChildren
	  | _ -> DoChildren
    end in
    let _ = prog_accept vis (dl,sl) in
      VarHash.fold (fun k () a -> k::a) vh []
  in
  let (_,sl) = recurse_mem othermems (dl,sl) in
    (dl,sl)

(** [memory2array_multi_visitor mem_p sizes] is a visitor that
    converts one or all memory variables in a program into array
    variables. The predicate [mem_p] is applied to a Vine.Var representing
    a meory to decide whether or not it should be converted; most often,
    it should just be the constant function true. For each memory, one or
    more replacement arrays will be created: an array will always be
    created for byte-sized loads, and optionally arrays will also be
    created for 2-byte, 4-byte, or 8-byte loads, as indicated by the 2, 4,
    and 8 bits of the integer [sizes].
*)
(* These hashes map from memory variables to the corresponding array
   variables of various sizes. Making them global is unfortunate for
   memory usage and modularity purposes, but means that you can
   translate different chunks of code separately and have them work
   together correctly, which appreplay does. *)
let b_arrs = VarHash.create 51
and w_arrs = VarHash.create 51
and l_arrs = VarHash.create 51
and q_arrs = VarHash.create 51

class memory2array_multi_visitor
  mem_p sizes
  =
  let type2size = function
    | REG_8  -> 1
    | REG_16 -> 2
    | REG_32 -> 4
    | REG_64 -> 8
    | _ -> failwith "Unsupported access size"
  in
  let size2type = function
    | 1 -> REG_8
    | 2 -> REG_16
    | 4 -> REG_32
    | 8 -> REG_64
    | _ -> failwith "Can't happen: unsupported access size"
  in
  let maybe_temp expr name ty rewrite = match expr with
    | Lval(Temp _)
    | Constant _ -> (expr, id)
    | _  -> let tmp = Temp(newvar name ty) in
	(Lval(tmp), fun x -> Let(tmp, (rewrite expr), x))
  in
  let maybe_temp_rewrite_block expr name ty rewrite = match expr with
    | Lval(Temp _)
    | Constant _ -> (expr, id)
    | _  -> let var = newvar name ty in
	(Lval(Temp var),
	 (function
	    | (Block(dl,sl)) ->
		Block(var :: dl, Move((Temp var), (rewrite expr)) :: sl)
	    | _ -> failwith "Not a block in maybe_temp_rewrite_block"))
  in
  let unblock = function
    | Block([], stmt :: []) -> stmt
    | _ as s -> s
  in
  let cfold = Vine_opt.constant_fold (fun x -> None) in 
  let maybe_plus expr ty offset = match offset with
    | 0 -> expr
    | _ -> cfold (exp_plus expr (const_of_int ty offset))
  in
  let maybe_shift direction expr offset = match offset with
    | 0 -> expr
    | _ -> cfold (BinOp(direction, expr, (const_of_int REG_8 offset)))
  in
  let maybe_lshift = maybe_shift LSHIFT and
      maybe_rshift = maybe_shift RSHIFT in
  let maybe_and e1 e2 = cfold (exp_and e1 e2) in
  let maybe_cast kind expr from_ty to_ty =
    if (from_ty == to_ty) then expr else
      cfold (Cast(kind, to_ty, expr)) in
  let maybe_cast_up = maybe_cast CAST_UNSIGNED and
      maybe_cast_down = maybe_cast CAST_LOW in
  let idx2shift targ_width endian elemb idx =
    match endian with
	Big -> targ_width - (idx+1)*elemb
      | Little -> idx*elemb
  in
  let fold_or exprs =
    Vine_opt.constant_fold_more (fun x -> None)
      (List.fold_left exp_or (List.hd exprs) (List.tl exprs))
  in
  let combine_bytes fn access_sz access_ty endian = 
    let read_nth_byte n =
      let casted = maybe_cast_up (fn n) REG_8 access_ty in
	maybe_lshift casted (idx2shift (8 * access_sz) endian 8 n)
    in
      fold_or (mapn read_nth_byte (access_sz - 1))
  in
  let assemble_bytes arr_b idx access_sz access_ty addr_ty endian = 
    combine_bytes
      (fun n -> Lval(Mem(arr_b, (maybe_plus idx addr_ty n), REG_8)))
      access_sz access_ty endian
  in
object (self)
  inherit nop_vine_visitor

  method private get_array mem size =
    let (hash, suffix, ty) = match size with
      | 1 -> (b_arrs, "_b", REG_8)
      | 2 -> (w_arrs, "_w", REG_16)
      | 4 -> (l_arrs, "_l", REG_32)
      | 8 -> (q_arrs, "_q", REG_64)
      | _ -> failwith "Can't happen: bad size in get_array"
    in
      try
	VarHash.find hash mem
      with Not_found -> match mem with
	| (i, name, TMem(addr_ty, endian)) ->
	    let arr = (newvar (name ^ suffix)
			 (Array(ty, (array_idx_type_to_size addr_ty)))) in
	      VarHash.add hash mem arr;
	      arr
	| _ -> failwith "Can't happen: non-memory in get_array"
	    
  method filter_decls = function
    | [] -> []
    | (_,_,TMem(_,_)) as mem :: rest when mem_p mem ->
	[(self#get_array mem 1)] @
	  (if sizes land 2 != 0 then [(self#get_array mem 2)] else []) @
	  (if sizes land 4 != 0 then [(self#get_array mem 4)] else []) @
	  (if sizes land 8 != 0 then [(self#get_array mem 8)] else []) @
	  (self#filter_decls rest)
    | var :: rest -> var :: self#filter_decls rest

  method private split_writes idx mem addr_ty endian value access_ty =
    let width = bits_of_width access_ty in
    let access_sz = width / 8 in
    let arr_b = self#get_array mem 1 in
    let get_nth_byte n = 
      let shift = idx2shift width endian 8 n in
      let mask = const_of_int64 access_ty (Int64.shift_left 255L shift) in
	maybe_cast_down (maybe_rshift (maybe_and value mask) shift)
	  access_ty REG_8
    in
    let new_bytes = mapn get_nth_byte (access_sz - 1) in
    let byte_targs = mapn (fun n ->
			     Mem(arr_b, maybe_plus idx addr_ty n, REG_8))
      (access_sz - 1) in
    let byte_writes = List.combine byte_targs new_bytes in
    let natural_writes = 
      (if sizes land access_sz != 0 then
	 [Mem((self#get_array mem access_sz), idx, access_ty), value]
       else []) in
    let byte_at_offset n =
      Lval(Mem(arr_b, maybe_plus idx addr_ty n, REG_8)) in
    let before_bytes = List.map byte_at_offset
      [-7; -6; -5; -4; -3; -2; -1] in
    let after_bytes = List.map (fun n -> (byte_at_offset (access_sz + n)))
      [0; 1; 2; 3; 4; 5; 6] in
    let bytes = before_bytes @ new_bytes @ after_bytes in
    let assemble_bytes offset size = 
      combine_bytes
	(fun n -> List.nth bytes (offset + n + 7))
	size (size2type size) endian
    in
    let rebuild_write size offset =
      (Mem((self#get_array mem size),
	   (maybe_plus idx addr_ty offset), 
	   (size2type size)),
       assemble_bytes offset size)
    in
    let rebuild_writes = List.flatten
      (List.map
	 (fun size ->
	    List.flatten (
	      (mapn (fun i -> 
		       if (size == access_sz) && (i == size - 1) then
			 [] (* Don't replace the natural store *)
			 else
			   [rebuild_write size (i - size + 1)])
		   (access_sz + size - 2)))
	   )
	   (List.filter (fun s -> s land sizes != 0) [2; 4; 8]))
      in
	natural_writes @ byte_writes @ rebuild_writes

    method visit_alvalue lv = 
      match lv with
	  Temp((_, _, TMem(_, _)) as m) when mem_p m ->
	    failwith "Unsupported first-class memory in memory2array_multi"
	| _ -> DoChildren
	    
    method visit_rlvalue lv = 
      match lv with
	  Temp((_, _, TMem(_, _)) as m) when mem_p m ->
	    failwith "Unsupported first-class memory in memory2array_multi"
	| _ -> DoChildren

    method visit_exp e =
      match e with
	| Lval(Mem((_, _, TMem(addr_ty, endian)) as m, orig_idx, access_ty))
	    when mem_p m ->
	    let access_sz = type2size access_ty in
	      if (access_sz == 1) || ((access_sz land sizes) != 0) then
		(* load from a natural-sized array *)
		let arr = self#get_array m access_sz in
		  ChangeDoChildrenPost(Lval(Mem(arr, orig_idx, access_ty)), id)
	      else
		(* wide load with no corresponding array, use the byte array *)
		let arr_b = self#get_array m 1 in
		let (idx, letwrap) = maybe_temp orig_idx "idx" addr_ty id in
		let newexp = (assemble_bytes arr_b idx access_sz
				access_ty addr_ty endian) in
		  ChangeDoChildrenPost(letwrap(newexp), id)
	| Let(Mem((_, _, TMem(addr_ty, endian)) as m, orig_idx, access_ty),
	      rhs, body) when mem_p m  ->
	    let access_sz = type2size access_ty in
	    let (idx, letwrap1) =
	      maybe_temp orig_idx "idx" addr_ty (exp_accept self) in
	    let (value, letwrap2) =
	      if access_sz == 1 && sizes land (2+4+8) == 0 then
		((exp_accept self rhs), id)
	      else
		maybe_temp rhs "val" access_ty (exp_accept self) in
	    let writes =
	      self#split_writes idx m addr_ty endian value access_ty in
	    let body' = exp_accept self body in
	    let body'' = List.fold_left	      
	      (fun e' (x,y) -> Let(x,y,e')) body' (List.rev writes)
	    in
	      ChangeTo(letwrap1(letwrap2(body'')))
	| _ -> DoChildren

    method visit_stmt s =
      match s with
	| Move(Mem((_, _, TMem(addr_ty, endian)) as m, orig_idx, access_ty),
	       rhs) when mem_p m ->
	    let access_sz = type2size access_ty in
	    let (idx, wrap1) =
	      maybe_temp_rewrite_block orig_idx "idx" addr_ty
		(exp_accept self) in
	    let (value, wrap2) =
	      if access_sz == 1 && sizes land (2+4+8) == 0 then
		((exp_accept self rhs), id)
	      else
		maybe_temp_rewrite_block rhs "val" access_ty
		  (exp_accept self) in
	    let writes =
	      self#split_writes idx m addr_ty endian value access_ty in
	    let moves = List.map(fun (x, y) -> Move(x, y)) writes
	    in
	      ChangeTo(unblock(wrap1(wrap2(Block([], moves)))))
	| Call(Some(Mem(m,idx,ty)), fn, args) when mem_p m ->
	    let retvar = newvar "ret" ty in
	      ChangeDoChildrenPost
		(Block([retvar],
		       Call(Some(Temp(retvar)), fn, args) ::
			 Move(Mem(m, idx, ty), Lval (Temp(retvar))) :: []),
		 id)
	| Block(dl, sl) ->
	    ChangeDoChildrenPost(Block((self#filter_decls dl), sl), id)
	| _ -> DoChildren

    method rewrite_prog (dl, sl) =
      ((self#filter_decls dl), List.map (stmt_accept self) sl)
end

let coerce_prog_multi prog =
  let vis = new memory2array_multi_visitor (fun _ -> true) 4 in 
    vis#rewrite_prog prog

let coerce_prog_multi_varlist varlist prog =
  let hash = VarHash.create 11 in
    List.iter (fun v -> VarHash.replace hash v true) varlist;
    let vis = new memory2array_multi_visitor
      (fun v -> try VarHash.find hash v with Not_found -> false) 4 in 
      vis#rewrite_prog prog
