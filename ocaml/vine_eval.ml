(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(* (\** *)
(*   An evaluator for our language. *)
(*   Each instruction is executed within a register scope.  The scope is *)
(*   a list with the most current register context for the innermost *)
(*   block first.   We evaluate a list of statements.  The list of *)
(*   statements does not include blocks or attributes. So *)
(*    \{ i1; \{ i2; i3; \} \} becomes the statement list \{i1; i2; i3\}. We *)
(*     a) build three maps. pcmap maps the statement number (program *)
(*   counter) to a statement. the lblmap maps a label to a program *)
(*   counter.  The scopemap maps a program counter to the execution *)
(*   scope. *)
(*    b) evaluate a statement. This returns Continue or Goto(exp) *)
(*    c) we look up the next pc. If the returned value is Continue, this *)
(*   is just the current pc+1.  If we return Goto(exp), we lookup in the *)
(*   label map the value of exp, then reset the pc, then lookup the *)
(*   correspnoding scope, and continue executing. *)
(* *\) *)

open Vine
open Vine_opt
module List = ExtList.List;;
module H = Hashtbl;;


(*module D = Debug.Make(struct let name = "vine_eval" and default=`Debug end)
open D *)

exception EvalError of string
exception NoSuchLabel of Vine.label
exception AssertFailure of Vine.exp 

let global_function_name = "$start"
let global_return_var = newvar "$return" Vine.REG_32

type pc = int

type ecode = {
  lblmap : (Vine.label, pc) Hashtbl.t;
  instmap : (pc, Vine.stmt) Hashtbl.t;
  sigma : (Vine.label, Vine.stmt * pc) Hashtbl.t;
  start_pc : pc;
}

let default_pc = -1

let label_to_pc ecode lbl = 
  try
    Hashtbl.find ecode.lblmap lbl
  with
  | Not_found -> raise (NoSuchLabel lbl)

let pc_to_stmt ecode pc = 
  Hashtbl.find ecode.instmap pc

let funname_to_proto_pc ecode v = 
  Hashtbl.find ecode.sigma v

let get_start_pc ecode = 
  ecode.start_pc

let functify_global_scope (dl,sl) =
(* funcs is a list of Function statements, and gs is a list of
   global non-function statements *)
  let (funcs, gs) = List.partition (fun x ->
				      match x with
					  Function _ -> true
					| Attr(Function _, _) -> true
					| _ -> false) sl in
  let global_fun = Function(global_function_name,
			    None, [], false,
			    Some(Block([], gs)))
  in
    (dl, global_fun :: funcs)
;;

let mk_ecode ((dl,sl):Vine.program) =
  let instmap = H.create (List.length sl) in
  let lblmap = H.create (List.length sl) in
  let sigma = H.create 13 in
    (* index_stmts assigns an index to each statement such that for
       all non-control-flow statements i, i+1 is the successor of i.
       Specifically, for Function(...,Block(_,blk_stmts)) which is
       statement i, we make the first statement of blk_stmt i+1.  This
       function assumes descoping is done, in particular, that
       we do not encounter Block's *)
  let rec index_stmts stmts stmtid = (
    match stmts with
	Function(v,topt,argdl,false,Some(Block(blkdl,blksl))) as f::ys ->
	  H.add sigma v (f, (stmtid+1));
	  H.add instmap stmtid f;
	  index_stmts (List.append blksl ys) (stmtid+1)
      | Attr(s, _)::ys ->
	  index_stmts (s::ys) (stmtid)
      | Label(l) as lbl::ys ->
	  H.add instmap stmtid lbl;
	  H.add lblmap l stmtid;
	  index_stmts ys (stmtid+1)
      | x::ys ->
	  H.add instmap stmtid x;
	  index_stmts ys (stmtid+1)
      | [] -> ()
  ) in
       index_stmts sl 1;
       { 
       lblmap  = lblmap;
       instmap = instmap;
       sigma  = sigma;
       start_pc = 1;
       }
	 
let print_ecode ecode pr =
  let lst = Hashtbl.fold (fun k s acc -> (k,s)::acc) ecode.instmap [] 
  in
  let lst = List.stable_sort (fun (k1,s1) (k2,s2) ->
				Pervasives.compare k1 k2) lst in
    List.iter (fun (k,s) -> 
		 match s with
		   | Function(v,_,_,_,_) -> 
		       Printf.printf "%d: <%s>\n%!" k v
		   | _ -> 
		       Printf.printf "%d: \t%s\n%!" k (Vine.stmt_to_string s)
	      ) lst


module VH = Vine.VarHash

type 'a frame = { 
  variables : 'a option Vine.VarHash.t;
  static_link : int option;
  name : Vine.label;
  mutable return_lvalue : Vine.lvalue option;
  mutable return_pc : pc;
}

let new_frame_variable frame v =
  VH.add frame.variables v None

let set_frame_value frame v va = 
  VH.replace frame.variables v (Some(va))

let get_frame_value frame v = 
  VH.find frame.variables v

let remove_frame_variable frame v = 
  VH.remove frame.variables v


let set_frame_return_pc frame pc = 
  frame.return_pc <- pc

let get_frame_return_pc frame  =
  frame.return_pc

let set_frame_return_lvalue frame lv = 
  frame.return_lvalue <- Some(lv)


let get_frame_return_lvalue frame =
  frame.return_lvalue

let mk_empty_frame n = 
  {
    variables = VH.create 0;
    static_link = None;
    name = n;
    return_lvalue = None;
    return_pc = default_pc;
  }

let mk_activation_frame vl slink lvopt retpc = function
    Vine.Function(v,_,argdl, false, Some(Block(blkdl,_))) -> (
      let frm = {
	variables = VH.create ((List.length blkdl) + (List.length argdl));
	static_link = slink;
	name = v;
	return_lvalue = lvopt;
	return_pc =retpc;
      } in 
	(* FIXME: aren't these backwards? --aij *)
	List.iter (new_frame_variable frm) blkdl;
	List.iter2 (fun v n ->
		      new_frame_variable frm n;
		      set_frame_value frm n v
		   ) vl argdl;
	frm
    )
  | _ -> raise (Invalid_argument "mk_frame requires function prototype")
  

type 'a callstack = {
  mutable fp : int;
  cs : (int, 'a frame) Hashtbl.t;
}

let mk_callstack () = 
  {fp = -1;
   cs = Hashtbl.create 5;
  }

let push_frame cs f = 
  assert (cs.fp >= -1);
  cs.fp <- cs.fp + 1;
  Hashtbl.add cs.cs cs.fp f

let pop_frame cs = 
  assert (cs.fp >= 0);
  Hashtbl.remove cs.cs cs.fp;
  cs.fp <- cs.fp -1

let top_frame cs = 
  assert (cs.fp >= 0);
  Hashtbl.find cs.cs cs.fp

let nth_frame cs n = 
  assert (n >= 0);
  Hashtbl.find cs.cs n


let callstack_var_to_frame cs n  = 
  let rec lookup_frame_variable frame  = (
    if VH.mem frame.variables n then
      frame
    else (
      match frame.static_link with
	  None -> raise (EvalError("variable "^var_to_string n^" not found"))
	| Some(i) -> lookup_frame_variable (nth_frame cs i) 
    )
  )
  in
  let f =  nth_frame cs cs.fp in
    (lookup_frame_variable f)


let get_callstack_value cs n =
  let frame = callstack_var_to_frame cs n in 
    get_frame_value frame n

let set_callstack_value cs n v = 
  let frame = callstack_var_to_frame cs n in 
    set_frame_value frame n v

let new_callstack_variable cs v = 
  let f = H.find cs.cs cs.fp in 
    new_frame_variable f v

let remove_callstack_variable cs n = 
  let frame = callstack_var_to_frame cs n in 
    remove_frame_variable frame n

let get_frame_pointer cs = 
  cs.fp
    

class virtual ['a]  evaluator = 
object(self)

  val mutable pc = 0
  method get_pc () = pc
  method set_pc p = pc <- p
  method virtual eval_exp : Vine.exp -> 'a
  method virtual step : unit -> bool
  method virtual run : unit -> 'a 
end


