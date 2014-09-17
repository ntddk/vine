(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

open Vine_eval
module V = Vine
module H = Hashtbl
module D = Debug.Make(struct let name = "Vine_ceval" and default=`NoDebug end) 
open D
module List = ExtList.List

(** whether to allow reading of uninitialized values *)
let allow_uninitialized_reads  =  ref true;;

type value = Int of V.typ * int64
   | Str of string
   | Arr of (value, value) Hashtbl.t
   | Lbl of V.label



let value_to_string (v:value) : string =
  match v with
      Int(t,i) -> Printf.sprintf "%Lx:%s" i (Vine.type_to_string t)
    | Str(s) -> Printf.sprintf "%s:string_t" s
    | Arr(tbl) -> "<array>"
    | Lbl(l) -> Printf.sprintf "%s:label_t" l
;;


let const_to_value c  =
  match c with
      V.Constant(V.Str(s)) -> Str(s)
    | V.Constant(V.Int(t,i)) -> Int(t,i)
    | _ -> failwith "Invalid const_to_value"
 
let value_to_const v  =
    match v with
	Int(t,c) -> V.Constant(V.Int(t,c))
      | Str(s) -> V.Constant(V.Str(s))
      | _ -> failwith "Invalid value_to_const"


let copy_value = function 
    Arr(tbl) -> Arr(H.copy tbl)
  | Str(str) -> Str(String.copy str)
  | _ as v -> v


let value_to_pc ecode v =
  match v with
      Lbl(l) -> label_to_pc ecode l
      | Int(addr_t, i) -> (* lift an address to a label *)
	  let l = Vine.addr_to_label i in
	    label_to_pc ecode l
      | _ ->  raise
	  (EvalError(Printf.sprintf "Invalid value used as label: %s"
		       (value_to_string  v)))
	    




(* This used to be in Vine *)


type du = Def | Use | DefUse
      
(* return def use sets.  Includes locals inside a let. *)
class def_use_visitor  = 
object(self)
  inherit V.nop_vine_visitor

  val mutable defset = V.VarSet.empty
  val mutable useset = V.VarSet.empty

  method visit_alvalue lv = 
    match lv with
	V.Temp(v) 
      | V.Mem(v,_,_) ->  defset <- V.VarSet.add v defset; V.DoChildren
	 
  method visit_rlvalue lv = 
    match lv with
	V.Temp(v) 
      | V.Mem(v,_,_) -> useset <- V.VarSet.add v useset; V.DoChildren


  method referenced () = V.VarSet.union defset useset
  method used  = useset
  method defed  = defset

end


(* [def_use_of_stmt s] will return a tuple (defined variables, used
    variables) for statement [s]. The optional size is an approximate
    number of variables in the stmts.
*)
let def_use_of_stmts  sl = 
  let vu = new def_use_visitor in 
  let () = List.iter (fun x -> ignore(V.stmt_accept vu x)) sl in
    (V.VarSet.elements vu#defed, V.VarSet.elements vu#used)
;;    

(* [def_use_of_stmts sl] will return a tuple (defined variables, used
    variables) for statements [sl]. The optional size is an
    approximate number of variables in the stmts.
*)
let def_use_of_stmt  s = 
  def_use_of_stmts [s]

(** [referenced_of_exp e] will return all variables referenced by
    expression [e]. There will be no duplicates in the list, e.g., if
    a variable is referenced 2 times, it only appears once in the list. *)
let  referenced_of_exp e = 
  let vu = new def_use_visitor in 
  let () = ignore(V.exp_accept vu e) in 
    V.VarSet.elements (vu#referenced ())

(** [referenced_of_stmt s] will return all variables referenced by
    stmt [s]. There will be no duplicates in the list, e.g., if
    a variable is referenced 2 times, it only appears once in the list. *)
let referenced_of_stmt s =
  let vu = new def_use_visitor in 
  let () = ignore(V.stmt_accept vu s) in
    V.VarSet.elements (vu#referenced ())


(* [remove_unreferenced_variables (dl,sl)] returns a prog where dl only
    contains variables that have been referenced. Note that if the
    program has not been alpha-varied, we may include more variables than
    necessary because we use a visitor that counts local variables as
    referenced, and if they happen to have the same name as a regular
    variable, will also be counted. This should always be safe, i.e.,
    an over-approximation of the variables needed.  *)
let remove_unreferenced_variables (dl,sl) =
  let vis = new def_use_visitor in 
  let () = List.iter (fun s -> ignore(V.stmt_accept vis s))  sl in
  let refed = vis#referenced () in 
    (List.filter (fun d -> V.VarSet.mem d refed) dl, sl)
;;


(* end used to be in vine *)



class  concrete_evaluator ((dl,sl) as program)  = 
  (* the global return value. used if the global scope returns  *)
  let (dl',sl') = Vine_alphavary.descope_program (dl,sl) in
  (* move all function calls to the end. We give each statement
  a number, decending into Function blocks. For example, if you have 
      1. x =y;
      2. void foo() {  
      3. y = t; 
         } 
   If we allowed code before functions, then we would just start
     executing through a function body since instruction 2 is the next
  after 3. So we sort all instructions so functions come last, and
  insert a halt as the last global instruction 
  *)
  let funs, nonfuns = List.partition (function V.Function _ -> true
					| _ -> false) sl' in 
  let (dl',sl') = (dl', List.append  
		     (List.append nonfuns [V.Halt(V.exp_true)]) funs) in 

(*  let glbl_ret_var = V.Temp global_return_var in 
  let cl = V.Call(Some(glbl_ret_var), V.Name(global_function_name),[]) in 
  let (dl',sl') = functify_global_scope (dl',sl') in  *)

    (* uncomment if you want to print out what is executing *)
(*  let oc = open_out "djb.ir" in
  let () = Vine.pp_program (output_string oc) (dl',sl') in
  let () = close_out oc in *)

  let ec = mk_ecode (dl',sl') in 

  let glbl_frame = mk_empty_frame "$globals" in
(*  let () = new_frame_variable glbl_frame global_return_var in 
  let () = set_frame_return_lvalue glbl_frame glbl_ret_var in   *)
  let () = List.iter (new_frame_variable glbl_frame) dl' in 
  let cs = mk_callstack () in 
  let () = push_frame cs glbl_frame in 

object(self)
  inherit [value] evaluator 

  val ecode = ec
  val mutable cs = cs
  val mutable is_halted = false

  val mutable halt_value = None


  initializer
    pc <- get_start_pc ecode

  method is_halted () = is_halted

  method private  get_next_stmt () = (
    try
      Some(pc_to_stmt ecode pc)
    with
	Not_found ->
	  if (Vine_eval.get_frame_pointer cs) <> 0 then (
	    let this_frame = top_frame cs in 
	    let () = pop_frame cs in 
	      match (get_frame_return_lvalue this_frame) with
		  None -> 
		    pc <- (get_frame_return_pc this_frame);
		    if (get_frame_pointer cs) = 0 then (
		      is_halted <- true;
		      None
		    ) else (
		      self#get_next_stmt () 
		    )
		| Some _ ->
		    failwith "function failed to return required value"
	  ) else (is_halted <- true; None)
  ) 

      
  method step () = 
    if not is_halted then (
      let s = self#get_next_stmt ()  in 
	match s with
	    None -> is_halted <- true; true
	  | Some(s) -> 
	      if D.debug then 
		D.dprintf "Stmt %d:\t%s" pc (Vine.stmt_to_string s);
	      self#print_used_info s;
	      self#eval_stmt s;
	      self#print_defed_info s;
	      is_halted
    ) else true 

  method run () = 
    while not is_halted do
      let s = self#get_next_stmt ()  in 
	match s with 
	    None -> ()
	  | Some(s) -> 
	      if D.debug then 
		D.dprintf "Stmt %d:\t%s" pc (Vine.stmt_to_string s);
	      self#print_used_info s;
	      self#eval_stmt s;
	      self#print_defed_info s
    done;
      match halt_value with
	  None -> Int(Vine.REG_32, 0L)
	| Some(v) -> v

(*
      match halt_value with
	  None -> 
	    raise (EvalError ("Program failed to return a value"))
	| Some(v) -> Some(v
*)

  method get_ecode () =  ec

  method private print_used_info s =
    if D.debug then 
      let used = Vine.freerefs_stmt s in
      let used = List.fold_left (fun vars -> function Vine.Temp x -> x::vars | _ -> vars) [] used in
      let str = 
	List.fold_left (fun acc ((_,nm,_) as var) -> (* try *)
			  (* we may fail to find a used variable in a
			     let expression, since its value may have
			     been local to the expression and removed
			     from the stack.  *)
			  let value = 
			    get_callstack_value cs var
			  in
			  let vs = (
			    match value with
				None -> "<none>"
			      | Some(v') -> value_to_string v'
			  ) in 
			    nm^"="^vs^", "^acc
			      (* FIXME: This was throwing out *ALL* exceptions.
				 If it is really ok to ignore some exceptions,
				 you should be ignoring those specifically.
			with
			    _ -> acc
			      *)
		       )
	  ""  used 
      in(
	if str <> "" && D.debug then 
	  D.dprintf "Used %d: \t[%s]" pc str
	)
    else
      ()

  method private print_defed_info s =
    if D.debug then 
      let (defed,_) = def_use_of_stmt s in 
      let str = 
	List.fold_left (fun acc ((_,nm,_) as var) -> 
			  (* don't let debugging mess us up with local
			     let variables which may no longer be
			     around *)
			  try
			  let value = get_callstack_value cs var in
			    match value with
				None -> acc
			      | Some(v') ->  
				  nm^"="^(value_to_string v')^", "^acc
			      (* FIXME: This was ignoring *ALL* exceptions.
				 If it is really ok to ignore some exceptions,
				 you should be ignoring those specifically. *)
			with
			    Vine_eval.EvalError _ -> acc

		       ) ""  defed
      in (
	if str <> "" && D.debug then 
	  D.dprintf "Def %d:\t[%s]" pc str
	)
    else
      ()

  (* Return a list of the offsets of the bytes within a word of the
     given width and endianness, in less-significant to more-significant
     order. For instance, for a little-endian REG_32 it returns
     [0; 1; 2; 3] since the LSB of the word *(int * )a is at "a" and the
     MSB is at "a + 3". *)
  method private byte_offsets width endian =
    let size = match width with
      | V.REG_8  -> 1
      | V.REG_16 -> 2
      | V.REG_32 -> 4
      | V.REG_64 -> 8
      | _ -> raise (EvalError "Unexpected memory operation size")
    in
      match endian with
	| V.Little -> List.init size (fun x -> x)
	| V.Big -> List.rev (List.init size (fun x -> x))
    
  (* Implement a load with the given endianness and width on a memory
     represented as a hash mapping address values to byte values. *)
  method private memory_load hash endian width addr_val =
    let addr = match addr_val with
      | Int(_, a) -> a
      | _ -> raise (EvalError "Memory address must be an integer") in
    let rec load_bytes offsets = match offsets with
      | [] -> Int64.zero
      | off::os ->
	let a = Int64.add addr (Int64.of_int off) in
	let byte = match H.find hash (Int(V.REG_64, a)) with
	  | Int(V.REG_8, i) ->
	      i
	  | _ -> raise (EvalError "Memory must contain bytes")
	in
	Int64.logor (Int64.shift_left (load_bytes os) 8) byte
    in
      Int(width, load_bytes (self#byte_offsets width endian))

  (* Implement a store with the given endianness and width on a memory
     represented as a hash mapping address values to byte values. *)
  method private memory_store hash endian width addr_val value_val =
    let addr = match addr_val with
      | Int(_, a) -> a
      | _ -> raise (EvalError "Memory address must be an integer") in
    let value = match value_val with
      | Int(width, v) -> v
      | _ -> raise (EvalError("Stored value must be an integer of "^
				"the appropriate width")) in
    let rec store_bytes bits offsets = match offsets with
      | [] -> ()
      | off::os -> begin
	  H.replace hash (Int(V.REG_64, (Int64.add addr (Int64.of_int off))))
	    (Int(V.REG_8, (Int64.logand bits (Int64.of_int 255))));
	  store_bytes (Int64.shift_right_logical bits 8) os
	end
    in
      store_bytes value (self#byte_offsets width endian)

  method private get_lval  = function
      (* note: all lvalues are initialized via set_lval, i.e., via a
      write to either the variable or memory address. Thus, the
      callstack lookup here should always be returning a value unless
      you are reading an uninitialized value.  *)
      V.Temp((_,_,t) as n) -> (
	let aropt = get_callstack_value cs n  in
	  match aropt with
	      None -> 
		let msg = V.var_to_string n^ " uninitialized read" in
		if !allow_uninitialized_reads then  (
		  pwarn msg;
		  match t with
		      V.REG_1
		    | V.REG_8
		    | V.REG_16
		    | V.REG_32
		    | V.REG_64 -> Int(t, 0L)
		    | _ -> raise 
			(EvalError(
			   V.var_to_string n^
			     " unhandled uninitialized access"))
					      

		)  else 
		  raise (EvalError msg)
	    | Some(v) -> v
      )
    | V.Mem((_, _, mem_t) as n,ind,t) -> (
	let aropt = get_callstack_value cs n in 
	let ind_val = self#eval_exp ind in
	  ( 
	    match aropt with 
		None -> 
		  (* if no memory is found, this means the memory
		     variable has never been written to in the past,
		     thus all indexes have undefined values. We could
		     create a memory and initialize the value to 0, but
		     this could mask bugs in our code.  *)
		  let msg = 			   Printf.sprintf 
			     ("%s[%s] uninitialized\n"^^
			     " (memory has never been written to, so all"^^
			     " indexes uninitialized)") (V.var_to_string n)
			     (value_to_string ind_val)
		  in
		  if !allow_uninitialized_reads then  (
		    pwarn msg;
		    match t with
			V.REG_1
		      | V.REG_8
		      | V.REG_16
		      | V.REG_32
		      | V.REG_64 -> Int(t, 0L)
		      | _ -> raise 
			  (EvalError(
			     V.var_to_string n^
			       " unhandled uninitialized access"))
		  ) else 
		    raise (EvalError msg)
	      | Some(Arr(tbl)) -> (
		  try
		    match mem_t with
		      | V.Array _ ->
			  H.find tbl ind_val
		      | V.TMem(idx_t, endian) ->
			  self#memory_load tbl endian t ind_val
		      | _ -> raise
			  (EvalError ("Illegal memory type in load"))
		  with
		      Not_found ->
                        let msg = Printf.sprintf "%s[%s] uninitialized"
			  (V.var_to_string n) 
			  (value_to_string ind_val)
                        in
		        if !allow_uninitialized_reads then  (
		          pwarn msg;
		          match t with
			    V.REG_1
		          | V.REG_8
		          | V.REG_16
		          | V.REG_32
		          | V.REG_64 -> Int(t, 0L)
		          | _ -> raise
			      (EvalError(
			         V.var_to_string n^
			           " unhandled uninitialized access")))
                        else
			  raise (EvalError(msg))
		)
	      | _ -> raise (EvalError ("Value for "^
					 V.var_to_string n^
					 " not a hashtbl"))
	  )
      )

  method private set_lval (v:value) = function
      V.Temp n ->
(*	D.dprintf "\t(%s = %s)\n%!" (V.var_to_string n)
	(value_to_string v); *)
	set_callstack_value cs n (copy_value v)

    | V.Mem((_,_,mem_t) as n, ind,t) -> (
	let aropt = get_callstack_value cs n in 
	let ar = (
	  match aropt with
	      None -> 
		let ht = Arr(Hashtbl.create 57) in 
		let () = set_callstack_value cs n ht in 
		  ht
	    | Some(ar) -> ar
	) in 
	let ind_val = self#eval_exp ind in
	  match (ar, mem_t) with
	      (Arr(tbl), V.Array _) ->
(*		D.dprintf "\t(%s[%s] = %s)\n%!"
		  (V.var_to_string n)
		  (value_to_string ind_val)
		  (value_to_string v); *)
		H.replace tbl ind_val (copy_value v)
	    | (Arr(tbl), V.TMem(idx_t,endian)) ->
		self#memory_store tbl endian t ind_val (copy_value v)
	    |_  -> raise (EvalError(
			    Printf.sprintf "%s[%s] invalid location"
			      (V.var_to_string n) (value_to_string ind_val)))
      )


  method private eval_exp e = 
    let raise_eval_error msg x =
      raise (EvalError (msg^" operands did not evaluate to constants"))
    in
      match e with
	  V.BinOp(bop, e1, e2) ->
	    let c1 = value_to_const (self#eval_exp  e1) in
	    let c2 = value_to_const (self#eval_exp  e2) in
	      const_to_value (Vine_opt.constant_fold 
				(raise_eval_error "BinOp")
				(V.BinOp(bop, c1,c2)))
	| V.UnOp(uop, e) ->
	    let c1 = value_to_const (self#eval_exp  e) in
	      const_to_value (Vine_opt.constant_fold (raise_eval_error "UnOp")
				(V.UnOp(uop, c1)))
	| V.Constant _ -> const_to_value e
	| V.Name(l) -> Lbl(l)
	| V.Cast(ct,t,e') ->
	    let c1 = value_to_const (self#eval_exp  e') in
	      const_to_value (Vine_opt.constant_fold (raise_eval_error "Cast")
				(V.Cast(ct,t,c1)))
	| V.Unknown(s) ->
	    raise (EvalError ("Cannot evaluate:  "^(V.exp_to_string e)))
	| V.Let(V.Temp n as tmp, e1, e2) ->
	    let v1 = self#eval_exp e1 in
	    let () = new_callstack_variable cs n in 
	    let () = self#set_lval v1 tmp in 
	    let v2 = self#eval_exp e2 in
	    let () = remove_callstack_variable cs n in
		v2
	| V.Let(V.Mem _ as m, e1, e2)  ->
	    let v1 = self#eval_exp e1 in
	      self#set_lval v1 m;
	      self#eval_exp e2
	| V.Lval(lv) -> self#get_lval lv


	

  method private eval_stmt s = (

    match s with
      V.Jmp(e) ->
	let v = self#eval_exp  e in
	let pc' = value_to_pc ecode v in
	  pc <- pc'
    | V.CJmp(e1, e2, e3) ->  (
	let v = self#eval_exp  e1 in
	let pc' =  (
	  match v with
	      Int(_, 0L) -> value_to_pc ecode (self#eval_exp  e3)
	    | Int _ -> value_to_pc ecode (self#eval_exp e2)
	    | _ -> raise (EvalError(
			    Printf.sprintf
			      "Invalid conditional value: %s"
			      (value_to_string v)))
	) in
	  pc <- pc'
      )
    | V.Move(lv, e) ->
	let v1 = self#eval_exp e in
	  self#set_lval  v1 lv;
	  pc <- (pc + 1)
    | V.Special(str) as s ->
	raise (EvalError (Printf.sprintf "Cannot evaluate %s"
			    (Vine.stmt_to_string s)))
    | V.Label(l) -> pc <- (pc + 1);
    | V.ExpStmt(e) ->
	let _ = self#eval_exp  e in
	  pc <- pc + 1
	    
    | V.Comment _ -> pc <- pc + 1
    | V.Block _ ->
	failwith "Should not encounter blocks during evaluation"
    | V.Function _ -> pc <- pc + 1
    | V.Return(eo) ->  (
	match eo with
	    None -> (
	      let this_frame = top_frame cs in 
	      let () = pop_frame cs in 
		match (get_frame_return_lvalue this_frame) with
		    None -> 
		      pc <- (get_frame_return_pc this_frame);
		      if (get_frame_pointer cs) < 0 then
			is_halted <- true;
		  | Some _ ->
		      failwith "function failed to return required value"
	    )
	  | Some(e) -> (
	      let v = self#eval_exp  e  in
	      let f = top_frame cs in 
	      let () = pop_frame cs in (
		match (get_frame_return_lvalue f) with
		    None -> ()
		  | Some(lv) -> self#set_lval v lv
		);
		pc <- get_frame_return_pc f;
		if (get_frame_pointer cs) < 0 then
		  is_halted <- true;
	    )
      )
    | V.Call(retopt, V.Name(fname), el) -> (
	let actuals = List.map (self#eval_exp ) el in
	let (sign,pc') = funname_to_proto_pc ecode fname in 
	let frame = mk_activation_frame actuals (Some(0)) retopt (pc+1) sign in
	  push_frame cs frame;
	  pc <- pc'
      )
    | V.Call _ -> raise (EvalError "Indirect calls not supported")
    | V.Attr(s, _) ->  
	(* we don't decend into s, since in the ecode 
	   instmap s is labeled as the next instruction *)
	pc <- pc + 1
    | V.Assert(e) -> (
	let v = self#eval_exp e in
	  match v with
	      Int(Vine.REG_1, 1L) -> pc <- pc+1
	    | _ -> 
		(* D.dprintf "Assert %s failed!"  (V.exp_to_string e);*)
		(* is_halted <- true; *)
		raise (Vine_eval.AssertFailure ( e))
      )
    | V.Halt(e) -> (
	let v = self#eval_exp e in 
	  halt_value <- Some(v);
	  is_halted <- true
      )
  )
end
    
    
