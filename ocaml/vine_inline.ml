(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

open Vine

module D = Debug.Make(struct let name = "Vine_inline" and default=`Debug end)
open D

let inline_call func call =
  (* Note: This depends on alpha_vary using fresh variables for the formals *)
  let func = Vine_alphavary.alpha_vary_stmt func in
  let (name, formals, body, returnlv, actuals) = match func,call with
    | (Function(name, _rettype, formals, false, Some body),
       Call(returnlv, _fexp, actuals)) ->
	(name, formals, body, returnlv, actuals)
    | (_, Call _) ->
	failwith "inline_call: Given function must be function definition"
    | _ ->
	failwith "inline_call: Given callsite must be call stmt"
  in
  let args =
    try List.map2 (fun v e -> Move(Temp v, e)) formals actuals
    with Invalid_argument "List.map2" ->
      failwith "inline_call: func and call must have same number of arguments"
  in
  let retlab = newlab (name^"_func_return") in
  let retjmp = Jmp(Name retlab) in
  let labels = Hashtbl.create 57 in
  let label_mapper = object
    method visit_exp _ = SkipChildren
    method visit_alvalue _ = SkipChildren
    method visit_rlvalue _ = SkipChildren
    method visit_binding _ = SkipChildren
    method visit_decl _ = SkipChildren
    method visit_stmt = function
      | Label l ->
	  Hashtbl.add labels l (newlab l);
	  SkipChildren
      | _ -> DoChildren
  end in
  let _ = stmt_accept label_mapper func in
  let label_remapper = object
    inherit nop_vine_visitor
    val mutable in_jmp = false
    method visit_exp = function
      | Name x -> (
	  try ChangeTo(Name(Hashtbl.find labels x))
	  with Not_found ->
	    if in_jmp then
	      wprintf "function %s references label %s not in function" name x;
	    SkipChildren
	)
      | _ -> DoChildren
    method visit_stmt = function
      | Return None -> ChangeTo retjmp
      | Return(Some x) -> (
	  match returnlv with
	    | None ->
		wprintf "function %s returning without setting retval" name;
		ChangeTo retjmp
	    | Some lv ->
		ChangeTo(Block([], [Move(lv, x); retjmp]))
	)
      | Label l ->
	  ChangeTo(Label(Hashtbl.find labels l))
      | Jmp _ | CJmp _ as s ->
	  in_jmp <- true;
	  ChangeDoChildrenPost(s, (fun x -> in_jmp <- false; x))
      | _ -> DoChildren
  end in
  let body' = stmt_accept label_remapper body in
    args @ [body'; Label retlab]


let find_function_definitions ir =
  let ht = Hashtbl.create 57 in
  let vis = object
    inherit nop_vine_visitor
    method visit_stmt = function
      | Function(name,_,_,false, Some _) as s -> 
	  Hashtbl.add ht name s;
	  SkipChildren
      | _ -> DoChildren
  end in
  let _ = prog_accept vis ir in
    Hashtbl.find ht

class inlining_visitor name_to_def =
object
  inherit nop_vine_visitor
  method visit_stmt = function
    | Call(_, Name n, _) as s -> (
	try  ChangeTo(Block([], inline_call (name_to_def n) s))
	with Not_found -> SkipChildren
      )
    | Function _ -> SkipChildren (* don't inline inside of functions *)
    | _ -> DoChildren
  method visit_exp _ = SkipChildren
end

class recursive_inlining_visitor ?(max_rec=0) name_to_def is_recursive =
  let reccount = Hashtbl.create 57 in
  let inc name =
    try
      let x = (Hashtbl.find reccount name + 1) in
	Hashtbl.replace reccount name x;
	x
    with Not_found -> Hashtbl.add reccount name 1; 1
  in
  let dec name =
    let x = Hashtbl.find reccount name - 1 in
      if x > 0 then
	Hashtbl.replace reccount name x
      else
	Hashtbl.remove reccount name
  in
object
  inherit inlining_visitor name_to_def as super

  method visit_stmt x =
    match x, super#visit_stmt x with
      | (Call(_, Name f, _), ChangeTo y) when is_recursive f ->
	  let count = inc f in
	    if count > max_rec then
	      SkipChildren
	    else
	      ChangeDoChildrenPost(y, fun x-> dec f; x)
      | (Call(_, Name f, _), ChangeTo y) ->
	  ChangeDoChildrenPost(y, fun x->x)
      | (_, z) -> z
end
