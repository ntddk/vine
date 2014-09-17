(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Independent clause stuff
*)

open Vine
open Vine_util

module D = Debug.Make(struct let name = "Vine_indepclauses" and default=`Debug end)
open D

let simplify =
  Vine_opt.simplify_rec
    (fun _ -> function
       | BinOp(BITAND,_,_) as e ->
	   dprintf "substituting conjunction: %s" (exp_to_string e);
	   true 
       | _ -> false)

(** Split the conjunctions inside an expresssion *)
let split_conjunction e =
  let rec split wrap l e =
    match e with
	BinOp(BITAND, e1, e2) ->
	  let l1 = split wrap l e1 in
	    split wrap l1 e2
      | Let(p,e1,e2) ->
	      split (fun e -> wrap(Let(p,e1,e))) l e2
      | _ -> wrap e :: l
  in
  let res = split (fun x -> x) [] e in
    dprintf "split into %d conjunctions" (List.length res);
    res


let split_indep e =
  let e = Vine_opt.simplify_faster(Vine_alphavary.alpha_vary_exp e) in
  let e = simplify e in (* substitutes lets with conjunctions *)
  let es = split_conjunction e in
  (* need to simplify so that unused inputs won't be in the free variables *)
  let es = List.map Vine_opt.simplify_faster es in
    (* FIXME: do we want to make two expressions that use memory independent? *)
  let groupings = Vine_util.union_find freetemps_exp es in
  let () = dprintf "found %d groupings" (List.length groupings) in
    List.map (list_join (fun x y -> BinOp(BITAND,x,y))) groupings
