(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Weakest Precondition
*)

open Vine
open Gcl

module D = Debug.Make(struct let name = "WP" and default=`Debug end)
open D

(** Calculate the weakest precodition, using simp to simplify the
    resulting expression. This does better than leino-wp (because we
    don't need SSA form: we let let's scope) if the branching factor
    isn't too great.  It does do worse on something like:
    if(e1) .. else ...
    if(e2) .. else ..
    if(e3) .. else ..
    assert(postcondition)
*)
let calculate_wp (simp:Gcl.exp->Gcl.exp) (post:Gcl.exp) (stmt:Gcl.t)  =
  let rec wp q s k =
    match s with
	Skip -> k q
      | Assume e ->
	  k (simp(exp_implies e q)) (* exp_and(e, post) *)
      | Choice(s1, s2) ->
	  wp q s1 (fun x -> wp q s2 (fun y -> k(simp(exp_and x y ))))
      | Seq(s1, s2) ->
	  wp q s2 (fun x -> wp x s1 k)
      | Assign(t, e) ->
	  k(simp(Let(t, e, q)))
      | Assert e -> 
	  k (simp(exp_and e q))
  in
    wp post stmt (fun x->x)

(** calculate the weakest liberal precondition, using simp to simplify
    the resulting expression *)
let calculate_wlp (simp:Gcl.exp->Gcl.exp) (post:Gcl.exp) (stmt:Gcl.t)  =
  let rec wlp q s k =
    match s with
	Skip -> k q
      | Assume e | Assert e -> 
	  k (simp(exp_implies e q)) 
      | Choice(s1, s2) ->
	  wlp q s1 (fun x -> wlp q s2 (fun y -> k(simp(exp_and x y ))))
      | Seq(s1, s2) ->
	  wlp q s2 (fun x -> wlp x s1 k)
      | Assign(t, e) ->
	  k(simp(Let(t, e, q)))
  in
    wlp post stmt (fun x->x)



(** Calculate the weakest precodition, using simp to simplify the resulting
  expression. The given GCL stmt must be in SSA form. *)
let calculate_wp_ssa simp post stmt =
  let rec wp q s k =
    match s with
	Skip -> k q
      | Assume e ->
	  k (simp(exp_implies e q)) (* exp_and(e, post) *)
      | Choice(s1, s2) ->
	  let tmp = Temp(newvar "newpost" bool_t) in
	  let q' = Lval tmp in
	    wp q' s1
	      (fun x ->
		 wp q' s2
		   (fun y ->
		      k (simp(Let(tmp, q, exp_and x y))) ))
      | Seq(s1, s2) ->
	  wp q s2 (fun x -> wp x s1 k)
      | Assign(t, e) ->
	  (* turn assignments into assumes if the GCL is from SSA form. *)
	  (* Note: to avoid future errors, this cannot be a let. t needs to
	     be global, because t may be used by a postcondition which is
	     outside the would-be-Let due to the Choice optimization *)
	  wp q (Assume(BinOp(EQ, Lval t, e))) k
      | Assert e -> 
	  k (simp(exp_and e q)) (* exp_and(e, post) *)
    (* Note if/when we add Assert, we will need a separate wp and wlp *)
  in
    wp post stmt (fun x->x)



(** Computes the Leino Weakest Precondition. The given GCL stmt must be in
    passified form. (no assignments). To passify, you should
    1. convert to SSA
    2. remove phi's
    3. convert back to cfg.
*)
let mk_and simp e1 e2 = 
  match e1,e2 with
      _, Constant(Int(REG_1, 0L)) -> exp_false
    | Constant(Int(REG_1, 0L)), _ -> exp_false
    | Constant(Int(REG_1, 1L)), _ -> e2
    | _, Constant(Int(REG_1, 1L)) -> e1
    | _,_ -> simp (exp_and e1 e2)
;;

let mk_implies simp e1 e2 = 
  match e1,e2 with
      Constant(Int(REG_1, 0L)), _ -> exp_true
    | _, Constant(Int(REG_1, 1L)) -> exp_true
    | _, Constant(Int(REG_1, 0L)) -> e1
    | Constant(Int(REG_1, 1L)), _ -> e2
    | _,_ -> simp (exp_implies e1 e2)
;;

let mk_or simp e1 e2 = 
  match e1,e2 with
      Constant(Int(REG_1, 1L)), _ -> exp_true
    | _, Constant(Int(REG_1, 1L)) -> exp_true
    | _, Constant(Int(REG_1, 0L)) -> e1
    | Constant(Int(REG_1,0L)), _ -> e2
    | _,_ -> BinOp(BITOR, e1, e2)
;;

(** do not use: this calculates the WP using dijkstra's semantics. It
   is really only useful as a comparison for more efficient things. *)
let dijkstra_wp (simp:Gcl.exp->Gcl.exp) (post:Gcl.exp) (stmt:Gcl.t)  =
  let subst q t e = 
    let vis = object(self)
      inherit nop_vine_visitor
      method visit_exp e1 = 
	match e1 with
	    Lval(lv) when lv = t -> ChangeTo(e)
	  | _ -> DoChildren
    end
    in exp_accept vis q
  in
  let rec wp q s  =
    match s with
	Skip ->  q
      | Assume e ->
	  mk_implies simp e q 
      | Choice(s1, s2) ->
	  let q1 = wp q s2 in 
	  let q2 = wp q s1 in 
	    mk_and simp q1 q2
      | Seq(s1, s2) ->
	  wp (wp q s2) s1
      | Assign(t, e) ->
	  subst q t e 
      | Assert e -> 
	  mk_and simp e q
  in
    wp post stmt 
;;

(** leino_wp calculates the weakest precondition using the predicate
    transformers from  Leino's paper "Efficient Weakest
    Preconditions". This is the algorithm djb proved correct. *)
let leino_wp (simp:Gcl.exp->Gcl.exp) (post:Gcl.exp) (stmt:Gcl.t)  =
  let rec  wp q s  =
    match s with
	Skip ->  q
      | Assume e ->
	  mk_implies simp e q 
      | Choice(s1, s2) -> 
	  let q1 = wptransform q s1 in 
	  let q2 = wptransform q s2 in 
	    mk_and simp q1 q2
      | Seq(s1, s2) -> 
	  let q1 = wptransform q s2 in 
	    wptransform  q1  s1
      | Assign(t, e) ->
	  mk_implies simp (BinOp(EQ, Lval(t), e)) q
      | Assert e -> 
	  mk_and simp e q
  and wlp q s =
    match s with
	Skip ->  q
      | Assert e 
      | Assume e ->
	  mk_and simp e q 
      | Choice(s1, s2) ->
	  let q1 = wlptransform q s2 in 
	  let q2 = wlptransform q s1 in 
	    mk_and simp q1 q2
      | Seq(s1, s2) -> 
	  let q1 = wlptransform  q s2 in 
	    wlp q1 s1
      | Assign(t, e) ->
	  mk_implies simp (BinOp(EQ, Lval(t), e)) q
  and wptransform  q  s = 
    mk_and simp (wp exp_true s) (mk_or simp (wlp exp_false s) q)
  and wlptransform q s = 
    mk_or simp (wlp exp_false s) q
  in
    wptransform post stmt
;;



(** run simp every n times, otherwise run other *)
let simp_skip other n simp =
  let count = ref n in
  let f e =
    if !count = 0
    then (count := n; simp e)
    else (count := !count-1; other e)
  in f

let simp_debug before after simp e =
  let () = before e in
  let r = simp e in
  let () = after r in
    r

let simp_print =
  simp_debug
    (fun e ->
       print_string "\nSimplifying "; flush stdout;
       pp_exp print_string e;
       print_string "\n"; flush stdout)
    (fun e ->
       print_string "\n-> "; flush stdout;
       pp_exp print_string e;
       print_string "\n"; flush stdout)


(** Takes a WP expression with Lets and converts it to use globals instead.
    The given expression must have been generated from SSA in order for the
    result to make sense.
    
    @return the globals which must now be declared outside the expression,
    and the expression.
*)
let globalize_wp e =
  let () = pdebug "globalizing WP" in
  let locals = VarHash.create 57 in
  let vis = object
    inherit nop_vine_visitor
    method visit_exp = function
      | Let(Temp v as l,e1,e2) ->
	  (* replace, because after removing phis, the variable may be assigned
	     more than once *)
	  VarHash.replace locals v ();
	  pdebug (var_to_string v);
	  let e' = BinOp(BITAND, BinOp(EQ, Lval l, e1), e2) in
	    ChangeDoChildrenPost(e', fun x->x)
      | Let(Mem(x,_,_), _, Lval(Temp y)) when Var.equal x y ->
	  (* our current representation for WITH *)
	  DoChildren
      | Let(Mem _, _, _) ->
	  failwith "globalize_wp: given WP needs to have been computed from SSA"
      | _ -> DoChildren
  end in
  let res = exp_accept vis e in
  let vars = VarHash.fold (fun k () l -> k::l) locals [] in
  let () = pdebug "done globalizing WP" in
    (vars, res)
