(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Translations from VinE expressions to SMT-Lib format.
    See the to_string, to_file, and print functions.

    For more on the SMT-Lib syntax we use, see
    http://goedel.cs.uiowa.edu/smtlib/theories/BitVectors.smt
 *)

open Vine
open Printf
module VH = Vine.VarHash
open ExtList

module D = Debug.Make(struct let name = "SMT" and default=`Debug end)
open D


let unletify e =
  let assigns = ref [] in
  let push x = assigns := x :: !assigns in
  let ctx = VH.create 5700 in
  let vis =
object(self)
  inherit nop_vine_visitor
  method visit_exp = function
    | Let(l, e1, e2) ->
	let (l', v, v') = match l with
	  | Temp v ->
	      let v' = renewvar v in
		(Temp v', v, v')
	  | Mem(v, i, t) ->
	      let v' = renewvar v in
	      let i' = exp_accept self i in
		(Mem(v, i', t), v, v')
 	in
	let e1' = exp_accept self e1 in
	  push (v', l', e1');
	  VH.add ctx v v';
	let e2' = exp_accept self e2 in
	  VH.remove ctx v;
	  ChangeTo e2'
    | _ ->
	DoChildren
  method visit_rlvalue l = 
    try ChangeTo(match l with Temp v ->Temp(VH.find ctx v)
		   | Mem(v,i,t) -> Mem(VH.find ctx v, i, t)
		)
    with Not_found -> SkipChildren
end in
  let e' = exp_accept vis e in
    (List.rev !assigns, e')

(* Translate a variable to conform to SMT syntax *)
let transvar s = s

let var2s (num,name,_) =
    name^"_"^string_of_int num

let rec type2s = function
  | t when is_integer_type t ->
      "BitVec["^string_of_int(Vine.bits_of_width t)^"]"
  | Array(t2,i) when is_integer_type t2 ->
      "Array[64:"^string_of_int(Vine.bits_of_width t2)^"]"
  | x ->
      failwith("Unsupported type for translation to SMT-Lib: "^type_to_string x)

(** Visitor for printing out SMT-Lib sytax for an expression. *)
class vine_smtlib_print_visitor puts =
object (self)
  inherit nop_vine_visitor

  (* map vine var to string, for special vars *)
  val g = VH.create 57

  val mutable unknown_counter = 0

  method extend2 v s =
    VH.add g v s

  method tr_var v =
    try VH.find g v
    with Not_found -> var2s v

  method declare_var ((_,_,t) as v) = 
    puts(sprintf "  :extrafuns ((%s %s))\n" (var2s v) (type2s t))

  method declare_freevars e =
    let fvs = get_req_ctx e in 
      dprintf "%d free variables\n%!" (List.length fvs);
      List.iter self#declare_var fvs


  method puts_store m addr e =
    let oldmem = self#tr_var m in
      puts ("(store "^oldmem^" ");
      ignore(exp_accept (self :> vine_visitor) (Cast(CAST_UNSIGNED,REG_64,addr)));
      puts "  ";
      ignore(exp_accept (self :> vine_visitor) e);
      puts ")";


  method visit_exp e =
    match e with
	Constant(v) ->
	  (match v with
	      Int(t,i) ->
		let bits = bits_of_width t in
		let maskedval = Int64.logand i
		  (Int64.pred(Int64.shift_left Int64.one bits))
		in
		  puts "bv";
		  puts (Int64.to_string maskedval);
		  puts "[";
		  puts (string_of_int bits);
		  puts "]"
	     | _ -> 
		 raise (Invalid_argument 
			  "Only constant integer types supported")
	  );
	  SkipChildren
      | Lval(Temp v) ->
	  puts(self#tr_var v);
	  SkipChildren
      | Lval(Mem(m, idx,t)) when Stp.is_not_memory t ->
	  puts "(select ";
	  puts (self#tr_var m);
	  ignore(exp_accept self (Cast(CAST_UNSIGNED,REG_64,idx)));
	  puts ")";
	  SkipChildren
      | Lval(Mem _) ->
	  raise (Invalid_argument "Memory type not handled")
      | Let(Mem(m,i,_), e, Lval(Temp m')) when m == m' ->
      (* Keep this, since it is the only syntax we have for a store expression *)
	  self#puts_store m i e;
	  SkipChildren
      | Let(l,e1,_) ->
	  Printf.eprintf "let %s = %s in ...\n" (lval_to_string l) (exp_to_string e1);
	  failwith "Unexpected Let. Expression should have been unletified."
      | UnOp(uop, _) ->
	  let () = match uop with
	      NEG -> puts "(bvneg "
	    | NOT -> puts "(bvnot "
	  in
	    ChangeDoChildrenPost(e, fun x ->(puts ")";x))
      | BinOp(bop, e1, e2) ->
	  let (pre,mid,post) = match bop with
	      PLUS     -> ("(bvadd ", " ", ")")
	    | MINUS    -> ("(bvadd ", " (bvneg ", "))")
	    | TIMES    -> ("(bvmul " , " ", ")")
	    | DIVIDE   -> ("(bvudiv ", " ", ")")
	    | SDIVIDE  -> ("(bvsdiv ", " ", ")")
	    | MOD      -> ("(bvurem ", " ", ")")
	    | SMOD     -> ("(bvsrem ", " ", ")") (* FIXME: bvsmod or bvsrem? *)
	    | BITAND   -> ("(bvand ", " ", ")")
	    | BITOR    -> ("(bvor ", " ", ")")
	    | XOR      -> ("(bvxor ", " ", ")")
	    | EQ       -> ("(ite (= ", " ", ") bit1 bit0)")
	    | NEQ      -> ("(ite (= ", " ", ") bit0 bit1)")
	    | LT       -> ("(ite (bvult ", " ", ") bit1 bit0)")
	    | LE       -> ("(ite (bvule ", " ", ") bit1 bit0)")
	    | SLT      -> ("(ite (bvslt ", " ", ") bit1 bit0)")
	    | SLE      -> ("(ite (bvsle ", " ", ") bit1 bit0)")
	    | LSHIFT   -> ("(bvshl ", " ", ")")
	    | ARSHIFT  -> ("(bvashr ", " ", ")")
	    | RSHIFT   -> ("(bvlshr ", " ", ")")
	  and e2 =  match bop with
	    | LSHIFT | ARSHIFT | RSHIFT ->
		(* SMT-LIB requires both shift operands to have the same size *)
		let t = Vine_typecheck.infer_type None e1
		and t2 = Vine_typecheck.infer_type None e2 in
		let b = bits_of_width t and b2 = bits_of_width t2 in
		  (* argh, our typechecker currently allows b2 > b *)
		  if b > b2 then      Cast(CAST_UNSIGNED, t, e2)
		  else if b < b2 then Cast(CAST_LOW, t, e2)
		  else e2
	    | _ ->
		e2
	  in
	    puts pre;
	    ignore(exp_accept (self :> vine_visitor) e1);
	    puts  mid;
	    ignore(exp_accept (self :> vine_visitor) e2);
	    puts post;
	    SkipChildren
      | Cast(ct,t, e1) ->
	  let t1 = Vine_typecheck.infer_type None e1 in
	  let (bits, bits1) = (bits_of_width t, bits_of_width t1) in
	  let (pre,post) = match ct with
	    | CAST_LOW       -> ("(extract["^string_of_int(bits - 1)^":0] ", ")")
	    | CAST_HIGH      ->
		("(extract["^string_of_int(bits1-1)^":"^string_of_int(bits1-bits)^"] ", ")")
	    | CAST_UNSIGNED  ->
		if bits = bits1 then ("","") else
		("(zero_extend["^string_of_int(bits-bits1)^"] ", ")")
	    | CAST_SIGNED    ->
		if bits = bits1 then ("","") else
		("(sign_extend["^string_of_int(bits-bits1)^"] ", ")")

	  in
	  let () = puts pre in
	  ChangeDoChildrenPost(e, fun x-> puts post; x)
      | Unknown s ->
	  puts ("unknown_"^string_of_int unknown_counter^" %"); 
	  puts s;  
	  puts "\n";
	  unknown_counter <- unknown_counter + 1;
	  DoChildren
      | Name _ -> raise (Invalid_argument "Names should be here") 


  method visit_binding _ =
    failwith "should not be visiting bindings..."

end

let get_lval_var = function Temp v | Mem(v,_,_) -> v

(** Print SMT-Lib header using given print function *)
let put_smtlib_header ?(benchmark="Vine_generated") ?(author="Bitblaze") 
    ?(description="") ?(status="sat") ?(difficulty=min_int) 
    ?(category="") ?(logic="QF_AUFBV") puts =

  let description_str = 
    if (description = "") then (
      let buf = Buffer.create 256 in
      Buffer.add_string buf "Using:";
      Array.iter 
	(fun s -> (Buffer.add_string buf) " "; (Buffer.add_string buf) s)
	Sys.argv;
      Buffer.contents buf
    )
    else description
  in

  let category_str = 
    if (category <> "") 
      then sprintf ":category { %s }\n" category
      else ""
  in

  let difficulty_str = 
    if (difficulty <> min_int) 
      then sprintf ":difficulty { %d }\n" difficulty
      else ""
  in

  let header_str = 
    sprintf (
      "(benchmark %s\n\n" ^^
      ":source { By %s. %s }\n\n" ^^
      ":status %s\n" ^^
      "%s%s" ^^
      ":logic %s\n\n"
    )
      benchmark author description_str status difficulty_str category_str logic
  in
  puts header_str

(** Print SMT-Lib variable declarations (extrafuns) using 
      given print function 
*)
let put_smtlib_extrafuns puts var_l = 
  List.iter 
    (fun ((_,_,t) as v) -> 
      puts(sprintf "  :extrafuns ((%s %s))\n" (var2s v) (type2s t))) 
    var_l

(** Print VinE expression in SMT-Lib format using given print function *)
let put_smtlib_assumption puts vis e_orig = 
  let (lets, e) = unletify e_orig in
  let letcount = List.length lets in
    puts "  :assumption ";
    List.iter
      (fun (v', l,e) ->
         let s = "?"^var2s v' in
           vis#extend2 v' s;
           puts ("(let ("^s^" ");
           (match l with
              | Temp _ ->
                  ignore(exp_accept vis e);
              | Mem(v,i,t) ->
                  vis#puts_store v i e
           );
           puts ")\n";
      )
      lets;
    puts "(= bit1\n    ";
    dprintf "exp-accept freevars \n%!";
    ignore(exp_accept (vis :> vine_visitor) e);
    puts "\n)\n";
    puts (String.make letcount ')')

(** Translates an expression into SMT-Lib and feeds it to the 
      given print function.
*)
let put_smtlib puts e_orig =
  let vis = new vine_smtlib_print_visitor puts in
    put_smtlib_header puts;
    dprintf "making declarations\n%!";
    vis#declare_freevars e_orig;
    put_smtlib_assumption puts vis e_orig;
    puts "\n  :formula false\n)\n"

(** Like put_smtlib, but the SMT-Lib code is retured in a string *)
let to_string e =
  let buf = Buffer.create 16 in
  let () = put_smtlib (Buffer.add_string buf) e in
    Buffer.contents buf

(** Like put_smtlib, but the SMT-Lib code is printed to standard out *)
let print = put_smtlib print_string

(** Like put_smtlib, but the SMT-Lib code is written to a channel *)
let to_file fd =
  put_smtlib (output_string fd)

