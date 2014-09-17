(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Translations to STP
    Translation from VinE expressions to STP.
    See the to_string, to_file, and print functions.
 *)

open Vine
open Printf
module VH = Vine.VarHash
module List = ExtList.List 

module D = Debug.Make(struct let name = "STP" and default=`Debug end)
open D

(* Translate a variable to conform to CVC's syntax *)
let transvar s = s

let is_not_memory t = 
  match unwind_type t with
      TMem _ -> false
    | _ -> true


let rec repeat = function 0 -> (fun _ -> ())
  | n -> (fun f -> ( f(); repeat (n-1) f))

(** Visitor for printing out CVCL sytax for an expression. *)
class vine_cvcl_print_visitor puts =
  let rec type2s = function
    | t when is_integer_type t ->
	"BITVECTOR("^string_of_int(Vine.bits_of_width t)^")"
    | Array(t2,i) ->
	"ARRAY BITVECTOR(64) OF "^type2s t2
    | x ->
	failwith("Unsupported type for translation to STP: "^type_to_string x)
	
  in
  let var2s (num,name,_) =
    name^"_"^string_of_int num
  in
object (self)
  inherit nop_vine_visitor
    (* variables already used in this output (needed for mems) *)
  val used_vars = Hashtbl.create 57
    (* map vine var to var we are using *)
  val g = VH.create 57
  val mutable unknown_counter = 0

  method extend2 v s =
    assert(not(Hashtbl.mem used_vars s));
    Hashtbl.add used_vars s ();
    dprintf "Extending %s -> %s" (var2s v) s;
    VH.add g v s
  method unextend v =
    dprintf "Unextending %s" (var2s v);
    VH.remove g v
  method tr_var v =
    let v' =
    try VH.find g v
    with Not_found -> (* free variable *)
      var2s v
    in
      dprintf "Translating %s -> %s" (var2s v) v';
      v'

  method declare_var ((_,_,t) as v) = 
    puts(sprintf "%s : %s;\n" (var2s v) (type2s t))

  method declare_freevars e =
    let () = puts "% free variables: \n" in
    let fvs = get_req_ctx e in 
    let () = dprintf "%d free variables\n%!" (List.length fvs) in 
    let () = List.iter self#declare_var fvs in
      puts "% end free variables.\n\n\n"



  method visit_exp e =
    match e with
	Constant(v) ->
	  (match v with
	      Int(t,i) ->
		let format = match (Vine.unwind_type t) with
		    REG_1  -> format_of_string "0bin%Ld"
		  | REG_8  -> format_of_string "0hex%02Lx"
		  | REG_16 -> format_of_string "0hex%04Lx"
		  | REG_32 -> format_of_string "0hex%08Lx"
		  | REG_64 -> format_of_string "0hex%016Lx"
		  | _ -> 
		      raise (Invalid_argument 
			       "Only constant integers supported")
		in
		let maskedval = Int64.logand i
		  (Int64.pred(Int64.shift_left Int64.one (bits_of_width t)))
		in
		  puts(sprintf format maskedval)
	     | _ -> 
		 raise (Invalid_argument 
			  "Only constant integer types supported")
	  );
	  SkipChildren
      | Lval(Temp v) ->
	  puts(self#tr_var v);
	  SkipChildren
      | Lval(Mem(m, idx,t)) when is_not_memory t ->
	  puts (self#tr_var m^"[");
	  ignore(exp_accept self (Cast(CAST_UNSIGNED,REG_64,idx)));
	  puts "]";
	  SkipChildren
      | Lval(Mem _) ->
	  raise (Invalid_argument "Memory type not handled")

      | Let(Temp v, _, _) ->
	  ChangeDoChildrenPost(e, (fun x -> puts ")";self#unextend v; x))

      | Let(Mem(m,_,t), _, _) when is_not_memory t ->
	  ChangeDoChildrenPost(e, fun x ->
				 self#unextend m;
				 puts ")";
				 x)
      | Let(Mem _,_,_) ->
	  raise (Invalid_argument "Memory not handled in STP")
      | UnOp(uop, _) ->
	  let () = match uop with
	      NEG -> puts "BVUMINUS("
	    | NOT -> puts "~("
	  in
	    ChangeDoChildrenPost(e, fun x ->(puts ")";x))
      | (* Eww, the << operator in stp seems to want a constant int on the right,
	   rather than a bitvector *)
	  BinOp(LSHIFT, e1, Constant(Int(_, i))) ->
	  if i = 0L then let _ = exp_accept (self :> vine_visitor) e1 in SkipChildren (* STP barfs on 0 *)
	  else
	  let  t = Vine_typecheck.infer_type None e1 in
	  let () = puts("((") in
	  let _ = exp_accept (self :> vine_visitor) e1 in
	  let () = puts(" << "^Int64.to_string i^")["^string_of_int(bits_of_width t -1)^":0])")
	  in
	    SkipChildren
      | BinOp(RSHIFT, e1, Constant(Int(_, i))) -> (* Same sort of deal :( *)
	  if i = 0L then let _ = exp_accept (self :> vine_visitor) e1 in SkipChildren (* STP barfs on 0 *)
	  else
	  (* let t = Vine_typecheck.infer_type None e1 in *)
	  let () = puts "(" in
	  let _ = exp_accept (self :> vine_visitor) e1 in
	  let () = puts(" >> "^Int64.to_string i^")")
	  in
	    SkipChildren
      | BinOp(ARSHIFT, e1, Constant(Int(_, i))) -> (* Same sort of deal :( *)
	  if i = 0L then let _ = exp_accept (self :> vine_visitor) e1 in SkipChildren (* STP barfs on 0 *)
	  else
	  let t = Vine_typecheck.infer_type None e1 in
	  let bits = string_of_int (bits_of_width t) in
	  let () = puts "SX(" in
	  let _ = exp_accept (self :> vine_visitor) e1 in
	  let () = puts(" >> "^Int64.to_string i^", "^bits^")")
	  in
	    SkipChildren
      | BinOp(bop, e1, e2) when bop = LSHIFT || bop = RSHIFT || bop = ARSHIFT ->
	  let t2 = Vine_typecheck.infer_type None e2 in
	  let const n = Constant(Int(t2,Int64.of_int n)) in
	  let put_one n = ignore(exp_accept (self :> vine_visitor) (BinOp(bop, e1, const n))) in
	  let rec put_all n =
	    if n < 64 then
	      (puts " IF ";
	       ignore(exp_accept (self :> vine_visitor) e2);
	       puts " = ";
	       ignore(exp_accept (self :> vine_visitor) (const n));
	       puts " THEN ");
	    put_one n;
	    if n < 64 then
	      (puts " ELSE ";
	       put_all (n+1);
	      puts " ENDIF ")
	  in
	    put_all 0; 
	    SkipChildren
      | BinOp(bop, e1, e2) ->
	  let t = Vine_typecheck.infer_type None e1 in
	  let bits = if is_integer_type t then  bits_of_width t else -1 in
	  let sw = string_of_int bits in
	  let (pre,mid,post) = match bop with
	      PLUS     -> ("BVPLUS("^sw^", ", ",", ")")
	    | MINUS    -> ("BVSUB("^sw^", ", ",", ")")
	    | TIMES    -> ("BVMULT("^sw^", ", ",", ")")
	    | DIVIDE   -> ("BVDIV("^sw^", ", ",", ")")
	    | SDIVIDE  -> ("SBVDIV("^sw^", ", ",", ")")
	    | MOD      -> ("BVMOD("^sw^", ", ",", ")")
	    | SMOD     -> ("SBVMOD("^sw^", ", ",", ")")
	    | BITAND   -> ("(", "&", ")")
	    | BITOR    -> ("(", "|", ")")
	    | XOR      -> ("BVXOR(", ",", ")")
	    | EQ       -> ("IF (", "=", ") THEN 0bin1 ELSE 0bin0 ENDIF")
	    | NEQ      -> ("IF (NOT(", "=", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | LT       -> ("IF (BVLT(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | LE       -> ("IF (BVLE(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | SLT       -> ("IF (BVSLT(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | SLE       -> ("IF (BVSLE(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | LSHIFT 
	    | ARSHIFT
	    | RSHIFT ->
		failwith "shifs should have been handled by a different case"
	  in
	  let () = puts pre in
	  let _ = exp_accept (self :> vine_visitor) e1 in
	  let () = puts  mid in
	  let _ =  exp_accept (self :> vine_visitor) e2 in
	  let () = puts post in
	    SkipChildren
      | Cast(ct,t, e1) ->
	  let t1 = Vine_typecheck.infer_type None e1 in
	  let (bits, bits1) = (bits_of_width t, bits_of_width t1) in
	  let (pre,post) = match ct with
	      CAST_SIGNED    -> ("SX(",", "^string_of_int bits^")")
	    | CAST_LOW       -> ("", "["^string_of_int(bits - 1)^":0]")
	    | CAST_HIGH      ->
		("", "["^string_of_int(bits1-1)^":"^string_of_int(bits1-bits)^"]")
	    | CAST_UNSIGNED  ->
		if bits = bits1 then ("","") else
		("(0bin"^String.make (bits-bits1) '0'^" @ ", ")")
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

(*
      | Name of string
      | Phi of string * exp list
      | Let of lvalue * exp * exp
*)

  method visit_binding (l,e1) =
    (* let () = puts"%let "; pp_lval puts l; puts " = "; pp_exp puts e1; puts "\n" in *)
    let () = puts "(LET " in
    let () = match l with
      | Temp v ->
	  (* v isn't allowed to shadow anything *)
	  let s = var2s v in
	  puts s;
	  puts " =\n    ";
	  ignore(exp_accept (self :> vine_visitor) e1);
	  self#extend2 v s
      | Mem(m,addr,t) when is_not_memory t  -> 
	  let oldmem = self#tr_var m in
	    (* m is shadowing the old m *)
	  let newmem = var2s (renewvar m) in
	    puts(newmem^" =\n    ("^oldmem^" WITH [");
	    ignore(exp_accept (self :> vine_visitor) (Cast(CAST_UNSIGNED,REG_64,addr)));
	    puts "] := ";
	    ignore(exp_accept (self :> vine_visitor) e1);
	    puts ")";
	    self#extend2 m newmem
      | Mem _ ->
	  raise (Invalid_argument "Memory type not handled in STP")
    in
    let () = puts "\nIN\n" in
      SkipChildren
 

end

(** Translates an expression into STP and feeds it to the given print function.
    
    The output of this function consists of an ASSERTion that the given
    expression is true.
*)
let put_cvcl puts e =
  let vis = new vine_cvcl_print_visitor puts in
  let () = dprintf "making declarations\n%!" in 
  let () = vis#declare_freevars e in
  let () = puts "ASSERT( 0bin1 =\n" in
  let () = dprintf "exp-accept freevars \n%!" in 
  let _ = exp_accept (vis :> vine_visitor) e in
    puts ");\n"

(** Like put_cvcl, but the STP code is retured in a string *)
let to_string e =
  let buf = Buffer.create 16 in
  let () = put_cvcl (Buffer.add_string buf) e in
    Buffer.contents buf

(** Like put_cvcl, but the STP code is printed to standard out *)
let print = put_cvcl print_string

(** Like put_cvcl, but the STP code is written to a channel *)
let to_file fd =
  put_cvcl (output_string fd)

