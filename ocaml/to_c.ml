(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)


open Vine
open Vine_typecheck
open Printf
open ExtList
module VH = VarHash;;

let find_uninitialized_vars prog =
  let cfg = Vine_cfg.prog_to_cfg prog in
  let live = Vine_dataflow.live_variables ~globals:[] cfg in
    live Vine_cfg.BB_Entry

class pp ?(debug_labels=false) ft =
  (* let varctx = Hashtbl.create 113 in *)
  let varctx = Hashtbl.create 113 in

  let pp = Format.pp_print_string ft
  (* and pp_int = Format.pp_print_int ft *)
  and space = Format.pp_print_space ft
  and cut = Format.pp_print_cut ft
  and open_box = Format.pp_open_hovbox ft
  and open_hvbox = Format.pp_open_hvbox ft
  and break = Format.pp_print_break ft 
  and close_box = Format.pp_close_box ft
  and newline = Format.pp_print_newline ft 
  and pp_int = Format.pp_print_int ft 
  and with_varids = false in 
  let comma () = pp ","; space() in 
object(self)

  (* the prototypes we assume we have for read and write are: *)
  (* uint64_t _mem_read(mem_t m, uint64_t addr, int num_bytes) *)
  (* mem_t _mem_write(mem_t m, uint64_t addr, uint64_t val, int num_bytes) *)

  (* you can extend this class if you want something different *)

  val mutable special = "_special"
  val mutable mem_read = "_mem_read"
  val mutable mem_write = "_mem_write"


 (** Pretty print a program using the given formater *)
  method format_program (dl,sl) =
    pp "#include <vine_helpers.h>\n";
    newline();
    pp "int64_t vine_ir_program() {";
    newline();
    open_box 2;
    open_hvbox 0;
    self#format_decls dl;
    close_box();
    newline();
    open_hvbox 0;
    self#format_initializations (dl,sl);
    close_box();
    newline();
    open_hvbox 0;
    List.iter self#format_stmt sl;
    close_box();
    newline();
    pp ";";
    newline();
    close_box();
    pp "}"

  method format_expstmt e = 
    self#format_exp e;
    pp ";"

  method format_comment s = 
    pp "/*";
    pp s;
    pp "*/"

  method format_block dl sl' = 
    open_hvbox 0;
    pp "{";
    open_hvbox 4;
    space();
    self#format_decls dl;
    List.iter self#format_stmt sl';
    close_box();
    cut();
    pp "}";
    close_box()

  method format_function v ropt al is_ext bopt = (
    if is_ext then (pp "extern"; space());
    (match ropt with
        None -> pp "void"
       | Some(rt') -> self#format_typ rt'
    );
    space();
    pp v;
    (match al with
        [] -> pp "()"
       | x::xs ->
          pp "(";
          self#format_var x;
          List.iter (fun y -> comma(); self#format_decl y) xs;
          pp ")"
    );
    match bopt with
       None -> pp ";"
      | Some(blk) -> 
         space();
         self#format_stmt blk
  )


    
  method format_stmt s = 
    let rec fs s =
      open_box 2; 
      (match s with
          Jmp e -> self#format_jmp e
        | CJmp(e1,e2,e3) -> self#format_cjmp e1 e2 e3
        | Move(l,e) -> self#format_move l e
        | Special s -> self#format_special s 
        | Label l -> self#format_label l
        | ExpStmt e -> self#format_expstmt e
        | Comment s -> self#format_comment s
        | Block(dl,sl') -> self#format_block dl sl'
        | Attr(s,a) -> self#format_attr s a
        | Function(v,ropt,al,is_ext,bopt) -> 
            self#format_function v ropt al is_ext bopt
        | Call (lv_o, e, al) -> self#format_call lv_o e al
        | Return eo ->  self#format_return eo
        | Assert e -> self#format_assert e
        | Halt e -> self#format_halt e
      );
      close_box ();
      space ()
    in
      fs s;

  method format_return e  =
    match e with
	None -> pp "return";
      | Some(e') -> 
	  pp "return";
	  space();
	  self#format_exp e';
	  pp ";"

  method format_initializations ((dl,sl) as prog) =
    let uninit = find_uninitialized_vars prog in
    let init_var ((_,_,t) as v) =
      pp "VINE_INIT_";
      self#format_typ t;
      pp "(";
      self#format_var v;
      pp ");";
      newline();
    in
      List.iter init_var uninit
    


  method format_exp e =
    let rec fe prec e =
      (* prec tells us how much parenthization we need. 0 means it doesn't need
	 to be parenthesized. Larger numbers means it has higher precedence.
	 Maximum prec before paretheses are added are as follows:
	 100 OR
	 200 XOR
	 300 AND
	 400 EQUAL NEQ
	 500 LT SLT SLE LE
	 600 LSHIFT RSHIFT ARSHIFT
	 700 PLUS MINUS
	 800 TIMES DIVIDE SDIVIDE MOD
	 900 UMINUS NOT
	 (* To avoid gcc -Wall warnings, we will artificially require
	 operands to | ^ and & to have precedency at least 750 *)
      *)
      let lparen bind = if bind < prec then pp "(" in
      let rparen bind = if bind < prec then pp ")" in
	open_box 0;
	(match e with
	     Let _ ->
	       raise (Invalid_argument "Let's not supported. Try calling unlet first.")
	   | BinOp(b,e1,e2) ->
	       let op_prec = match b with
		 | BITOR                          -> 100
		 | XOR                            -> 200
		 | BITAND	                  -> 300
		 | EQ | NEQ			  -> 400
		 | LT | SLT | SLE | LE	          -> 500
		 | LSHIFT | RSHIFT | ARSHIFT      -> 600
		 | PLUS | MINUS		          -> 700
		 | TIMES|DIVIDE|SDIVIDE|MOD|SMOD  -> 800
	       in
	       let child_prec = if op_prec <= 300 then 750 else op_prec in
		 lparen op_prec;
		 open_box 0;
		 (* all our binops are left associative *)
		 fe child_prec e1;
		 space();
		 pp (self#binop_to_string b);
		 pp " ";
		 fe (child_prec+1) e2;
		 close_box();
		 rparen op_prec;
		 cut()
	   | UnOp(u,e) ->
	       lparen 900;
	       pp(unop_to_string u);
	       fe 900 e;
	       rparen 900
	   | Constant(v) ->
	       self#format_value v
	   | Lval l ->
	       self#format_lval l
	   | Name l ->
	       pp "name("; pp l; pp ")"
	   | Cast(ct,t,e) -> self#format_cast fe ct t e 
	   | Unknown u ->
	       pp "unknown \""; pp u; pp "\""
	);
	close_box()
    in
      fe 0 e





  method pp_label_debug l = 
    if debug_labels then (
      open_box 0;
      pp "print_label(\""; pp l; pp "\");";
      close_box ()
    )

  method mem_read  lv = 
    match lv with
	Temp _ -> raise (Invalid_argument "cannot read from a scalar")
      | Mem(v,e,t) ->
	  open_hvbox 0;
	  pp "("; self#format_typ t; pp ")"; space(); 
	  pp mem_read; pp "(";
	  self#format_name v; pp ",";
	  self#format_exp  e; pp ",";
	  pp "sizeof("; self#format_typ  t; pp ")";
	  pp ")";
	  close_box ()

  method mem_write  lv e = 
    match lv with
	Temp _ -> raise (Invalid_argument "cannot write to a scalar")
      | Mem(v,addr,t) ->
	  open_hvbox 0;
	  self#format_var v; pp " =";
	  pp mem_write; pp "(";
	  self#format_var v; pp ",";    (* memory *)
	  self#format_exp addr; pp ","; (* addr *)
	  self#format_exp  e; pp ",";    (* val *)
	  pp "sizeof("; self#format_typ t; pp ")"; (* num bytes *)
	  pp ");";
	  close_box ()



  method format_var ?(print_type=false) ((_,_,typ) as var) =
    self#format_name var;

(* FIXME: format_exp does precedence wrong *)

  method binop_to_string op = 
    match op with
      | SDIVIDE ->  "/ (int64_t)" 
      | SMOD -> "% (int64_t)"
      | SLT -> "< (int64_t)"
      | SLE -> "<= (int64_t)"
      | RSHIFT -> ">> (uint64_t)"
      | ARSHIFT -> ">> (int64_t)"
      | NEQ -> "!="
      | _ -> ( binop_to_string op)


  method format_typ = function
      REG_1 -> pp "bool"
    | REG_8 -> pp "uint8_t"
    | REG_16 -> pp "uint16_t"
    | REG_32 -> pp "uint32_t"
    | REG_64 -> pp "uint64_t"
    | TMem(REG_32, Little) -> pp "mem_little32_t"
    | TMem(REG_64, Little) -> pp "mem_little64_t"
    | Array(t2, i) -> pp "mem_norm_t" (* array is normalized memory *) 
    | TAttr(t', _) -> self#format_typ t'
    | _ -> raise (Invalid_argument "Unsupported type")


  method format_decl v = 
    self#format_decls [v]

  method format_name ((vid,name,typ) as var) =
    if with_varids then (
      pp name;
      pp "_";
      pp_int vid )
    else
      let strings =
	Stream.from (function
		       | 0 -> Some name
		       | 1 -> Some(name^"_"^string_of_int vid)
		       | 2 -> Some(name^"__"^string_of_int vid)
		       | n -> Some(name^"_"^string_of_int (n-2))
		    )
      in
      let rec pp_next() =
	let s = Stream.next strings in
	  try
	    let var' = Hashtbl.find varctx s in
	      if var == var' then pp s else
		pp_next()
	  with Not_found ->
	    Hashtbl.add varctx s var;
	    pp s
      in
	pp_next()


  method format_decls  dl = 
    let name_sort (_,n1,_) (_,n2,_) = String.compare n1 n2  in
    let idx_sort (i1,_,_) (i2,_,_) = compare i1 i2 in 
    let ctx = Hashtbl.create 19009 in 
      (* map type -> list of variables with that type *)
    let () = List.iter (fun ((_,_,t) as v) -> 
			  if Hashtbl.mem ctx t then
			    Hashtbl.replace ctx t (v::(Hashtbl.find ctx t))
			  else
			    Hashtbl.add ctx t [v]) dl in
    let rec pd vars =  (
      match vars with
	  [] -> () 
	| x::[] -> 	    
	    self#format_var x; 
	    pp ";"
	| x::ys -> 
	    self#format_var  x; pp ","; break 1 0;
	    pd ys
    ) in
    let keys = Hashtbl.fold (fun k _ acc -> k::acc) ctx [] in 
    let ints,others = List.partition (is_integer_type) keys in 
    let ints = List.stable_sort 
      (fun t1 t2 ->
	 match t1,t2 with
	     x,y when is_integer_type t1 && is_integer_type t2 ->
	       compare (bits_of_width t1) (bits_of_width t2)
	   | _ -> -1) ints in
    let keys = List.append ints others in
      List.iter 
	(fun typ ->
	   let vars = Hashtbl.find ctx typ in 
	   let sorted_vars = List.stable_sort  
	     (fun v1 v2 -> 
		if (name_sort v1 v2) <> 0 then 
		  name_sort v1 v2 
		else
		  idx_sort v1 v2) vars in
	     match vars with
		 [] -> ()
	       | _ -> 
		   Format.pp_open_vbox ft 0;
		   Format.pp_open_hbox ft ();
		   self#format_typ  typ;
		   space ();
		   close_box ();
		   Format.pp_open_hovbox ft 0;
		   pd sorted_vars;
		   close_box ();
		   break 0 0;
		   close_box ()
	) keys 


  method format_lval lv = 
      match lv with
	  Temp(v) -> self#format_var v
	| Mem(v,e,t) -> self#mem_read  lv


  method format_value = function
    | Int(REG_1, 1L) -> pp "1"
    | Int(REG_1, 0L) -> pp "0"
    | Int(t,x) -> 
	(* No moding here, since the value should have been modded earlier,
	   and if it wasn't, it's better not to hide it. *)
	pp (if 0L <= x && x < 10L
	    then Int64.to_string x
	    else Printf.sprintf "0x%LxLL" x)
    | Str(x) -> pp "\""; pp x; pp "\""	  


  method format_cast fe  ct t e = 
    let to_signed_type_string t = 
      match t with
	  REG_1 -> "bool"
	| REG_8 -> "int8_t"
	| REG_16 -> "int16_t"
	| REG_32 -> "int32_t"
	| REG_64 -> "int64_t"
	| _ -> raise (Invalid_argument "Invalid integral type")
    in
    let () = open_box 2 in (
	match ct with
	    CAST_LOW 
	  | CAST_UNSIGNED ->
	      (* this avoids warnings in visual studio 2005 *)
	      (* FIXME: but it's wrong! *)
	      if t = REG_1 && ct = CAST_LOW then (
		pp "(((";  self#format_exp e; 
		pp ")&1) == 1)"
	      ) else (
		pp "("; self#format_typ  t; pp ")"; cut(); pp "(";
		self#format_exp  e; pp ")"
	      )
		
	  | CAST_SIGNED when is_integer_type t -> 
	      let tstr = to_signed_type_string t in
		pp "("; pp tstr; pp ")"; cut(); pp "(";
		self#format_exp  e; pp ")"
	  | CAST_HIGH  when is_integer_type t ->
	      let dstlen = bits_of_width t in 
	      let srctyp = Vine_typecheck.infer_type None e in 
	      let srclen = bits_of_width srctyp in 
	      let offset = srclen - dstlen in 
	      let (mask:int64) = Int64.sub 
		(Int64.shift_left 1L dstlen)
		(Int64.one) in 
		pp "("; self#format_typ t; pp ")"; space();
		pp "("; space () ; pp "(";
		pp "("; self#format_exp e; pp ")";
		pp ">>"; pp (string_of_int offset); space ();
		pp ")&"; Format.fprintf ft "0x%LX" mask;
		(* pp (string_of_int mask); *)
		pp ")"
	  | CAST_HIGH 
	  | CAST_SIGNED -> raise (Invalid_argument "Unhandled case")
      ); close_box ()

  method format_jmp e = 
    match e with
	Name(l) -> pp "goto"; space (); pp l; pp ";"
      | _ -> raise (Invalid_argument "Indirect jump not supported")
	  
  method format_cjmp e e1 e2 = 
    match e1,e2 with
	Name(l1),Name(l2) -> 
	  pp "if("; self#format_exp e; pp ")"; space ();
	  pp "goto"; space (); pp l1; pp ";"; space();
	  pp "goto"; space (); pp l2; pp ";"
      | _,_ -> raise (Invalid_argument "Indirect cjmp target not supported")

  method format_move l e = 
    match l with
	Temp _ ->
	  self#format_lval l;
	  pp " ="; space();
	  self#format_exp e;
	  pp ";"
      | Mem _ as mem ->
	     self#mem_write mem e

  method format_special s = 
    pp special;
    pp "(\"";
    pp s;
    pp "\");"

  method format_label l = 
    pp l;
    pp ":";
    self#pp_label_debug l


  method format_attr s a = 
    self#format_stmt (Comment("Statement attribute "^(attr_to_string a)));
    self#format_stmt s

  method format_call lv_o e al = 
    match e with
	Name(l) -> 
	  (match lv_o with
	     | None -> ()
	     | Some lv ->
		 self#format_lval lv; pp " ="; space()
	  );
	  pp l;
	  pp "(";
	  ignore(List.fold_left
		   (fun c a ->
		      self#format_exp a; (if c then comma());true)
 		   false al);
	  pp ");"
      | _ -> raise (Invalid_argument "Indirect call not supported")



  method format_assert e = 
    pp "assert(";
    self#format_exp e;
    pp ");"

  method format_halt e = 
    pp "exit(";
    self#format_exp e;
    pp ");"

end


let unlet (dl,sl) = 
  let  unlet_vis =
object(self)
  inherit nop_vine_visitor
    
  val mutable new_decls = []
  val mutable new_stmts = []
  val ctx = VH.create 57

  method visit_rlvalue lv = 
    try
      match lv with
	| Temp(v) -> ChangeTo(Temp(VH.find ctx v))
	| Mem(v,e,t) -> let v' = VH.find ctx v in
	    ChangeTo(Mem(v', (exp_accept self e), t)) 
    with Not_found -> DoChildren
      
  method visit_exp = function
    | Let(l,e1,e2) -> (* visit binding will be called in post *)
	let v = match l with Temp(v) -> v | Mem(v,_,_) -> v in 
	let v' = renewvar v in
	let e1' = exp_accept self e1 in
	let () = VH.add ctx v v' in
	let e2' = exp_accept self e2 in
	  VH.remove ctx v;
	  new_decls <- v'::new_decls;
	  new_stmts <- (match l with
			  | Temp(v) ->
			      Move(Temp v', e1') :: new_stmts
			  | Mem(v,addr,t) ->
			      let addr' = exp_accept self addr in
			      Move(Temp v', Lval(Temp v))
			      :: Move(Mem(v',addr',t), e1')
			      :: new_stmts
		       );
	  ChangeTo e2';
    | _ -> DoChildren

  method visit_stmt s =
    assert(new_decls == [] && new_stmts == []);
    ChangeDoChildrenPost(s, 
		   fun s ->
		     match (new_decls, new_stmts) with
		       | ([],[]) -> s
		       | (dl,sl) -> 
			   new_decls <- [];
			   new_stmts <- [];
			   Block(dl, sl@[s])
		  )
end
  in
  let p' =  prog_accept unlet_vis (dl,sl) in 
    Vine_alphavary.descope_program p'
