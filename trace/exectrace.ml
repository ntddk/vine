(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(**
   Functions for processing execution traces.
*)

open Vine
module List = ExtList.List;;
module String = ExtString.String;;
module Trace = Temu_trace;;
module D = Debug.Make(struct let name = "exectrace" and default=`Debug end) 

exception Found

let rvars_of_stmt s =
  let vis = object(self)
    inherit nop_vine_visitor

    val mutable rvars = VarSet.empty
    method get_rvars = rvars

    method visit_rlvalue lv =
      let v = 
        match lv with
        | Temp(v) -> v
        | Mem(v,_,_) -> v
      in
      rvars <- VarSet.add v rvars;
      DoChildren
  end
  in
  let _ = stmt_accept vis s in
  vis#get_rvars

let s_exists pred s =
  let vis = 
object (self)
  inherit nop_vine_visitor
   
  method visit_stmt s =
    if pred s then
      raise Found
    else
      DoChildren
end
  in
  try
    let _ = Vine.stmt_accept vis s in
    false
  with 
    Found -> true

let slist_exists pred sl =
  List.exists (s_exists pred) sl

let varset_of_prog prog = 
  let vis = 
object (self)
  inherit nop_vine_visitor
  val mutable varset = VarSet.empty
  method get_varset () = varset

  method visit_stmt s =
    (match s with
     | Block(dl, _) ->
         List.iter 
           (fun v -> varset <- VarSet.add v varset)
           dl
     | _ -> ());
    DoChildren
end
  in
  let (dl, sl) = prog in
  List.iter (fun s -> let _ = Vine.stmt_accept vis s in ()) sl;
  let ds = vis#get_varset () in
  let ds = List.fold_left (fun ds d -> VarSet.add d ds) ds dl in
  ds
;;

let gamma_of_varset mem vs =
  let dl = VarSet.elements vs in
  let gamma = Asmir.gamma_create mem dl in
  gamma
;;

(* gets rid of blocks, raising decls to global scope.
   fails if variable is decl'd in multiple scopes.
*)
let rec flatten_blocks (dl,sl) =
  let rec foo dset sl_rev sl =
    match sl with
    | [] -> 
        (VarSet.elements dset, List.rev sl_rev)
    | Block(dl,sl)::tl -> 
        let (dl,sl) = flatten_blocks (dl,sl) in
        let dset = 
          List.fold_left 
            (fun dset d -> 
               assert(not (VarSet.mem d dset));
               VarSet.add d dset) 
            dset 
            dl
        in
        foo dset (List.rev_append sl sl_rev) tl
    | s::tl ->
        foo dset (s::sl_rev) tl
  in
  let dset = VarSet.empty in
  let dset = List.fold_left
    (fun dset d -> VarSet.add d dset)
    dset
    dl
  in
  foo dset [] sl
;;

(** @param vine expression, which should evaluate to an integer
    @return resulting int64
    XXX: consider adding this as a utility somewhere in vine
*)
let eval_const_exp expr =
  let folded = 
    Vine_opt.simplify
      expr
  in
  match folded with
  | Constant(Int(_,i)) -> i
  | _ -> failwith "eval_const_exp: expected a constant int"
;;

module Reg_x86 =
  (** utility functions for reading and writing x86 regs *)

struct
  type regid =
      (* segment registers *)
      Reg_es
    | Reg_cs 
    | Reg_ss 
    | Reg_ds 
    | Reg_fs 
    | Reg_gs 
        (* address-modifier dependent registers *)
    | Reg_eAX 
    | Reg_eCX
    | Reg_eDX
    | Reg_eBX
    | Reg_eSP
    | Reg_eBP
    | Reg_eSI
    | Reg_eDI
        (* 8-bit registers *)
    | Reg_al
    | Reg_cl
    | Reg_dl
    | Reg_bl
    | Reg_ah
    | Reg_ch
    | Reg_dh
    | Reg_bh
        (* 16-bit registers *)
    | Reg_ax
    | Reg_cx
    | Reg_dx
    | Reg_bx
    | Reg_sp
    | Reg_bp
    | Reg_si
    | Reg_di
        (* 32-bit registers *)
    | Reg_eax
    | Reg_ecx
    | Reg_edx
    | Reg_ebx
    | Reg_esp
    | Reg_ebp
    | Reg_esi
    | Reg_edi
        (* ??? *)
    | Reg_indir_dx

  let regid_of_addr x = 
    match x with
      (* segment registers *)
    | 100L -> Reg_es
    | 101L -> Reg_cs
    | 102L -> Reg_ss
    | 103L -> Reg_ds
    | 104L -> Reg_fs
    | 105L -> Reg_gs
        (* address-modifier dependent registers *)
    | 108L -> Reg_eAX
    | 109L -> Reg_eCX
    | 110L -> Reg_eDX
    | 111L -> Reg_eBX
    | 112L -> Reg_eSP
    | 113L -> Reg_eBP
    | 114L -> Reg_eSI
    | 115L -> Reg_eDI
        (* 8-bit registers *)
    | 116L -> Reg_al
    | 117L -> Reg_cl
    | 118L -> Reg_dl
    | 119L -> Reg_bl
    | 120L -> Reg_ah
    | 121L -> Reg_ch
    | 122L -> Reg_dh
    | 123L -> Reg_bh
        (* 16-bit registers *)
    | 124L -> Reg_ax
    | 125L -> Reg_cx
    | 126L -> Reg_dx
    | 127L -> Reg_bx
    | 128L -> Reg_sp
    | 129L -> Reg_bp
    | 130L -> Reg_si
    | 131L -> Reg_di
        (* 32-bit registers *)
    | 132L -> Reg_eax
    | 133L -> Reg_ecx
    | 134L -> Reg_edx
    | 135L -> Reg_ebx
    | 136L -> Reg_esp
    | 137L -> Reg_ebp
    | 138L -> Reg_esi
    | 139L -> Reg_edi
        (* ??? *)
    | 150L -> Reg_indir_dx
    | _ -> raise (Invalid_argument (Int64.to_string x))

  let regid_to_str x = 
    match x with
      (* segment registers *)
    | Reg_es -> "ES"
    | Reg_cs -> "CS"
    | Reg_ss -> "SS"
    | Reg_ds -> "DS"
    | Reg_fs -> "FS"
    | Reg_gs -> "GS"
        (* address-modifier dependent registers *)
    | Reg_eAX -> "*eAX*"
    | Reg_eCX -> "*eCX*"
    | Reg_eDX -> "*eDX*"
    | Reg_eBX -> "*eBX*"
    | Reg_eSP -> "*eSP*"
    | Reg_eBP -> "*eBP*"
    | Reg_eSI -> "*eSI*"
    | Reg_eDI -> "*eDI*"
        (* 8-bit registers *)
    | Reg_al -> "AL"
    | Reg_cl -> "CL"
    | Reg_dl -> "DL"
    | Reg_bl -> "BL"
    | Reg_ah -> "AH"
    | Reg_ch -> "CH"
    | Reg_dh -> "DH"
    | Reg_bh -> "BH"
        (* 16-bit registers *)
    | Reg_ax -> "AX"
    | Reg_cx -> "CX"
    | Reg_dx -> "DX"
    | Reg_bx -> "BX"
    | Reg_sp -> "SP"
    | Reg_bp -> "BP"
    | Reg_si -> "SI"
    | Reg_di -> "DI"
        (* 32-bit registers *)
    | Reg_eax -> "EAX"
    | Reg_ecx -> "ECX"
    | Reg_edx -> "EDX"
    | Reg_ebx -> "EBX"
    | Reg_esp -> "ESP"
    | Reg_ebp -> "EBP"
    | Reg_esi -> "ESI"
    | Reg_edi -> "EDI"
        (* ??? *)
    | Reg_indir_dx -> raise (Vine.Unimplemented "indir_dx")

  let regid_to_full x = 
    match x with
      (* segment registers *)
    | Reg_es -> Reg_es
    | Reg_cs -> Reg_cs
    | Reg_ss -> Reg_ss
    | Reg_ds -> Reg_ds
    | Reg_fs -> Reg_fs
    | Reg_gs -> Reg_gs
        (* address-modifier dependent registers *)
    | Reg_eAX 
    | Reg_eCX 
    | Reg_eDX 
    | Reg_eBX 
    | Reg_eSP 
    | Reg_eBP
    | Reg_eSI
    | Reg_eDI -> raise (Vine.Unimplemented "address-modifier dependent reg")
    | Reg_al
    | Reg_ah
    | Reg_ax
    | Reg_eax -> Reg_eax
    | Reg_cl
    | Reg_ch
    | Reg_cx
    | Reg_ecx -> Reg_ecx
    | Reg_dl
    | Reg_dh
    | Reg_dx
    | Reg_edx -> Reg_edx
    | Reg_bl
    | Reg_bh
    | Reg_bx
    | Reg_ebx -> Reg_ebx
    | Reg_sp
    | Reg_esp -> Reg_esp
    | Reg_bp 
    | Reg_ebp -> Reg_ebp
    | Reg_si 
    | Reg_esi -> Reg_esi
    | Reg_di 
    | Reg_edi -> Reg_edi
        (* ??? *)
    | Reg_indir_dx -> raise (Vine.Unimplemented "indir_dx")

  let regid_to_write_mask x =
    match x with
      (* 8 bit low *)
    | Reg_al 
    | Reg_cl
    | Reg_dl
    | Reg_bl-> 0xFFFFFF00L
        (* 8 bit high *)
    | Reg_ah
    | Reg_ch
    | Reg_dh
    | Reg_bh -> 0xFFFF00FFL
        (* 16 bit *)
    | Reg_ax
    | Reg_cx
    | Reg_dx
    | Reg_bx
    | Reg_sp
    | Reg_bp
    | Reg_si
    | Reg_di
        (* segment regs (also 16 bit) *)
    | Reg_es
    | Reg_cs
    | Reg_ss
    | Reg_ds
    | Reg_fs
    | Reg_gs -> 0x0000L
        (* 32 bit *)
    | Reg_eax
    | Reg_ecx
    | Reg_edx
    | Reg_ebx
    | Reg_esp
    | Reg_ebp
    | Reg_esi
    | Reg_edi -> 0L
    | _ -> raise (Vine.Unimplemented (regid_to_str x))

  let regid_to_read_mask x =
    match x with
      (* special case for segment regs, which are 16 bit *)
    | Reg_es
    | Reg_cs
    | Reg_ss
    | Reg_ds
    | Reg_fs
    | Reg_gs -> 0xFFFFL
    | _ -> 
        Int64.logand 
          (0x00000000ffffffffL)
          (Int64.lognot (regid_to_write_mask x))

  let regid_to_type x =   
    match x with
      (* 8 bit low *)
    | Reg_al 
    | Reg_cl
    | Reg_dl
    | Reg_bl
        (* 8 bit high *)
    | Reg_ah
    | Reg_ch
    | Reg_dh
    | Reg_bh -> Vine.REG_8
        (* 16 bit *)
    | Reg_ax
    | Reg_cx
    | Reg_dx
    | Reg_bx
    | Reg_sp
    | Reg_bp
    | Reg_si
    | Reg_di
        (* segment regs (also 16 bit) *)
    | Reg_es
    | Reg_cs
    | Reg_ss
    | Reg_ds
    | Reg_fs
    | Reg_gs -> Vine.REG_16
        (* 32 bit *)
    | Reg_eax
    | Reg_ecx
    | Reg_edx
    | Reg_ebx
    | Reg_esp
    | Reg_ebp
    | Reg_esi
    | Reg_edi -> Vine.REG_32
    | _ -> raise (Vine.Unimplemented (regid_to_str x))

  let canon_reg_offset regid offset =
    let fullreg = regid_to_full regid in
    let offset = 
      match regid with
      | Reg_ah
      | Reg_ch
      | Reg_dh
      | Reg_bh -> offset + 1
      | _ -> offset
    in
    fullreg, offset

  let write_reg gamma regid rval offset =
    let fullid = regid_to_full regid in
    let fullname = "R_" ^ regid_to_str fullid in
    let fullt = regid_to_type fullid in
    let lhs = Temp(Asmir.gamma_lookup gamma fullname) in

    (* give rval correct type *)
    let rvalt = Vine_typecheck.infer_type None rval in
    let cast_rval = 
      if (Vine.bits_of_width rvalt) < (Vine.bits_of_width fullt) then
        (* widen *)
        Vine.Cast(Vine.CAST_UNSIGNED,
                  fullt,
                  rval)
      else if (Vine.bits_of_width rvalt) > (Vine.bits_of_width fullt) then
        (* narrow *)
        Vine.Cast(Vine.CAST_LOW,
                  fullt,
                  rval)
      else
        rval
    in

    (* shift rval for .h registers *)
    let shift_amt = 
      (+)
        (match regid with
         | Reg_ah | Reg_ch | Reg_dh | Reg_bh -> 8
         | _ -> 0)
        (if offset >= 0 then
           offset * 8
         else
           0)
    in

    let shifted_rval =
      if shift_amt = 0 then
        cast_rval
      else
        (*
          match rval with 
          | Vine.Constant(ctyp, cval) ->
          let newval = match cval with
          | Vine.Int(i) -> Vine.Int(Int64.shift_left i shift_amt)
          | Vine.Str(s) -> raise (Invalid_argument "string constant")
          in
          Vine.Constant(ctyp, newval)
          | _ -> *)
        (* skipping above optimization- should use generic simplifier *)
        BinOp(LSHIFT, 
              cast_rval, 
              Constant(Int(REG_8,Int64.of_int shift_amt)))
    in

    (* set up mask *)
    let mask = 
      if offset < 0 then
        (* overwriting whole (sub)-register *)
        regid_to_write_mask regid 
      else
        (* overwriting one byte *)
        eval_const_exp(
          UnOp(NOT,
               BinOp(LSHIFT,
                     (const_of_int fullt 0xff),
                     (const_of_int fullt
                        (match regid with
                         | Reg_ah | Reg_ch | Reg_dh | Reg_bh 
                             -> offset * 8 + 8
                         | _ -> offset * 8))
                    )
              )
        )
    in

    (* lhs = (reg & mask) | (val & ~mask) *)
    let final_rval =
      if mask <> 0L then
        BinOp(BITOR,
              BinOp(BITAND,
                    Lval(Temp(Asmir.gamma_lookup gamma fullname)),
                    Constant(Int(fullt, mask))
                   ),
              BinOp(BITAND,
                    shifted_rval,
                    (const_of_int64
                       fullt
                       (eval_const_exp
                          (UnOp(NOT,
                                const_of_int64 fullt mask))))
                   )
             )
      else
        shifted_rval
    in
    Vine.Move(lhs, final_rval)

  let read_reg gamma regid =
    let fullreg = regid_to_full regid in
    let fullname = "R_" ^ regid_to_str fullreg in
    let fullt = regid_to_type fullreg in
    let mask = regid_to_read_mask regid in

    let masked_val =
      Vine.BinOp(Vine.BITAND,
                 Vine.Lval(Temp(Asmir.gamma_lookup gamma fullname)),
                 Vine.Constant(Vine.Int(fullt, mask)))
    in
    let shifted_val =
      match regid with
      | Reg_ah | Reg_ch | Reg_dh | Reg_bh 
          -> Vine.BinOp(Vine.RSHIFT,
                        masked_val,
                        Vine.Constant( Vine.Int(Vine.REG_8,8L)))
      | _ -> masked_val
    in
    shifted_val

  let read_reg_byte gamma regid offset =
    Vine.Cast(Vine.CAST_LOW,
              Vine.REG_8,
              Vine.BinOp(Vine.RSHIFT,
                         read_reg gamma regid,
                         Vine.Constant( 
                                       Vine.Int(Vine.REG_8,Int64.of_int (offset * 8)))))

  let opcode rawbytes =
    let rec _opcode rawbytes idx =
      match (int_of_char rawbytes.(idx)) with
      | 0xf0 | 0xf2 | 0xf3 (* lock or rep prefix *)
      | 0x2e | 0x36 | 0x3e | 0x26 | 0x64 | 0x65 (* sgmt override
                                                   prefix *)
      | 0x66 | 0x67 (* opsize prefix *) 
          ->
          _opcode rawbytes (idx+1) (* skip prefix *)
      | _ ->
          int_of_char rawbytes.(idx),
          (if ((int_of_char rawbytes.(idx)) = 0x0f)  then
             int_of_char rawbytes.(idx+1)
           else
             ((int_of_char rawbytes.(idx+1)) lsr 3) land 7)
    in
    _opcode rawbytes 0

  let is_call rawbytes = 
    match opcode rawbytes with
    | 0xe8, _
    | 0xff, 2
    | 0x9a, _
    | 0xff, 3
        -> true
    | _ -> false

  let is_ret rawbytes =
    match opcode rawbytes with
    | 0xc3, _
    | 0xcb, _ 
    | 0xc2, _
    | 0xca, _ ->
        true
    | _ -> false

  let uses_esp rawbytes =
    match opcode rawbytes with
    | 0xe8, _ (* call variations *)
    | 0xff, 2
    | 0x9a, _
    | 0xff, 3
    | 0xc3, _ (* ret variations *)
    | 0xcb, _
    | 0xc2, _
    | 0xca, _
    | 0xff, 6 (* push *)
    | 0x50, _
    | 0x6a, _
    | 0x68, _
    | 0x0e, _
    | 0x16, _
    | 0x1e, _
    | 0x06, _
    | 0x0f, 0xa0
    | 0x0f, 0xa8 
    | 0x60, _ (* pusha *)
    | 0x9c, _ (* pushf *)
    | 0x8f, 0 (* pop *)
    | 0x58, _
    | 0x1f, _
    | 0x07, _
    | 0x17, _
    | 0x0f, 0xa1
    | 0x0f, 0xa9
    | 0x61, _ (* popa *)
    | 0x9d, _ (* popf *)
    | 0x0f, 0x34 (* sysenter *)
    | 0x0f, 0x35 (* sysexit *)
    | 0xc8, _ (* enter *)
    | 0xc9, _ (* leave *)
    | 0xcc, _ (* int *)
    | 0xcd, _
    | 0xce, _
    | 0xcf, _ (* iret *)
        -> true
    | _ -> false
                
end;;
open Reg_x86

module Opval =
  (** Utility functions for operating on opvals from TEMU trace *)
struct
  let vine_t opval =
    match opval#optype with
    | Trace.TRegister ->
        regid_to_type 
          (regid_of_addr opval#opaddr)
    | Trace.TMemLoc ->
        (match opval#oplen with
         | 1 -> REG_8
         | 2 -> REG_16
         | 4 -> REG_32
         | 8 -> REG_64
         | _ -> Printf.printf "oplen %d\n" opval#oplen;
             raise (Invalid_argument "Opval.vine_t"))
    | _ -> raise (Invalid_argument "Opval.vine_t")


  let byte_foldleft (f:'a -> Trace.operand -> int -> 'a) opval acc_init=
    let num_bytes =
      match opval#optype with
      | Trace.TRegister -> 
          (Vine.bits_of_width 
             (regid_to_type 
                (regid_of_addr opval#opaddr))) / 8
      | Trace.TMemLoc -> opval#oplen
      | _ -> raise (Invalid_argument "Opval.byte_foldleft")
    in
    
    let rec loop offset acc =
      if (offset = num_bytes) then
        acc
      else
        loop 
          (offset+1)
          (f acc opval offset)
    in
    loop 0 acc_init

  let byte_tainted opval offset =
    (Int64.logand 
       (Int64.shift_right_logical opval#taintflag offset)
       1L)
    = 1L
  
  let byte_map (f:Trace.operand -> int -> 'a) opval =
    byte_foldleft
      (fun l opval offset ->
         (f opval offset)::l)
      opval
      []

  let byte_conc_val opval offset =
    Trace.int64_of_uint32
      (Int32.logand
         (Int32.shift_right_logical opval#opvalue (offset * 8))
         0xffl)

  let byte_exp gamma opval offset : exp =
    match opval#optype with
    | Trace.TRegister ->
        read_reg_byte gamma (regid_of_addr opval#opaddr) offset
    | Trace.TMemLoc ->
        let addr = 
          Vine.Constant(
                        Int(addr_t,Int64.add 
                              opval#opaddr 
                              (Int64.of_int offset))) in
        Lval(Mem(Asmir.gamma_lookup gamma "$mem", addr, REG_8))
    | _ -> raise (Invalid_argument "opval_byte_exp")
        
  let exp gamma opval =
    match opval#optype with
    | Trace.TRegister ->
        read_reg gamma (regid_of_addr opval#opaddr)
    | Trace.TMemLoc ->
        let addr = const_of_int64 addr_t opval#opaddr in
        let t = vine_t opval in
        Lval(Mem(Asmir.gamma_lookup gamma "$mem", addr, t))
    | _ -> raise (Invalid_argument "exp")

  let const opval =
    const_of_int64 (vine_t opval) (Trace.int64_of_uint32 opval#opvalue)

  let byte_mov gamma opval offset rval_byte =
    match opval#optype with
    | Trace.TRegister ->
        write_reg gamma (regid_of_addr opval#opaddr) rval_byte offset
    | Trace.TMemLoc ->
        let addr = 
          Vine.Constant(
                        Int(addr_t,Int64.add 
                              opval#opaddr 
                              (Int64.of_int offset))) in
        let lval = Mem(Asmir.gamma_lookup gamma "$mem", addr, REG_8) in
        Move(lval, rval_byte)
    | _ -> raise (Invalid_argument "opval_byte_mov")

  let mov gamma opval rhs =
    match opval#optype with
    | Trace.TRegister -> 
        let regid = regid_of_addr opval#opaddr in
        write_reg
          gamma
          regid
          rhs
          (-1)
    | Trace.TMemLoc ->
        Move(
          Mem(Asmir.gamma_lookup gamma "$mem", 
              const_of_int64 Vine.addr_t (opval#opaddr), 
              vine_t opval),
          rhs)
    | _ -> raise (Invalid_argument "Opval.mov")
          
  let to_string opval =
    match opval#optype with
    | Trace.TRegister ->
        let regid = regid_of_addr opval#opaddr in
        regid_to_str regid
    | Trace.TMemLoc ->
        Printf.sprintf 
          "mem[%Ld]"
          opval#opaddr
    | Trace.TMemAddress ->
        Printf.sprintf 
          "~mem[%Ld]"
          opval#opaddr

    | _ -> raise (Invalid_argument "Opval.to_string")

end;;

let val_to_const v =
  match v with
  | Vine_ceval.Int(t, v) ->
      Constant( Int(t,v))
  | _ -> failwith "Expected an int val"

type halt_t = NormalExit | MissingLabel of string | NoMoreStmts
let execute_prog_cb 
    (prog:program) 
    (cb:Vine_ceval.concrete_evaluator -> 'a -> 'a) 
    (acc:'a) : 'a * halt_t =

  let evaluator = new Vine_ceval.concrete_evaluator prog in

  let rec step acc =
    if evaluator#is_halted () then
      acc, NormalExit
    else
      let ecode = evaluator#get_ecode () in

      let stmt_o = 
        try
          Some(Vine_eval.pc_to_stmt ecode (evaluator#get_pc ()))
        with
          Not_found -> None
      in

      match stmt_o with
      | None -> acc, NoMoreStmts
      | Some _ ->
          let acc = cb evaluator acc in
          let missing_lbl =
            try
              let _ = evaluator#step () in
              None
            with
              Vine_eval.NoSuchLabel l -> Some(l)
          in
          match missing_lbl with
          | None -> 
              step acc
          | Some l ->
              acc, MissingLabel(l)
  in
  step acc

      
let label_is_addr l = 
  try
    let _ = label_to_addr l in
    true
  with
    VineError _ -> 
      false

let execute_trace_cb 
    (prog:program)
    (cb:Vine_ceval.concrete_evaluator -> 'a -> 'a)
    (acc:'a) : 'a * halt_t =

  let rec wrapper_cb 
      (evaluator:Vine_ceval.concrete_evaluator) 
      ((prev_pc, skip_to, stack, acc) : (Vine_eval.pc * int64 option *
                                           int * 'a))
      : (Vine_eval.pc * int64 option * int * 'a) =

    let pc = evaluator#get_pc () in
    let ecode = evaluator#get_ecode () in
    let stmt = Vine_eval.pc_to_stmt ecode pc in
    let stmt = remove_stmt_attrs stmt in
    
    let rec label_to_next_pc target pc =
      let stmt = Vine_eval.pc_to_stmt ecode pc in
      match remove_stmt_attrs stmt with
      | Label l when l = target ->
          pc
      | _ -> label_to_next_pc target (pc+1)
    in

(*     (\* skipping due to an ijmp? *\) *)
(*     if  *)
(*       (match skip_to with  *)
(*        | None -> false  *)
(*        | Some v ->  *)
(*            match stmt with  *)
(*            | Label l when (label_is_addr l) && (label_to_addr l) = v  *)
(*                -> false *)
(*            | _ -> true) *)
(*     then ( *)
(*       evaluator#set_pc (pc+1); *)

(*       (\* see if we've reached end of program *\) *)
(*       let eop =  *)
(*         try *)
(*           let _ = Vine_eval.pc_to_stmt ecode (evaluator#get_pc ()) in *)
(*           false *)
(*         with *)
(*           Not_found -> true *)
(*       in *)
(*       if eop then *)
(*         (\* XXX check for skipped instructions? *\) *)
(*         (prev_pc, skip_to, stack, acc) *)
(*       else *)
(*         wrapper_cb evaluator (prev_pc, skip_to, stack, acc) *)
(*     )  *)
     
    (* is this an ijmp? *)
    if 
      match stmt with
      | Jmp(Name _) -> false
      | Jmp _ -> true
      | _ -> false
    then (
      (match stmt with
       | Jmp(e) -> 
           (* let cb process the ijmp *)
           let acc = cb evaluator acc in
           
           (* don't let evaluator see the ijmp *)
           let target = val_to_const (evaluator#eval_exp e) in
           let target = 
             (match target with
              | Constant(Int(_,v)) ->
                  addr_to_label v
              | _ -> failwith "expected a constant")
           in
           (try 
              let next_pc = label_to_next_pc target pc in
              evaluator#set_pc next_pc;
              wrapper_cb evaluator (pc, skip_to, stack, acc)
            with
              Not_found ->
                (* target doesn't exist. let evaluator handle it. *)
                (pc, None, stack, acc)
           )
       | _ -> assert false
      )
    )

    (* is this a return? *)
    else if (match stmt with Return _ -> true | _ -> false)
    then (
      (prev_pc, skip_to, stack-1, acc)
    )

    (* in a function call? *)
    else if stack > 0 then (
      if match stmt with Call _ -> true | _ -> false
      then 
        (prev_pc, skip_to, stack+1, acc)
      else
        (prev_pc, skip_to, stack, acc)
    )
        
    else ( (* call callback and step *)
      assert (pc > prev_pc);

      (* FIXME: add check for skipped instructions *)
      let rec check_skipped check_pc cur_pc =
        if check_pc >= cur_pc then
          ()
        else (
          let ecode = evaluator#get_ecode () in
          let stmt = Vine_eval.pc_to_stmt ecode check_pc in
          (*          D.dprintf "Jumping over: %s" (stmt_to_string
                      stmt); *)

          (match stmt with
           | Label l when label_is_addr l ->
               D.wprintf "Skipping label %s\n" l;
               ()
           | _ ->
               ());
          check_skipped (check_pc+1) cur_pc
        )
      in
      check_skipped (prev_pc+1) pc;
      
      let stack = 
        if match stmt with Call _ -> true | _ -> false then
          stack+1
        else (
          assert (stack = 0);
          stack
        )
      in

      (pc, None, stack, cb evaluator acc)
    )
  in

  let (final_pc, final_skip_to, final_stack, final_acc), halt_type = 
    execute_prog_cb prog wrapper_cb (0, None, 0, acc)
  in
  final_acc, halt_type


(** mark beginning and end of the sl. returns a callback
    sl -> sl which extracts everything between those marks.
*)
let mark_block sl =
  let sl_start = Label(newlab "sl_start") in
  let sl_end = Label(newlab "sl_end") in
  let sl = sl_start :: (sl @ [sl_end]) in

  (* delete everything through sl_start label *)
  let rec delete_prefix last_to_del all =
    match all with
    | s::all_tl ->
        if s = last_to_del then
          all_tl
        else
          delete_prefix last_to_del all_tl
    | [] -> raise Not_found
  in
  let rec delete_suffix keep_rev first_dont_keep all =
    match all with
    | s::all_tl ->
        if s = first_dont_keep then
          List.rev keep_rev
        else
          delete_suffix (s::keep_rev) first_dont_keep all_tl
    | [] -> raise Not_found
  in

  let extract_block sl =
    let sl = delete_prefix sl_start sl in
    let sl =
      try (* end label won't be included if control jumps out of insn *)
        delete_suffix [] sl_end sl
      with
        Not_found -> sl
    in
    sl
  in
    
  sl, extract_block

let execute_trace_rewrite_cb 
    (prog:program)
    (cb:Vine_ceval.concrete_evaluator -> 'a -> stmt list * 'a)
    (acc:'a) : program * 'a * halt_t =
  let dl, sl = prog in
  
  let wrapper_cb evaluator (rnewsl, acc) = 
    let newstmts, acc = cb evaluator acc in
(*     Printf.printf "Adding stmts:\n"; *)
(*     pp_program print_string ([], newstmts); *)
    let rnewsl = List.rev_append newstmts rnewsl in
    (rnewsl, acc)
  in

  (* evaluator can add things to beginning and end.
     mark the current beginning and end and re-extract later. *)
  let sl, extract_marked_block = mark_block sl in

  let (rnewsl, acc), halt_kind = 
    execute_trace_cb (dl, sl) wrapper_cb ([], acc) in

  let add_to_decls s decls =
    let vis = object (self)
      inherit nop_vine_visitor
      val mutable varset = decls
      method get_varset () = varset

      method add_lval_to_decls l =
        (match l with
         | Mem (v,_,_)
         | Temp (v) ->
             (* XXX- check for same var with multiple types? *)
             varset <- VarSet.add v varset
        );
        DoChildren
          
      method visit_alvalue l =
        self#add_lval_to_decls l

      method visit_rlvalue l =
        self#add_lval_to_decls l
    end
    in
    let _ = Vine.stmt_accept vis s in
    vis#get_varset ()
  in
  
  (* FIXME: preserve unused decls from original dl? *)
  let varset = 
    List.fold_left 
      (fun varset s -> add_to_decls s varset)
      VarSet.empty
      rnewsl
  in

  (* add original decls to varset *)
  let varset = 
    List.fold_left
      (fun varset decl -> VarSet.add decl varset)
      varset
      dl
  in
  
  (VarSet.elements varset, 
   extract_marked_block (List.rev rnewsl)),
  acc, halt_kind

(* XXX FIXME TEMP *)
(**
   use evaluator to rewrite all Mem exps to have concrete index.
   @param prog program to rewrite
   @return program with concrete memory indices
*)
let trace_to_conc_idx (prog:Vine.program) : Vine.program =
  let callback (evaluator:Vine_ceval.concrete_evaluator) _ =
    (*   let callback pc stmt sc pcmap lblmap () = *)
    
    (* rewrite mems to have concrete indexes. *)
    let vis =
object (self)
  inherit nop_vine_visitor
  method visit_exp exp =
    match exp with
    | Let _ -> raise (Unimplemented "Let expressions in trace_to_conc_idx")
    | _ -> DoChildren
      
  method mem_to_conc_idx m =
    match m with
    | Mem(v,idx,t) ->
        let idx_val = evaluator#eval_exp idx in
        let idx_exp =
          match idx_val with
          | Vine_ceval.Int(it, iv) -> Constant(Vine.Int(it, iv))
          | _ -> raise (Invalid_argument "should be int")
        in
        Mem(v,idx_exp,t)
    | _ -> raise (Invalid_argument "expected mem")

  method visit_alvalue l =
    match l with
    | Mem _ ->
        ChangeTo(self#mem_to_conc_idx l)
    | _ ->
        DoChildren

  method visit_rlvalue l =
    match l with
    | Mem _ ->
        ChangeTo(self#mem_to_conc_idx l)
    | _ ->
        DoChildren
end
    in

    let ecode = evaluator#get_ecode () in
    let pc = evaluator#get_pc () in
    let stmt = Vine_eval.pc_to_stmt ecode pc in
    let stmt = stmt_accept vis stmt in
    ([stmt], ())
  in
  let prog, _, _ = 
    execute_trace_rewrite_cb prog callback () in
  prog

let conc_initialize_operand gamma opval =
  match opval#optype with
  | Trace.TMemLoc ->
      Opval.byte_foldleft
      (fun movs opval offset ->
         (* init with concrete value *)
         let rval = Constant(
                             Int(REG_8,Opval.byte_conc_val opval offset)) in
         let mov = Opval.byte_mov gamma opval offset rval in
         (mov::movs))
      opval
      []
  | Trace.TRegister ->
      (* initialize in one mov *)
      let regid = regid_of_addr opval#opaddr in
      let mov =
        write_reg
          gamma
          regid
          (Constant(
		    Int(regid_to_type regid,Trace.int64_of_uint32 opval#opvalue)))
          (-1)
      in
      [mov]
  | _ ->
      []

(* let array_array_to_list arr_arr = *)
(*   let ls_arr = Array.to_list arr_arr in *)
(*   (\* to flat array *\) *)
(*   let arr = Array.concat ls_arr in *)
(*   (\* to flat list *\) *)
(*   let ls = Array.to_list arr in *)
(*   ls *)

let get_flags_from_eflags gamma =
  let cf_p = 0 in
  let pf_p = 2 in
  let af_p = 4 in
  let zf_p = 6 in
  let sf_p = 7 in
  let of_p = 11 in

  let (_,_,eflags_t) as eflags = Asmir.gamma_lookup gamma "EFLAGS" in

  let flag_assignment s pos =
    Move(Temp(Asmir.gamma_lookup gamma s),
         Cast(CAST_LOW,
              REG_1,
              BinOp(RSHIFT,
                    Lval(Temp(eflags)),
                    const_of_int eflags_t pos)
             )
        )
  in
  [flag_assignment "R_CF" cf_p;
   flag_assignment "R_PF" pf_p;
   flag_assignment "R_AF" af_p;
   flag_assignment "R_ZF" zf_p;
   flag_assignment "R_SF" sf_p;
   flag_assignment "R_OF" of_p;
  ]

type trace_insn = {
  label : Vine.label;
  cmt : string;
  asm_dl : Vine.decl list;
  asm_sl : Vine.stmt list;
  setup_ir : Vine.stmt list;
  tainted_eflags : bool; (* XXX temu interface should provide this
                            eventually *)
} ;;

type trace_prog = {
  use_thunks : bool;
  arch : Asmir.arch;
  gamma : Asmir.varctx;
  dl : Vine.decl list;
  mems_arrays : (Vine.decl * Vine.decl) list;
  prog_setup_ir : Vine.stmt list;
} ;;

type filter_application = trace_prog -> trace_insn option -> 
  Temu_trace.instruction -> int64 -> trace_prog * trace_insn option
class type eh_filter =
object
  method apply : filter_application
(*     trace_prog -> trace_insn option -> Temu_trace.instruction -> int *)
(*     -> trace_prog * trace_insn option *)
end

let trace_prog_add_var prog ((_,s,_) as v) =
  Asmir.gamma_extend prog.gamma s v;
  let dl = v :: prog.dl in
  {prog with dl = dl}

let trace_insn_of_eh prog eh = 
  let raw_ir =        
    Asmir.asm_bytes_to_vine prog.gamma prog.arch eh#address eh#rawbytes 
  in

  let label, cmt, asm_dl, asm_sl =
    match raw_ir with
    | [Vine.Block(asm_dl, 
                  (Label label)
                  :: (Comment cmt) 
                  :: asm_sl)] ->
        label, cmt, asm_dl, asm_sl
    | _ -> raise (Invalid_argument "trace_insn_of_eh") 
  in

  let cmt = 
    if String.exists cmt "not implemented" then
      let rawbytes = Array.to_list eh#rawbytes in
      let rawbytes = 
        List.map
          (fun x -> Printf.sprintf "0x%02x " (int_of_char x))
          rawbytes
      in
      Printf.sprintf "%s (%s)" cmt (String.join "" rawbytes)
    else
      cmt
  in

  {
    label = label;
    cmt = cmt;
    asm_dl = asm_dl;
    asm_sl = asm_sl;
    setup_ir = [];
    tainted_eflags = true; (* XXX *)
  }

(* Memory registers and source operands needs to be initialized after 
   destination operands. Otherwise, the former's values will be overwriten
   
   Example:  movz{bl|x|bl|x} (%eax,%ecx,2),%eax
*)
let opvals_fold_left ?(ignore_write_ops=false) cb acc eh =
  let is_read_op op =
    match op#opaccess with
      | Trace.A_Unknown | Trace.A_RW | Trace.A_R
      | Trace.A_RCW | Trace.A_CR | Trace.A_CRW -> true
      | Trace.A_W | Trace.A_CW -> false
  in
  let acc =
    Array.fold_left
      (fun acc op_ary ->
         Array.fold_left
           (cb true)
           acc
           op_ary
      )
      acc
      eh#memregs
  in
  let op_l = (List.rev (Array.to_list eh#operand)) in
  let op_l = 
    if (ignore_write_ops) 
      then List.filter is_read_op op_l 
      else op_l
  in
  let acc =
    List.fold_left
      (cb false)
      acc
      op_l
  in
  if uses_esp eh#rawbytes then
    (cb true) acc eh#esp
  else
    acc

let opvals_exists pred eh =
  try
    (opvals_fold_left
       (fun _ () ov ->
          if pred ov then
            raise Found)
       ()
       eh);
    false
  with Found -> true

let has_eflags_lval gamma stmts =
  slist_exists
    (function 
       | Move(Temp(v),_) when 
           ((Var.equal v (Asmir.gamma_lookup gamma "R_CC_OP")) 
            || (Var.equal v (Asmir.gamma_lookup gamma "EFLAGS")))
           ->
           true
       | _ -> false)
    stmts

let has_cjmp stmts =
  slist_exists
    (function 
       | CJmp _ ->
           true
       | _ -> false)
    stmts

(** print each instruction (for debugging) *)
class print_filter : eh_filter = 
object(self)
  val mutable num_nones = 0
  method apply prog insn_opt eh eh_num =
    if D.debug then (
      (match insn_opt with
       | None -> 
           num_nones <- num_nones + 1;
       | Some(insn) ->
           if num_nones > 0 then
             Printf.printf "(%d undisasmed)\n" num_nones;
           num_nones <- 0;
           Printf.printf "%s\n%s\n" 
             insn.label
             insn.cmt;
           pp_program print_string ([], insn.setup_ir);
           pp_program print_string (insn.asm_dl, insn.asm_sl)
      )
    );
    prog, insn_opt
      
end

let deend_prog_single (dl,sl) mems_arrays =
  List.fold_left
    (fun (dl, sl) (mem, arr) ->
       Vine_memory2array.coerce_program_variable
         mem
         arr
         (dl, sl)
    )
    (dl, sl)
    mems_arrays

let deend_prog_multi (dl, sl) mems_arrays =
  let mems = (List.map (fun (m, a) -> m) mems_arrays) in
    (* Because of the way exectrace works, de-endianizing different
       parts of the program separately, we have to be careful to make
       sure the arrays the memories are translated into get properly
       declared. Rather than fixing exectrace's assumption that it knows
       the name of the array, we just always include the memory
       declaration in the input to the transformation here, so that the
       output of the transformation will always include the proper array
       declaration. *)
    Vine_memory2array.coerce_prog_multi_varlist
      mems (mems @ dl, sl)

let deend_use_multi_flag = ref false
let deend_use_multi b =
  Printf.printf "Setting deend_multi to %b\n" b;
  deend_use_multi_flag := b

let deend_prog prog arrays =
  (if !deend_use_multi_flag then
     deend_prog_multi else deend_prog_single) prog arrays

(** deendianize asm and filter IR *)
class deend_filter : eh_filter = 
object(self)
  method apply prog insn_opt eh eh_num =
    (match insn_opt with
     | None -> 
         prog, insn_opt
     | Some(insn) ->
         let asm_dl, asm_sl = 
           deend_prog (insn.asm_dl, insn.asm_sl) prog.mems_arrays
         in
         let setup_dl, setup_sl =
           deend_prog ([], insn.setup_ir) prog.mems_arrays
         in
         let setup_ir =
           if setup_dl = [] then
             setup_sl
           else
             [Block(setup_dl, setup_sl)]
         in
         prog, Some({insn with 
                       asm_dl=asm_dl; 
                       asm_sl=asm_sl; 
                       setup_ir=setup_ir})
    )
end


(** rewrite labels to ensure uniqueness. currently implemented
    by appending the entry-header-number to each label.
*)
class uniqify_labels : eh_filter = 
object(self)
  method apply prog insn_opt eh eh_num =
    let rewrite_lbl_string s =
      Printf.sprintf "%s_%Ld" s eh_num
    in

    let insn_opt =
      (match insn_opt with
       | None -> None
       | Some(insn) ->
           let vis = 
       object (self)
         inherit nop_vine_visitor
         method visit_stmt stmt =
           match stmt with
           | Label(s) ->
               ChangeDoChildrenPost(Label(rewrite_lbl_string s),
                                    Vine_util.id)
           | _ -> DoChildren
       end
           in
           let asm_sl = 
             List.map
               (Vine.stmt_accept vis)
               insn.asm_sl
           in
           Some({insn with 
                   asm_sl=asm_sl; 
                   label=rewrite_lbl_string insn.label}))
    in
    prog, insn_opt
end
    
(** an execution trace disasm'er.
    determines whether to include each instruction.
    if so, disassembles it.
    otherwise, drops it.
    currently simplified.
    todo: add back options to include some untainted instructions.
*)
class disasm_tainted : eh_filter = 
object(self)
  val mutable tainted_eflags = false
  method apply prog insn_opt eh eh_num =
    let tainted_opvals = 
      opvals_exists 
        (fun ov -> ov#taintflag <> 0L)
        eh
    in

    (* avoid disassembling most untainted instructions *)
    let trace_insn_lazy = 
      Lazy.lazy_from_fun 
        (fun _ -> 
           {(trace_insn_of_eh prog eh)
            with tainted_eflags = tainted_eflags})
    in

    (* include disassembly if:
       tainted operands
       tainted eflags and instruction has a cjmp
    *)
    let include_in_prog =
      match eh#tp with
      | Trace.TPUnknown ->
          (* trace doesn't have taint prop info, use our own
               tracking *)
          (tainted_opvals ||
             (tainted_eflags && has_cjmp (Lazy.force
                                            trace_insn_lazy).asm_sl))
      | Trace.TPNone
        -> false
      | Trace.TPSrc
      | Trace.TPCjmp
      | Trace.TPMemReadIndex
      | Trace.TPMemWriteIndex
      | Trace.TPRepCounter 
        -> true
    in

    let insn_opt =
      if include_in_prog
      then
        Some(Lazy.force trace_insn_lazy)
      else
        None
    in

    (* determine if eflags is tainted after executing this instruc *)
    (* careful to only force instr_stmts if necessary *)
    (tainted_eflags <- 
       if (not tainted_eflags) && (not tainted_opvals) then
         false
       else if (has_eflags_lval 
                  prog.gamma
                  (Lazy.force trace_insn_lazy).asm_sl) then
         tainted_opvals
       else
         tainted_eflags);

    prog, insn_opt
end

class disasm_all : eh_filter = 
object(self)
  method apply prog insn_opt eh eh_num =
    prog, Some(trace_insn_of_eh prog eh)
end

(** tracks what symbolic values and locations have been seen.
    used as a common resource by other filters. 
    when using this filter with other filters that depend on it,
    make sure this one is BEFORE the others.
*)
class track_opval_filter =
object(self)
  val mutable initd_mems = PMap.empty
  val mutable initd_regs = PMap.empty
  val mutable prev_initd_mems = PMap.empty
  val mutable prev_initd_regs = PMap.empty

  val mutable input_vars = PMap.empty
  val mutable prev_input_vars = PMap.empty

  method private initd_locs_add opval offset = 
    match opval#optype with
    | Trace.TRegister -> 
        let reg, offset =
          canon_reg_offset (regid_of_addr opval#opaddr) offset in
        initd_regs <- PMap.add (reg,offset) () initd_regs
    | Trace.TMemLoc ->
        initd_mems <- 
          (PMap.add 
             (Int64.add opval#opaddr (Int64.of_int offset)) 
             () 
             initd_mems)
    | _ -> failwith "unhandled optype"

  method prev_initd_locs_mem (opval:Trace.operand) offset =
    match opval#optype with
    | Trace.TRegister ->
        let reg, offset =
          canon_reg_offset (regid_of_addr opval#opaddr) offset in
        PMap.mem
          (reg, offset)
          prev_initd_regs
    | Trace.TMemLoc ->
        PMap.mem
          (Int64.add opval#opaddr (Int64.of_int offset))
          prev_initd_mems
    | _ -> failwith "unhandled optype"

  method prev_input_vars_mem origin offset = 
    PMap.mem (origin,offset) prev_input_vars

  method private input_vars_add origin offset value = 
    let name =
      Printf.sprintf "INPUT_%lu_%04lu"
        origin
        offset
    in
    let symvar = newvar name Vine.REG_8 in
    input_vars <- PMap.add (origin,offset) (symvar, value) input_vars;
    symvar

  method input_vars_var origin offset = 
    let ivar, ival = PMap.find (origin,offset) input_vars in
    ivar

  method input_vars_val origin offset = 
    let ivar, ival = PMap.find (origin,offset) input_vars in
    ival

(* XXX: currently very approximately simulated by initd_locs_mem *)
(*   method may_be_tainted_by  *)
(*     opval opval_offset input_origin input_offset = *)

  method apply (prog:trace_prog) (insn_opt:trace_insn option)
    (eh:Trace.instruction) (eh_num:int64) = 
    prev_initd_mems <- initd_mems;
    prev_initd_regs <- initd_regs;
    prev_input_vars <- input_vars;

    (match insn_opt with
     | None -> 
         prog, insn_opt
     | Some(insn) ->
         let new_vars = 
           opvals_fold_left
             (fun _ new_vars opval ->
                match opval#optype with
                | Trace.TRegister 
                | Trace.TMemLoc ->
                    Opval.byte_foldleft
                      (fun new_vars opval offset -> 
                         self#initd_locs_add opval offset;
                         if (Opval.byte_tainted opval offset
                             && not (PMap.mem 
                                       ((opval#origin).(offset),
                                        (opval#offset).(offset))
                                       input_vars))
                         then
                           let new_var = 
                             self#input_vars_add 
                               (opval#origin).(offset)
                               (opval#offset).(offset)
                               (Opval.byte_conc_val opval offset)
                           in 
                           new_var::new_vars
                         else
                           new_vars
                      )
                      opval
                      new_vars
                | _ -> new_vars
             )
             []
             eh
         in
         {prog with dl = List.rev_append new_vars prog.dl}, insn_opt
    )
end

(** initializes instruction operands concretely or symbolically.
    Needs a track_opval_filter object, which must execute BEFORE
    this one in the filter list given to disasm_trace *)
class initialize_operands_small
  (tracker:track_opval_filter) expected_asserts =
object (self)
  val tracker = tracker
  val expected_asserts = expected_asserts

  (* handle initialization of an operand byte tainted by
     an input that has NOT appeared earlier in the trace. *)
  method private init_sym_byte_first 
    prog eh_num opval opval_offset input_origin input_offset conc_val =
    
    let input_var = 
      tracker#input_vars_var
        input_origin input_offset
    in
    [Opval.byte_mov 
       prog.gamma 
       opval 
       opval_offset
       (Lval(Temp(input_var)))]

  (* handle initialization of an operand byte tainted by
     an input that HAS appeared earlier in the trace,
     and for which the propagation IS plausible. *)
  method private init_sym_byte_propped 
    prog eh_num opval opval_offset input_origin input_offset conc_val =
    (* initd this symbol and location. all ok *)

    (* XXX FIXME : break into separate filter? *)
    let movs =
      if expected_asserts then
        [Assert(BinOp(EQ, 
                      (Opval.byte_exp 
                         prog.gamma 
                         opval 
                         opval_offset),
                      conc_val))]
      else
        []
    in

    let movs = 
      (Comment(Printf.sprintf
                 "%s:%d Already initd input (%ld, %ld)"
                 (Opval.to_string opval)
                 opval_offset
                 input_origin
		 input_offset))
      :: movs
    in
    movs

  (* handle initialization of an operand byte tainted by
     an input that HAS appeared earlier in the trace,
     and for which the propagation is NOT plausible. *)      
  method private init_sym_byte_missed_prop 
    prog eh_num opval opval_offset input_origin input_offset conc_val =
    (* initd this symbol, but not this
       location. this probably means
       the trace is missing instructions that
       propagated tainted data.
    *)
    (D.wprintf
       "eh %Ld: Missed prop of inp(%ld, %ld) to %s:%d\n"
       eh_num
       input_origin
       input_offset
       (Opval.to_string opval)
       opval_offset);

    (* create a fresh symbol *)
    let name =
      Printf.sprintf "INPUT_missed_%lu_%04lu"
        input_origin
        input_offset
    in
    let symvar = newvar name Vine.REG_8 in

    (* assign the symbol the conc. val from trace,
       and assign the operand the symbol.*)
    let sl =
      [Comment("WARNING missed prop, using concrete init");
       Move(Temp(symvar), conc_val);
       (Opval.byte_mov 
          prog.gamma
          opval
          opval_offset
          (Lval(Temp(symvar))))]
    in

    [Vine.Block([symvar], sl)]

  (* initialize a single byte of a tainted operand *)
  method private init_sym_byte 
    prog eh_num opval opval_offset input_origin input_offset conc_val =

    if not (tracker#prev_input_vars_mem input_origin input_offset)
    then
      (* case: first time seeing this input origin and offset *)
      self#init_sym_byte_first
        prog eh_num opval opval_offset input_origin input_offset conc_val
    else
      if (tracker#prev_initd_locs_mem opval opval_offset) then 
        (* case: not first time. propagation check passes *)
        self#init_sym_byte_propped
          prog eh_num opval opval_offset input_origin input_offset conc_val
      else
        (* case: not first time. propagation check fails *)
        self#init_sym_byte_missed_prop
          prog eh_num opval opval_offset input_origin input_offset conc_val

  (* initialize a single byte of an untainted operand *)
  method private init_conc_byte prog eh_num opval opval_offset conc_val =
    (* untainted, init concretely *)
    [Opval.byte_mov 
         prog.gamma
         opval
         opval_offset
         conc_val]

  (* initialize a whole concrete operand *)
  method private init_conc_opval prog eh_num opval =
    let rhs = Opval.const opval in
    [Opval.mov prog.gamma opval rhs]

  (* initialize an operand *)
  method private initialize_operand prog eh_num is_mem_reg movs opval =      
    let isreg =
      match opval#optype with
      | Trace.TRegister -> true
      | _ -> false
    in
    let ismem =
      match opval#optype with
      | Trace.TMemLoc -> true
      | _ -> false
    in
    if not (ismem || isreg) then
      movs
    else
      (* case: whole opval is concrete *)
      let new_movs = 
        if opval#taintflag = 0L then
          self#init_conc_opval prog eh_num opval 
        else
          (* at least partially tainted. iterate through opval bytes *)
          Opval.byte_foldleft
            (fun movs opval offset ->
               let conc_val =
                 const_of_int64
                   REG_8
                   (Opval.byte_conc_val
                      opval
                      offset)
               in

               let new_movs =
                 if Opval.byte_tainted opval offset then
                   (* symbolic byte *)
                   self#init_sym_byte 
                     prog
                     eh_num
                     opval 
                     offset 
                     (opval#origin).(offset)
                     (opval#offset).(offset)
                     conc_val
                 else
                   (* concrete byte *)
                   self#init_conc_byte prog eh_num opval offset conc_val
               in
               List.rev_append new_movs movs
            )
            opval
            []
      in
      List.rev_append new_movs movs
            
  method apply prog insn_opt (eh:Trace.instruction) (eh_num:int64) = 
    match insn_opt with
    | None -> prog, insn_opt
    | Some(insn) ->

        (* create initializers *)
        let special_movs =
          [
            Move(Temp(Asmir.gamma_lookup prog.gamma "R_GDT"),
                 Constant(Int(REG_32, Trace.int64_of_uint32 eh#gdt)));
            Move(Temp(Asmir.gamma_lookup prog.gamma "R_LDT"),
                 Constant(Int(REG_32, Trace.int64_of_uint32 eh#ldt)));
            Move(Temp(Asmir.gamma_lookup prog.gamma "R_DFLAG"),
                 Constant(Int(REG_32, Trace.int64_of_uint32 eh#df)));
          ]
        in

        (* if eflags is untainted 
           and conditions are used, 
           initialize conditions *)
        let special_movs = 
          if ( not insn.tainted_eflags )
            &&
            ( 
              let flag_vars = 
                List.fold_left
                  (fun s v -> VarSet.add v s)
                  VarSet.empty
                  [Asmir.gamma_lookup prog.gamma "R_CF";
                   Asmir.gamma_lookup prog.gamma "R_PF";
                   Asmir.gamma_lookup prog.gamma "R_AF";
                   Asmir.gamma_lookup prog.gamma "R_ZF";
                   Asmir.gamma_lookup prog.gamma "R_SF";
                   Asmir.gamma_lookup prog.gamma "R_OF"]
              in
              let rvars = rvars_of_stmt (Block([], insn.asm_sl)) in
              not (VarSet.is_empty (VarSet.inter rvars flag_vars))
            ) then
              let eflag_inits = 
                (Move(Temp(Asmir.gamma_lookup prog.gamma "EFLAGS"),
                      Constant(Int(REG_32, Trace.int64_of_uint32
                                     eh#eflags))))
                :: (get_flags_from_eflags prog.gamma) in
              eflag_inits @ special_movs
          else
            special_movs
        in

        let movs =
          opvals_fold_left
            (self#initialize_operand prog eh_num)
            special_movs
            eh
        in

        let init_block = Block([], Comment("Initializers") :: movs) in

        (prog,
         Some({insn with setup_ir = init_block::(insn.setup_ir)}))

end

(** variation of initialize_operands_small that creates
    a fresh symbol after a missed propagation.
    XXX FIXME: initialize_input_symbols currently won't initialize
    these symbols.
*)
(* class initialize_operands_small_fresh_symbols *)
(*   (tracker:track_opval_filter) expected_asserts = *)
(* object (self) *)
(*   inherit initialize_operands_small tracker expected_asserts *)

(*   (\* handle initialization of an operand byte tainted by *)
(*      an input that HAS appeared earlier in the trace, *)
(*      and for which the propagation is NOT plausible. *\)       *)
(*   method private init_sym_byte_missed_prop  *)
(*     prog eh_num opval opval_offset input_origin input_offset conc_val = *)
(*     (\* initd this symbol, but not this *)
(*        location. this probably means *)
(*        the trace is missing instructions that *)
(*        propagated tainted data. *)
(*     *\) *)
(*     (D.wprintf *)
(*        "eh %d: Missed prop of inp(%ld, %ld) to %s:%d\n" *)
(*        eh_num *)
(*        input_origin *)
(*        input_offset *)
(*        (Opval.to_string opval) *)
(*        opval_offset); *)

(*     let fresh_var =  *)
(*       renewvar (tracker#input_vars_var input_origin input_offset)  *)
(*     in *)

(*     (\* initialize concretely *\) *)
(*     [Comment("WARNING missed prop, using concrete init"); *)
(*      (Opval.byte_mov  *)
(*         prog.gamma *)
(*         opval *)
(*         opval_offset *)
(*         (Lval(Temp(fresh_var))))] *)
(* end *)





(** 
    initialize input symbols to the concrete value from the trace on
    the first instruction involving a particular input byte. 
    Needs a track_opval_filter object, which must execute BEFORE
    this one in the filter list given to disasm_trace 
*)
class initialize_input_symbols (tracker:track_opval_filter)=
object(self)
  val tracker = tracker
  method apply (prog:trace_prog) insn_opt (eh:Trace.instruction) (eh_num:int64) = 
    match insn_opt with
    | None -> prog, None
    | Some(insn) ->
        let movs =
          opvals_fold_left
            (fun _ movs opval ->
               if opval#taintflag = 0L 
                 || (match opval#optype with 
                     |Trace.TMemLoc|Trace.TRegister->false
                     |_ -> true) then
                   movs
               else
                 Opval.byte_foldleft
                   (fun movs opval offset ->
                      if (Opval.byte_tainted opval offset) &&
                        not (tracker#prev_input_vars_mem
                               (opval#origin).(offset)
                               (opval#offset).(offset))
                      then
                        (Move(Temp(tracker#input_vars_var
                                     (opval#origin).(offset)
                                     (opval#offset).(offset)),
                              const_of_int64
                                REG_8
                                (Opval.byte_conc_val
                                   opval
                                   offset)
                             )
                        )::movs
                      else
                        movs
                   )
                   opval
                   movs
            )
            []
            eh
        in

        let insn =
          if movs = [] then
            insn
          else
            {insn with setup_ir = 
                (Vine.Block([],Comment("initialize_input_symbols")
                              ::movs))
                ::insn.setup_ir}
        in
        prog, Some(insn)
end

(** for Register operands with some fresh symbolic values,
    and no propagated symbolic values,
    zero out the register. this makes dependency analysis easier
    by making it clear that the value of the register does not
    depend on any previous statements
    Needs a track_opval_filter object, which must execute BEFORE
    this one in the filter list given to disasm_trace.
*)
class break_dep_chains_filter (tracker:track_opval_filter)=
object(self)
  val tracker = tracker
  method apply (prog:trace_prog) insn_opt (eh:Trace.instruction) (eh_num:int64) = 
    match insn_opt with
    | None -> prog, None
    | Some(insn) ->
        let movs =
          opvals_fold_left
            (fun _ movs opval ->
               if (match opval#optype with 
                   | Trace.TRegister -> true
                   | _ -> false) then
                 let (propd_bytes,sym_bytes) =
                   Opval.byte_foldleft
                     (fun (propd_bytes,sym_bytes) opval offset ->
                        if (Opval.byte_tainted opval offset) then
                          if (tracker#prev_input_vars_mem
                                (opval#origin).(offset)
                                (opval#offset).(offset)) then
                            true, true
                          else
                            propd_bytes, true
                        else
                          (propd_bytes,sym_bytes)
                     )
                     opval
                     (false,false)
                 in
                 if (not propd_bytes) && sym_bytes then
                   let rhs = Opval.const opval in
                   (Opval.mov prog.gamma opval rhs)::movs
                 else
                   movs
               else
                 movs
            )
            []
            eh
        in

        let insn =
          if movs = [] then
            insn
          else
            {insn with setup_ir = 
                (Vine.Block([],Comment("break_dep_chains")
                              ::movs))
                ::insn.setup_ir}
        in
        prog, Some(insn)
end

(** XXX To implement. break this out of initialize_operands_small *)
class check_expected_values_filter (tracker:track_opval_filter)=
object(self)
  val tracker = tracker
    (* add assertions that propagated tainted data has the same
       value as it did concretely in the trace. *)
end

(** trace filter: add initializer statements to beginning 
    @deprecated This has been broken into several smaller filters.
    see initialize_operands_small, initialize_input_symbols,
    check_expected_values_filter, and track_opval_filter.
*)
class initialize_operands conc_inputs (expected_asserts:bool)  = 
object (self)
  val conc_inputs = conc_inputs
  val expected_asserts = expected_asserts
  val mutable initd_mems = PMap.empty
  val mutable initd_regs = PMap.empty
  val mutable initd_inputs = PMap.empty
  method private initd_locs_add opval offset = 
    match opval#optype with
    | Trace.TRegister -> 
        let reg, offset =
          canon_reg_offset (regid_of_addr opval#opaddr) offset in
        initd_regs <- PMap.add (reg,offset) () initd_regs
    | Trace.TMemLoc ->
        initd_mems <- 
          (PMap.add 
             (Int64.add opval#opaddr (Int64.of_int offset)) 
             () 
             initd_mems)
    | _ -> failwith "unhandled optype"
  method private initd_locs_mem opval offset : bool = 
    match opval#optype with
    | Trace.TRegister ->
        let reg, offset =
          canon_reg_offset (regid_of_addr opval#opaddr) offset in
        PMap.mem
          (reg, offset)
          initd_regs
    | Trace.TMemLoc ->
        PMap.mem              
          (Int64.add opval#opaddr (Int64.of_int offset)) 
          initd_mems
    | _ -> failwith "unhandled optype"
  method private initd_inputs_add origin offset = 
    initd_inputs <- PMap.add (origin,offset) () initd_inputs
  method private initd_inputs_mem origin offset = 
    PMap.mem (origin,offset) initd_inputs
  method apply prog insn_opt (eh:Trace.instruction) (eh_num:int64) = 
    let init_symbolic_byte (movs,ivars) opval offset =
      (* init with fresh name *)
      let name =
        Printf.sprintf "INPUT_%lu_%04lu"
          (opval#origin).(offset)
          (opval#offset).(offset)
      in
      let concval =
        const_of_int64
          REG_8
          (Opval.byte_conc_val
             opval
             offset)
      in

      let symvar = newvar name Vine.REG_8 in
      let symval = Temp(symvar) in
      let movs = 
        (Opval.byte_mov 
           prog.gamma 
           opval 
           offset
           (Lval(symval)))
        :: movs 
      in
      self#initd_inputs_add 
        (opval#origin).(offset)
        (opval#offset).(offset);
      
      (* optionally init symbol with concrete value *)
      let movs =
        if conc_inputs then
          Move(symval, concval)::movs
        else
          movs
      in
      
      movs, symvar::ivars
    in


    match insn_opt with
    | None -> prog, insn_opt
    | Some(insn) ->
        let initialize_operand is_mem_reg (movs,ivars) opval =
          let isreg =
            match opval#optype with
            | Trace.TRegister -> true
            | _ -> false
          in

          match opval#optype with
          | Trace.TMemLoc
          | Trace.TRegister ->
              (* optimization -
                 initialize untainted operands in one mov *)
              if opval#taintflag = 0L && isreg &&
                (let regid = regid_of_addr opval#opaddr in
                 (regid_to_full regid) = regid)
              then
                let regid = regid_of_addr opval#opaddr in
                let mov =
                  write_reg
                    prog.gamma
                    regid
                    (const_of_int64
                       (regid_to_type regid)
                       (Trace.int64_of_uint32 opval#opvalue))
                    (-1)
                in
                (Opval.byte_foldleft
                   (fun () opval offset -> self#initd_locs_add opval offset)
                   opval
                   ());
                mov::movs, ivars
              else
                Opval.byte_foldleft
                  (fun (movs,ivars) opval offset ->
                     let concval =
                       const_of_int64
                         REG_8
                         (Opval.byte_conc_val
                            opval
                            offset)
                     in
                     let movs, ivars =
                       if Opval.byte_tainted opval offset then
                         if (self#initd_inputs_mem
                               (opval#origin).(offset)
                               (opval#offset).(offset))
                         then (
                           if (self#initd_locs_mem opval offset) then (
                             (* initd this symbol and location. all ok *)
                             let movs =
                               if expected_asserts then
                                 (Assert(BinOp(EQ, 
                                               (Opval.byte_exp 
                                                  prog.gamma 
                                                  opval 
                                                  offset),
                                               concval)))::movs
                               else
                                 movs
                             in
                             let movs = 
                               (Comment(Printf.sprintf
                                          "%s:%d Already initd input (%ld, %ld)"
                                          (Opval.to_string opval)
                                          offset
                                          (opval#origin).(offset)
		                          (opval#offset).(offset)))
                               :: movs
                             in
                             movs, ivars
                           ) else (
                             (* initd this symbol, but not this
                                location. this probably means
                                the trace is missing instructions that
                                propagated tainted data.
                             *)
                             (D.wprintf
                                "eh %Ld: Missed prop of inp(%ld, %ld) to %s:%d\n"
                                eh_num
                                ((opval#origin).(offset))
		                ((opval#offset).(offset))
                                (Opval.to_string opval)
                                offset);

                             init_symbolic_byte (movs,ivars) opval offset
                               (* movs, ivars *)
                           )
                         ) else
                           (* init with fresh name *)
                           init_symbolic_byte (movs,ivars) opval offset
                       else (
                         (* untainted, init concretely *)
                         let movs =
                           (Opval.byte_mov 
                              prog.gamma
                              opval
                              offset
                              concval) 
                           :: movs
                         in
                         movs, ivars
                       )
                     in
                     self#initd_locs_add opval offset;
                     movs, ivars
		  )
		  opval
		  (movs, ivars)
	  | _ ->
              movs, ivars
        in
              
        (* create initializers *)
        let special_movs =
          [
            Move(Temp(Asmir.gamma_lookup prog.gamma "R_GDT"),
                 Constant(Int(REG_32, Trace.int64_of_uint32 eh#gdt)));
            Move(Temp(Asmir.gamma_lookup prog.gamma "R_LDT"),
                 Constant(Int(REG_32, Trace.int64_of_uint32 eh#ldt)));
            Move(Temp(Asmir.gamma_lookup prog.gamma "R_DFLAG"),
                 Constant(Int(REG_32, Trace.int64_of_uint32 eh#df)));
          ]
        in

        (* if eflags is untainted 
           and conditions are used, 
           initialize conditions *)
        let special_movs = 
          if ( not insn.tainted_eflags )
            &&
            ( 
              let flag_vars = 
                List.fold_left
                  (fun s v -> VarSet.add v s)
                  VarSet.empty
                  [Asmir.gamma_lookup prog.gamma "R_CF";
                   Asmir.gamma_lookup prog.gamma "R_PF";
                   Asmir.gamma_lookup prog.gamma "R_AF";
                   Asmir.gamma_lookup prog.gamma "R_ZF";
                   Asmir.gamma_lookup prog.gamma "R_SF";
                   Asmir.gamma_lookup prog.gamma "R_OF"]
              in
              let rvars = rvars_of_stmt (Block([], insn.asm_sl)) in
              not (VarSet.is_empty (VarSet.inter rvars flag_vars))
            ) then
              let eflag_inits = 
                (Move(Temp(Asmir.gamma_lookup prog.gamma "EFLAGS"),
                      Constant(Int(REG_32, Trace.int64_of_uint32
                                     eh#eflags))))
                :: (get_flags_from_eflags prog.gamma) in
              eflag_inits @ special_movs
          else
            special_movs
        in

        let movs, ivars =
          opvals_fold_left
            initialize_operand
            (special_movs, [])
            eh
        in

        let prog = 
          List.fold_left
            trace_prog_add_var
            prog
            ivars
        in

        let init_block = Block([], Comment("Initializers") :: movs) in

        (prog,
         Some({insn with setup_ir = init_block::(insn.setup_ir)}))
end

(** add assertions to constrain memory accesses of tainted indices
    are within the memory ranges specified in the instruction
    operands, or within the provided static_valid_ranges
*)
class constrain_mem_accesses static_valid_ranges  = 
object (self)
  val mutable static_valid_ranges = static_valid_ranges
  method apply (prog:trace_prog) insn_opt (eh:Trace.instruction) (eh_num:int64) = 
    match insn_opt with
    | None -> prog, insn_opt
    | Some(insn) ->
        let there_is_a_tainted_mem_reg eh = 
          opvals_fold_left 
            (fun is_mem_reg flag opval ->
               flag || (is_mem_reg && (opval#taintflag <> 0L)))
            false 
            eh
        in
        (* get ranges of initd memory from opvals.
           list of tuples for which [lo, hi] are initd *)
        let get_valid_mem_ranges eh = 
          opvals_fold_left 
            (fun _ valid_mem_locs opval ->
               match opval#optype with
               | Trace.TMemLoc ->
                   (opval#opaddr, 
                    Int64.pred
                      (Int64.add 
                         opval#opaddr 
                         (Int64.of_int opval#oplen)))
                   :: valid_mem_locs
               | _ -> 
                   valid_mem_locs
            )
            static_valid_ranges 
            eh
        in
        let add_asserts ir_sl valid_mem_ranges = 
          let vis =  object (self)
            inherit nop_vine_visitor
            val mutable mem_exps = []
            method visit_exp exp =
              match exp with
              | Let _ -> 
                  (* shouldn't be too difficult to implement if this comes
                     up. just need to keep a stack of mem_exp lists *)
                  raise (Unimplemented "Let expressions in constrain_mem_accesses")
              | _ -> DoChildren
                  
            method visit_alrvalue lr =
              ChangeDoChildrenPost(
                lr,
                (fun lr ->
                   match lr with
                   | Mem(v, idx_exp, t) ->
                       let typ = Vine_typecheck.infer_type None idx_exp in
                       let idx_var = newvar "idx" typ in
                       mem_exps <- 
                         ((idx_var, 
                           (idx_exp, 
                            Int64.of_int ((bits_of_width t) / 8)
                           )
                          )
                          :: mem_exps);
                       Mem(v,Lval(Temp(idx_var)),t)
                   | _ -> 
                       lr
                ))

            method visit_alvalue l =
              match l with
              | Mem _ ->
                  raise (Unimplemented "shouldn't get here")
              | _ -> 
                  DoChildren

            method visit_rlvalue l =
              self#visit_alrvalue l

            method visit_stmt s =
              mem_exps <- [];
              ChangeDoChildrenPost(
                s,
                (fun s ->
                   if mem_exps = [] then
                     s
                   else
                     let (decls, _) = List.split mem_exps in
                     let asserts = 
                       List.fold_left 
                         (fun asserts (idx_var, (idx_exp, idx_sz)) ->
                            let must_idx_opt =
                              match valid_mem_ranges with
                              | [(x,y)] ->
                                  if (Int64.succ (Int64.sub y x)) =
                                    idx_sz then (
(*                                       Printf.printf "Fixing to 0x%Lx\n" x; *)
                                      Some(x))
                                  else
                                    None
                              | _ ->
                                  None
                            in
                            let _,_,typ = idx_var in
                            match must_idx_opt with
                            | Some(must_idx) ->
                                (* idx can only have one possible value.
                                   use a simpler assert statement, and
                                   set index to the concrete value. *)
                                let must_idx = 
                                  const_of_int64 typ must_idx 
                                in
                                Move(Temp(idx_var), must_idx)
                                :: Assert(exp_eq idx_exp must_idx)
                                :: asserts
                            | None ->
                                Move(Temp(idx_var), idx_exp)
                                :: Assert(
                                  List.fold_left
                                    (fun disj (lo,hi) -> 
                                       BinOp(BITOR,
                                             BinOp(BITAND,
                                                   BinOp(LE,
                                                         const_of_int64 typ lo,
                                                         Lval(Temp(idx_var))),
                                                   BinOp(LE,
                                                         BinOp(PLUS,
                                                               Lval(Temp(idx_var)),
                                                               const_of_int64 typ (Int64.pred idx_sz)),
                                                         const_of_int64 typ hi)),
                                             disj)
                                    )
                                    exp_false
                                    valid_mem_ranges
                                ) 
                                :: asserts)
                         []
                         mem_exps
                     in
                     Block(decls, Comment("constrain_mem_accesses") :: (asserts @ [s]))
                )
              )
          end
          in
          List.map (Vine.stmt_accept vis) ir_sl
        in

        let has_mem_lval sl =
          let vis =  object (self)
            inherit nop_vine_visitor
            val mutable flag = false
            method get_flag = flag
            method visit_alvalue lr =
              match lr with
              | Mem _ ->
                  flag <- true;
                  SkipChildren
              | _ ->
                  DoChildren

            method visit_stmt s =
              if flag then
                SkipChildren
              else
                DoChildren
          end
          in
          let _ = List.map (Vine.stmt_accept vis) sl in
          vis#get_flag
        in

        let constrain_mem_ops eh =
          let asserts =
            opvals_fold_left
              (fun is_mem_reg asserts opval ->
                 if opval#taintflag <> 0L && is_mem_reg then (
                   Assert(BinOp(EQ,
                                Opval.exp prog.gamma opval,
                                Opval.const opval))
                   :: asserts
                 ) else
                   asserts)
              []
              eh
          in
          Block([],
                Comment("constrain_mem_accesses (tainted index lval)")
                :: asserts)
        in

        (* here's the main filter logic *)
        if (there_is_a_tainted_mem_reg eh) then
          if has_mem_lval insn.asm_sl then (
            (* if there are writes to symbolic addresses,
               constrain all operands used to calculate addresses
               to have the same concrete value as in trace.
               sound, but may overconstrain.
            *)
            let constraints = constrain_mem_ops eh in
            prog, Some({insn with setup_ir = constraints :: insn.setup_ir})
          ) else
            let valid_mem_ranges = get_valid_mem_ranges eh in
            let asm_sl = add_asserts insn.asm_sl valid_mem_ranges in
            prog, Some({insn with asm_sl = asm_sl})
        else
          prog, insn_opt
end

(** add assertions to constrain memory accesses of tainted indices
    are within the memory ranges specified in the instruction
    operands, or within the provided static_valid_ranges
    XXX experimental. for sym reads, add a type attribute specifying
    the valid index ranges. the idea is to LAZILY convert these to
    asserts, ideally after doing DCE etc. to get rid of irrelevant
    memory references.
*)
class constrain_mem_accesses_tattrs static_valid_ranges  = 
object (self)
  val mutable static_valid_ranges = static_valid_ranges
  method apply (prog:trace_prog) insn_opt (eh:Trace.instruction) (eh_num:int64) = 
    match insn_opt with
    | None -> prog, insn_opt
    | Some(insn) ->
        let there_is_a_tainted_mem_reg eh = 
          opvals_fold_left 
            (fun is_mem_reg flag opval ->
               flag || (is_mem_reg && (opval#taintflag <> 0L)))
            false 
            eh
        in
        (* get ranges of initd memory from opvals.
           list of tuples for which [lo, hi] are initd *)
        let get_valid_mem_ranges eh = 
          opvals_fold_left 
            (fun _ valid_mem_locs opval ->
               match opval#optype with
               | Trace.TMemLoc ->
                   (opval#opaddr, 
                    Int64.pred
                      (Int64.add 
                         opval#opaddr 
                         (Int64.of_int opval#oplen)))
                   :: valid_mem_locs
               | _ -> 
                   valid_mem_locs
            )
            static_valid_ranges 
            eh
        in
        let add_tattrs ir_sl valid_mem_ranges = 
          let tattr = 
            List.fold_left 
              (fun s (lo,hi) ->
                 Printf.sprintf "%s (%Lx,%Lx)" (* XXX inefficient *)
                   s lo hi
              )
              "valid_ranges:"
              valid_mem_ranges
          in

          let vis =  object (self)
            inherit nop_vine_visitor
            method visit_exp exp =
              match exp with
              | Let _ -> 
                  (* shouldn't be too difficult to implement if this comes
                     up. just need to keep a stack of mem_exp lists *)
                  raise (Unimplemented "Let expressions in constrain_mem_accesses_tattr")
              | _ -> DoChildren
                  
            method visit_alrvalue lr =
              ChangeDoChildrenPost(
                lr,
                (fun lr ->
                   match lr with
                   | Mem(v, idx_exp, t) ->
                         Mem(v, idx_exp, TAttr(t, [tattr]))
                   | _ -> 
                       lr
                ))

            method visit_alvalue l =
              match l with
              | Mem _ ->
                  raise (Unimplemented "shouldn't get here")
              | _ -> 
                  DoChildren

            method visit_rlvalue l =
              self#visit_alrvalue l
          end
          in
          List.map (Vine.stmt_accept vis) ir_sl
        in

        let has_mem_lval sl =
          let vis =  object (self)
            inherit nop_vine_visitor
            val mutable flag = false
            method get_flag = flag
            method visit_alvalue lr =
              match lr with
              | Mem _ ->
                  flag <- true;
                  SkipChildren
              | _ ->
                  DoChildren

            method visit_stmt s =
              if flag then
                SkipChildren
              else
                DoChildren
          end
          in
          let _ = List.map (Vine.stmt_accept vis) sl in
          vis#get_flag
        in

        let constrain_mem_ops eh =
          let asserts =
            opvals_fold_left
              (fun is_mem_reg asserts opval ->
                 if opval#taintflag <> 0L && is_mem_reg then (
                   Assert(BinOp(EQ,
                                Opval.exp prog.gamma opval,
                                Opval.const opval))
                   :: asserts
                 ) else
                   asserts)
              []
              eh
          in
          Block([],
                Comment("constrain_mem_accesses (tainted index lval)")
                :: asserts)
        in

        (* here's the main filter logic *)
        if (there_is_a_tainted_mem_reg eh) then
          if has_mem_lval insn.asm_sl then (
            (* if there are writes to symbolic addresses,
               constrain all operands used to calculate addresses
               to have the same concrete value as in trace.
               sound, but may overconstrain.
            *)
            let constraints = constrain_mem_ops eh in
            prog, Some({insn with setup_ir = constraints :: insn.setup_ir})
          ) else
            let valid_mem_ranges = get_valid_mem_ranges eh in
            let asm_sl = add_tattrs insn.asm_sl valid_mem_ranges in
            prog, Some({insn with asm_sl = asm_sl})
        else
          prog, insn_opt
end

(** remove asserts added by constrain_mem_accesses filter.
    this is a temporary hack to get the concretized indexes
    provided by the constrain_mem_accesses filter without 
    the asserts. 
    WARNING: resulting IR is UNSOUND
*)
class remove_mem_constraints =
object(self)
  method apply (prog:trace_prog) insn_opt (eh:Trace.instruction) (eh_num:int64) = 
    match insn_opt with
    | None -> prog, insn_opt
    | Some(insn) ->
        let remove_asserts s =
          let vis = object
            inherit nop_vine_visitor
            method visit_stmt s =
              match s with
              | Assert _ -> 
                  ChangeTo(Comment("assert removed by remove_mem_constraints"))
              | _ -> 
                  DoChildren
          end
          in
          Vine.stmt_accept vis s
        in
          
        let vis = object
          inherit nop_vine_visitor
          method visit_stmt s =
            match s with
            | Block(dl, Comment(cmt)::sl) 
                when String.starts_with cmt "constrain_mem_accesses"
                  ->
                ChangeTo(Block(dl, Comment(cmt)::(List.map remove_asserts sl)))
            | _ ->
                DoChildren
        end
        in
        prog, Some({insn with asm_sl = List.map (Vine.stmt_accept vis) insn.asm_sl})
end
  

(** Use the vine evaluator to reduce stmts to a completely
    straight-line trace. All relevant concrete values must
    be provided. 
    Output trace will be flattened (an unfortunate side-effect),
    cjmps and ijmps will be replaced by assignments to post-condition
    variables,
    other jmps will be removed.
    @param remove_inits Remove concrete initializations to INPUT
    variables.
    @param stmt_block A vine block with concrete initializers,
    with jumps and labels rewritten.
    @return a single block with all relevant decls,
    and statements.
    Also returns a list of generated post-condition variables.
*)
let rewrite_jmps_eval_v2 prog =
  let callback evaluator () =
    let ecode = evaluator#get_ecode () in
    let pc = evaluator#get_pc () in
    let stmt = Vine_eval.pc_to_stmt ecode pc in

    match stmt with
    | Jmp(Name(l)(* as target*)) ->
        (* direct jmp- just remove *)
        [], ()
    | Attr(_, AReturn) ->
        (* jmp caused by a return- just remove. *)
        [], ()
    | Jmp(e) -> 
        (* ijmp- remove and add assert *)
        let target = val_to_const (evaluator#eval_exp e) in
        [Assert(BinOp(EQ,
                      e,
                      target));
        ],
        ()
    | CJmp(c, t_target, f_target) -> 
        (* evaluate cond, add post-condition *)
        let evald_c = val_to_const (evaluator#eval_exp c) in
        [Assert(BinOp(EQ,
                      c,
                      evald_c));
        ],
        ()
    | s -> 
        [s], ()
  in

  let (dl,sl), _, halt_kind = 
    execute_trace_rewrite_cb 
      prog
      callback
      ()
  in

  (dl, sl), halt_kind

(** replaces all stmts in sl that match predicate p with
    a place-holder label. returns a callback which can
    be used to restore these statements.
*)
let remove_matching_stmts p sl =
  let vis =
object (self)
  inherit nop_vine_visitor
  val mutable lbls_to_stmts = PMap.empty
  method get_lbls_to_stmts = lbls_to_stmts
  method visit_stmt s =
    if p s then
      let lbl = newlab "placeholder" in
      lbls_to_stmts <- PMap.add lbl s lbls_to_stmts;
      ChangeTo(Label(lbl))
    else
      DoChildren
end
  in
  let sl = List.map (stmt_accept vis) sl in
  let lbls_to_stmts = vis#get_lbls_to_stmts in

  let restore_stmts sl =
    let vis = object (self)
      inherit nop_vine_visitor
      method visit_stmt s =
        match s with
        | Label(l) when PMap.mem l lbls_to_stmts ->
            ChangeTo(PMap.find l lbls_to_stmts)
        | _ -> 
            DoChildren
    end
    in
    List.map (stmt_accept vis) sl
  in

  (sl, restore_stmts)

let deend_use_flag = ref true
let deend_use b =
  deend_use_flag := b

let execute_insn_rewrite_cb callback prog eh insn acc =
  let asm_dl, asm_sl = insn.asm_dl, insn.asm_sl in
  (* deendianize all *)
  let asm_dl, asm_sl = 
    if !deend_use_flag then deend_prog (asm_dl, asm_sl) prog.mems_arrays
    else asm_dl, asm_sl
  in
  let (asm_dl,asm_sl) = flatten_blocks (asm_dl,asm_sl) in

  (* if we're not using eflag thunks
     temporarily remove assignments to flags *)
  let asm_sl, restore_eflag_assignments =
    if prog.use_thunks then
      asm_sl, Vine_util.id
    else
      let flag_vars = 
        List.fold_left
          (fun flag_vars s ->
             let v = Asmir.gamma_lookup prog.gamma s in
             VarSet.add v flag_vars)
          VarSet.empty
          ["R_CF";"R_PF";"R_AF";"R_ZF";"R_SF";"R_OF";]
      in
      remove_matching_stmts 
        (fun s -> 
           match s with
           | Move(Temp(v),_) ->
               VarSet.mem v flag_vars
           | _ ->
               false)
        asm_sl
  in

  (* create concrete initializers for all opvals *)
  let inits_sl = 
    opvals_fold_left
      (fun _ inits opval ->
         List.rev_append
           (conc_initialize_operand prog.gamma opval)
           inits)
      []
      eh
  in
  let inits_dl, inits_sl = [], inits_sl in
  let inits_dl, inits_sl = 
    if !deend_use_flag then deend_prog ([], inits_sl) prog.mems_arrays
    else inits_dl, inits_sl
  in
  let inits_dl, inits_sl = flatten_blocks (inits_dl,inits_sl) in
  
  (* when initializing a sub-register, add an initializer
     to set the whole register to 0. Otherwise we get
     spurious warnings about accessing an uninitialized register.
     This *may* mask real problems. Could initialize to something
     more "random" than zero, which may help uncover cases
     where those bits are actually accessed.
  *)
  let inits_sl =
    opvals_fold_left
      (fun _ inits_sl opval ->
         let new_inits =
           match opval#optype with
           | Trace.TRegister ->
               let reg = regid_of_addr opval#opaddr in
               let full_reg = regid_to_full reg in
               if reg <> full_reg then (
                 [(write_reg prog.gamma full_reg (const_of_int REG_32
                                                    0) (-1));
                 ]
               ) else
                 []
           | _ -> 
               []
         in
         List.rev_append new_inits inits_sl
      )
      inits_sl
      eh
  in

  (* initializers for potential implicit operands *)
  let inits_sl =
    Move(Temp(Asmir.gamma_lookup prog.gamma "R_GDT"),
         Constant( Int(REG_32,Trace.int64_of_uint32 eh#gdt)))
    :: Move(Temp(Asmir.gamma_lookup prog.gamma "R_LDT"),
            Constant(Int(REG_32, Trace.int64_of_uint32 eh#ldt))) 
    :: Move(Temp(Asmir.gamma_lookup prog.gamma "R_DFLAG"),
            Constant(Int(REG_32, Trace.int64_of_uint32 eh#df)))
    :: Move(Temp(Asmir.gamma_lookup prog.gamma "EFLAGS"),
            Constant(Int(REG_32, Trace.int64_of_uint32 eh#eflags)))
    :: inits_sl
  in

  (* dummy inits for flag thunks, which won't be used. *)
  let inits_sl = 
    Move(Temp(Asmir.gamma_lookup prog.gamma "R_CC_OP"),
         const_of_int REG_32 0)
    :: Move(Temp(Asmir.gamma_lookup prog.gamma "R_CC_NDEP"),
            const_of_int REG_32 0)
    :: Move(Temp(Asmir.gamma_lookup prog.gamma "R_CC_DEP1"),
            const_of_int REG_32 0)
    :: Move(Temp(Asmir.gamma_lookup prog.gamma "R_CC_DEP2"),
            const_of_int REG_32 0)
    :: inits_sl
  in

  (* concrete initializers for eflags and individual flags *)
  let inits_sl = 
    List.append 
      inits_sl
      (get_flags_from_eflags prog.gamma)
  in

  (* add dummy eflag helpers (we're getting actual flag values from
     logged eflags rather than calculating them) *)
  (*   let helpers = Asmir.x86_eflags_helpers () in *)
  let helpers =
    if prog.use_thunks then
      [
        Function("x86g_calculate_eflags_all",
                 None,
                 [],
                 false,
                 Some(Block([],[Return(None)])));
        Function("x86g_calculate_eflags_c",
                 None,
                 [],
                 false,
                 Some(Block([],[Return(None)])));
      ] 
    else
      []
  in

  let asm_sl, extract_marked_block = mark_block asm_sl in

  (* add global variables and fn defs *)
  let (eval_dl, eval_sl) =
    (inits_dl @ asm_dl @ prog.dl,
     helpers @ inits_sl @ asm_sl)
  in
  
(*             Printf.printf "About to give to evaluator:\n"; *)
(*             pp_program print_string (eval_dl, eval_sl); *)

  let (_,asm_sl), acc, halt_kind = 
    execute_trace_rewrite_cb 
      (eval_dl, eval_sl)
      callback
      acc
  in

  (* re-extract the marked block *)
  let asm_sl = extract_marked_block asm_sl in

  (* restore previously removed eflag assignments *)
  let asm_sl = 
    if prog.use_thunks then
      asm_sl
    else
      restore_eflag_assignments asm_sl 
  in

  {insn with asm_dl=asm_dl; asm_sl=asm_sl}, acc, halt_kind


(** trace filter: rewrite to straightline, adding asserts *)
class make_straightline_filter : eh_filter =
object
  val mutable prev_addr = 0L
  val mutable expected_addr = None
  method apply prog insn_opt eh eh_num =
    (match expected_addr with 
     | None -> ()
     | Some(expected_addr) ->
         (if eh#address <> expected_addr then
            D.dprintf 
              "eh %Ld: prev insn at 0x%Lx transitions to 0x%Lx, but this is 0x%Lx\n"
              eh_num
              prev_addr
              expected_addr
              eh#address
         )
    );
    prev_addr <- eh#address;

    match insn_opt with 
    | None -> 
        expected_addr <- None;
        prog, insn_opt
    | Some(insn) ->
        let rec cheap_rewrite newsl_rev sl =
          match sl with
          | (Block (_,b_sl)) as s :: tl -> 
              let has_jmps =
                s_exists 
                  (fun s -> 
                     match s with
                     | Jmp _ | CJmp _ -> true | _ -> false)
                  s
              in
              if has_jmps then
                [], false
              else
                cheap_rewrite (s::newsl_rev) tl
          | [Jmp(Name(l))] -> 
              expected_addr <- Some(label_to_addr l);
              (List.rev newsl_rev), true
          | CJmp _ :: tl -> [], false
          | Jmp _ :: tl -> [], false
          | s :: tl -> cheap_rewrite (s::newsl_rev) tl
          | [] -> 
              D.wprintf "eh %Ld: x86 block didn't end with a jump\n"
                eh_num;
              expected_addr <- None;
              newsl_rev, true
        in
        let asm_sl', success = cheap_rewrite [] insn.asm_sl in
        if success then (
          prog, Some({insn with asm_sl=asm_sl'})
        ) else

          let callback evaluator () =
            let ecode = evaluator#get_ecode () in
            let pc = evaluator#get_pc () in
            let stmt = Vine_eval.pc_to_stmt ecode pc in

            match stmt with
            | Jmp(Name(l)(* as target*)) ->
                (* direct jmp- just remove *)
                [], ()
            | Attr(_, AReturn) ->
                (* jmp caused by a return- just remove. *)
                [], ()
            | Jmp(e) -> 
                (* ijmp- remove and add assert *)
                let target = val_to_const (evaluator#eval_exp e) in
                [Assert(BinOp(EQ,
                              e,
                              target));
                ],
                ()
            | CJmp(c, t_target, f_target) -> 
                (* evaluate cond, add post-condition *)
                let evald_c = val_to_const (evaluator#eval_exp c) in
                [Assert(BinOp(EQ,
                              c,
                              evald_c));
                ],
                ()
            | s -> 
                [s], ()
          in

          let insn, _, halt_kind =
            execute_insn_rewrite_cb callback prog eh insn ()
          in

          (match halt_kind with
           | NormalExit ->
               D.wprintf 
                 "eh %Ld: evaluator halted when executing inst"
                 eh_num
           | NoMoreStmts ->
               D.wprintf
                 "eh %Ld: evaluator ran out of stmts"
                 eh_num
           | MissingLabel s ->
               expected_addr <- Some(label_to_addr s));


          (prog, Some(insn))
end

(** trace filter: handle outsw instructions.
    rep outsw's are translated to decrement ECX appropriately.
    outsw's are dropped.
*)
class handle_outsw : eh_filter =
object
  method apply prog insn_opt eh eh_num =
    let is_outsw rawbytes =
      match opcode rawbytes with
      | 0x6e, _
      | 0x6f, _ ->
          true
      | _ ->
          false
    in

    (* XXX not fully general. could be multiple prefixes *)
    let has_rep rawbytes =
      match int_of_char (rawbytes.(0)) with
      | 0xf3 -> true
      | _ -> false
    in

    match insn_opt with
    | None -> 
        prog, None
    | Some insn ->
        if is_outsw eh#rawbytes then
          (if has_rep eh#rawbytes then
            let dec_lbl = newlab "do_dec" in
            let out_lbl = newlab "out" in
            let ecx = Temp(Asmir.gamma_lookup prog.gamma "R_ECX") in
            let custom_asm = 
              [
                CJmp(BinOp(EQ,
                           Lval(ecx),
                           const_of_int REG_32 0),
                     Name(out_lbl),
                     Name(dec_lbl));
                Label(dec_lbl);
                Move(ecx,
                     BinOp(MINUS,
                           Lval(ecx),
                           const_of_int REG_32 1));
                Label(out_lbl);
                Jmp(Name("pc_0x0")); (* dummy jump *)
              ]
            in
            prog, Some({insn with asm_sl = custom_asm})
           else
             prog, None
          )
        else
          prog, insn_opt
end

(* should be *after* any filters that could generate new memory
   accesses. also, will work better if *after* something to concretize
   memory indexes.
   XXX only looks at Mems for now, so call before any would be
   converted to Arrays.
   XXX add support for shared memory regions (e.g. kernel region)
*)
class rewrite_mem_by_pid(mem_t) : eh_filter =
object(filter_self)
  val mutable pid_to_mem = PMap.empty (* int pid to vine memory var *)

  method private add_mem_var prog (pid:int) =
    (* generate var *)
    let name = Printf.sprintf "mem_p%d" pid in
    let v = newvar name mem_t in

    (* add to our mapping *)
    pid_to_mem <- PMap.add pid v pid_to_mem;

    (* add to prog dl *)
    let prog = {prog with dl = v :: prog.dl} in

    (* add to prog gamma *)
    Asmir.gamma_extend prog.gamma name v;

    prog

  method private ensure_mem_var prog (pid:int) =
    if not (PMap.mem pid pid_to_mem) then
      filter_self#add_mem_var prog pid
    else
      prog

  method private get_mem_var (pid:int) =
    PMap.find pid pid_to_mem 

  method apply prog insn_opt eh eh_num =
    match insn_opt with
    | None -> 
        prog, None
    | Some insn ->
        let rewrite_sl sl =
          (* rewrite all accesses to per-pid mem objects *)
          let pid_rewriter = object (self)
            inherit nop_vine_visitor

            method rewrite_arlvalue lv =
              match lv with
              | Mem ((_,_, TMem _),i,w) ->
                  ChangeDoChildrenPost(
                    Mem(filter_self#get_mem_var eh#pid, i, w),
                    Vine_util.id
                  )
              | _ ->
                  DoChildren

            method visit_rlvalue lv =
              self#rewrite_arlvalue lv

            method visit_alvalue lv =
              self#rewrite_arlvalue lv
          end
          in
          let sl = List.map (stmt_accept pid_rewriter) sl in

          (* XXX add duplicate writes for shared memory regions *)

          sl
        in

        let prog = filter_self#ensure_mem_var prog eh#pid in
        let insn = {insn with 
                      asm_sl = rewrite_sl insn.asm_sl;
                      setup_ir = rewrite_sl insn.setup_ir}
        in

        prog, Some(insn)
end

(**
   use evaluator to rewrite all Mem exps to have concrete index.
   @param prog program to rewrite
   @return program with concrete memory indices
*)
class conc_idx_filter : eh_filter =
object
  method apply prog insn_opt eh eh_num =
    match insn_opt with
    | None ->
        prog, insn_opt
    | Some(insn) ->
        let callback (evaluator:Vine_ceval.concrete_evaluator) _ =
          (* rewrite mems to have concrete indexes. *)
          let vis = object (self)
            inherit nop_vine_visitor
            method visit_exp exp =
              match exp with
              | Let _ -> 
                  raise (Unimplemented "Let expressions in trace_to_conc_idx")
              | _ -> DoChildren

            method to_conc e =
              let exp = evaluator#eval_exp e in
              let exp =
                match exp with
                | Vine_ceval.Int(it, iv) -> Constant(Vine.Int(it, iv))
                | _ -> raise (Invalid_argument "should be int")
              in
              exp
                  
(*             method mem_to_conc_idx m = *)
(*               match m with *)
(*               | Mem(v,idx,t) -> *)
(*                   let idx_val = evaluator#eval_exp idx in *)
(*                   let idx_exp = *)
(*                     match idx_val with *)
(*                     | Vine_ceval.Int(it, iv) -> Constant(Vine.Int(it, iv)) *)
(*                     | _ -> raise (Invalid_argument "should be int") *)
(*                   in *)
(*                   Mem(v,idx_exp,t) *)
(*               | _ -> raise (Invalid_argument "expected mem") *)

            method visit_alvalue l =
              match l with
              | Mem(v,idx,t) ->
                  ChangeTo(Mem(v, self#to_conc idx, t))
                    (*                   ChangeTo(self#mem_to_conc_idx l) *)
              | _ ->
                  DoChildren

            method visit_rlvalue l =
              match l with
              | Mem(v,idx,t) ->
                  ChangeTo(Mem(v, self#to_conc idx, t))
(*                   ChangeTo(self#mem_to_conc_idx l) *)
              | _ ->
                  DoChildren
          end
          in

          let ecode = evaluator#get_ecode () in
          let pc = evaluator#get_pc () in
          let stmt = Vine_eval.pc_to_stmt ecode pc in
          let stmt = stmt_accept vis stmt in
          ([stmt], ())
        in

        let there_is_a_tainted_mem_reg eh = 
          opvals_fold_left 
            (fun is_mem_reg flag opval ->
               flag || (is_mem_reg && (opval#taintflag <> 0L)))
            false 
            eh
        in

        let insn =
          if there_is_a_tainted_mem_reg eh then
            insn
          else
            let insn, _, _ =
              execute_insn_rewrite_cb callback prog eh insn ()
            in
            insn
        in

        (prog, Some(insn))
end

(**
   Removes instructions that VEX/Vine do not currently support.
   Removing an instruction from the IR can have unexpected consequences, 
     but sometimes is the only way to continue some analysis
   Use with care!
*)
(* TODO: This filter should remove all prefixes first *)
class unknowns_filter : eh_filter =
object
  method apply prog insn_opt eh eh_num =
    let is_unknown rawbytes =
      let first_byte = int_of_char rawbytes.(0) in
      let second_byte =
        if ((Array.length rawbytes) > 1) then int_of_char rawbytes.(1)
        else (-1)
      in
      let third_byte =
        if ((Array.length rawbytes) > 2) then int_of_char rawbytes.(2)
        else (-1)
      in
      match (first_byte,second_byte,third_byte) with
      (* int *)
      | (0xcc,_,_) | (0xcd,_,_) | (0xce,_,_) -> true
      (* sysenter *)
      | (0xf,0x34,_) -> true
      (* out *)
      | (0xe6,_,_) | (0xe7,_,_) | (0xee,_,_) | (0xef,_,_) -> true
      (* rdtsc *)
      | (0xf,0x31,_) -> true
      (* opsize movaps *)
      | (0x66,0xf,0x28) -> true
      (* opsize pushf/popf *)
      | (0x66,0x9c,_) | (0x66,0x9d,_) -> true
      (* floating point instructions *)
      | (0xd8,_,_) | (0xd9,_,_) | (0xda,_,_) | (0xdb,_,_) | (0xdc,_,_)
      | (0xdd,_,_) | (0xde,_,_) | (0xdf,_,_) -> true
      (* OTHER *)
      | _ -> false
    in
    let cmt = Comment("Unsupported instruction removed by unknowns_filter") in
    if is_unknown eh#rawbytes then (
      match insn_opt with
       | None -> prog,insn_opt
       | Some(insn) ->
          let insn_opt =
            Some({
              label = insn.label;
              cmt = insn.cmt;
              asm_sl = [cmt];
              asm_dl = [];
              setup_ir = [];
              tainted_eflags = false;
            })
          in
          prog, insn_opt
    )
    else prog, insn_opt
end


 
type disasm_context = {
   mutable prog : trace_prog;
   mutable r_insns : trace_insn list;
   filters : eh_filter list;
}

(** creates a disassembly context that stores all program info as a trace is 
  disassembled using a trace_fold function *)
let create_disasm_ctx ?(use_thunks = true) ?(gammaparam = None) filters =
  Libasmir.set_use_eflags_thunks use_thunks;
  let mem_arr =
    match Asmir.x86_mem with
    | (_,s,Vine.TMem(t,_)) ->
        newvar
          (s ^ "_arr")
          (Vine.Array(Vine.REG_8,
                      Vine.array_idx_type_to_size t))
    | _ -> failwith "memory type mismatched"
  in
  let arch = Asmir.arch_i386; (* FIXME *) in
  let prog =
    {
      use_thunks = use_thunks;
      arch = arch;
      gamma = (
        match gammaparam with
            None -> Asmir.gamma_for_arch arch
          | Some(gm) -> gm
      );
      dl = mem_arr :: Asmir.decls_for_arch arch;
      mems_arrays = [(Asmir.x86_mem, mem_arr)];
      prog_setup_ir = [];
    }
  in
  {
     prog = prog;
     r_insns = [];
     filters = filters;
  }

(** disassembles one instruction from the trace as part of trace folding *)
let handle_eh disasm_ctx eh eh_num =
  (* D.dprintf "Handling trace entry %Ld" eh_num; *)

  let prog = disasm_ctx.prog in

  (* iterate over filters *)
  let prog, insn_opt =
    List.fold_left
      (fun (prog,insn_opt) f -> (f#apply prog insn_opt eh eh_num))
      (prog, None)
      disasm_ctx.filters
  in

  let r_insns =
    match insn_opt with
    | None -> disasm_ctx.r_insns
    | Some(insn) -> insn::disasm_ctx.r_insns
  in
  let _ = disasm_ctx.prog <- prog in
  let _ = disasm_ctx.r_insns <- r_insns in
  disasm_ctx


(** opens a trace, processes each instruction with the list of
    filter objects, and returns the list of processed instructions
    and global information about the program.
    @param stop_addr optional, stop processing at this PC
    @param stop_addr_ctr optional, stop processing after seen this PC ctr times
    @param start_ctr optional, start processing after seing this number 
      of instructions
    @param stop_ctr optional, stop processing after seing this number of 
      instructions
    @param use_thunks optional, call eflag thunks instead of inlining
    them.
    @param tracename file name of trace to be processed
    @param filters list of filter objects to apply to each instruction
*)
let disasm_trace ?(stop_l = []) ?(start_ctr = 1L) ?(stop_ctr = 0L) ?(use_thunks = true) ?(gammaparam = None) tracename filters =

  (* Create disasm context *)
  let disasm_ctx = 
    create_disasm_ctx ~use_thunks:use_thunks ~gammaparam:gammaparam filters 
  in

  (* Open trace *)
  let tif = Trace.open_trace tracename in 
  let _ = 
    if start_ctr <> 1L then
      tif#seek_instruction start_ctr
  in

  (* Iterate over trace *)
  let trace_fold = match (stop_l,stop_ctr) with 
    | ([],0L) -> Trace.trace_fold
    | ([],_) -> Trace.trace_fold_until_ctr stop_ctr
    | (_,_) -> Trace.trace_fold_until_addr stop_l
  in
  let disasm_ctx = 
    trace_fold 
      handle_eh 
      disasm_ctx
      tif
  in
  (* Close trace *)
  Trace.close_trace tif;
  (disasm_ctx.prog, List.rev disasm_ctx.r_insns)
;;

let create_assert_variable (dl, sl) =
  let tmpvarlist = ref [] in
  let vis =
object (self)
  inherit nop_vine_visitor
  val mutable last_label = ""
  val mutable label_counter = 0
  method visit_stmt s =
    match s with
    | Label(label_str) when (String.sub label_str 0 5 = "pc_0x") ->
        last_label <- label_str;
        label_counter <- 0;
        SkipChildren
    | Assert(c) ->
        let token_list = Str.split (Str.regexp "_") last_label in
        let name =
          Printf.sprintf "cond_%06d_%s_%02d"
            (int_of_string (List.nth token_list 2)) 
	    (List.nth token_list 1) label_counter
        in
        let _ = label_counter <- label_counter + 1 in
        let tempvar = newvar name REG_1 in
          tmpvarlist := tempvar :: !tmpvarlist;
          ChangeTo(
            Block([],
                 [Move(Temp(tempvar), c); Assert(Lval(Temp(tempvar)))]
            ))
    | _ ->
        DoChildren
end
  in
  let sl = List.map (stmt_accept vis) sl in
  let dl = List.append !tmpvarlist dl in
  (dl, sl)

(** Take the output of disasm_trace and create an IR program.
    @param deend optional, deendianize all memory accesses.
    you MUST use this to deendianize the trace, rather than
    doing a separate deendianization pass. This uses info
    in trace_prog to make sure that memory variable names
    are consistent.
    @param trace_prog global info about the trace, generated by
    disasm_trace
    @param trace_insns list of processed instructions, generated
    by disasm_trace
*)
let trace_to_prog ?(deend = true) trace_prog trace_insns =
  (* paste together all the sl's *)
  let sl = 
    List.fold_left
      (fun sl trace_insn -> 
         [Label(trace_insn.label);
          Comment(trace_insn.cmt);
          Comment("Filter IRs:");]
         @
           trace_insn.setup_ir
         @
           [Comment("ASM IR:");
            Block(trace_insn.asm_dl, trace_insn.asm_sl)]
         @
           sl)
      []
      (List.rev trace_insns)
  in
  let dl = trace_prog.dl in

  (* add flag helpers *)
  let sl =
    if trace_prog.use_thunks then
      let helpers = Asmir.x86_eflags_helpers () in
      helpers @ sl
    else
      sl
  in

  (* add setup ir *)
  let sl = trace_prog.prog_setup_ir @ sl in
    
  (* deendianize all *)
  let dl, sl =
    if deend then
      deend_prog (dl, sl) trace_prog.mems_arrays
    else
      dl, sl
  in
  (dl, sl)

let prop_constants prog mem_idxs_only =
  let callback (evaluator:Vine_ceval.concrete_evaluator) defd_locs =
    (* need to do this to update from visitor *)
    let defd_tmps, defd_mems = defd_locs in
    let defd_tmps = ref defd_tmps in
    let defd_mems = ref defd_mems in

    let exp_to_const exp =
      match evaluator#eval_exp exp with
      | Vine_ceval.Int(it, iv) -> Constant(Vine.Int(it, iv))
      | _ -> raise (Invalid_argument "should be int")
    in

    let rec stmt_is_defd s =
      let rv = ref true in
      let vis = object
        inherit nop_vine_visitor
        method visit_rlvalue lv =
          (match lv with
           | Temp(v) ->
               let tmp_defd = VarSet.mem v !defd_tmps in
               D.dprintf "Tmp %s defd: %b" (var_to_string v) tmp_defd;
               rv := !rv && tmp_defd
           | Mem(v,e,t) ->
               let index_defd = is_defd e in
               let index = if index_defd then exp_to_const e else e in
               let loc_defd = index_defd && (PMap.mem index !defd_mems) in
               D.dprintf "MemLoc @ %s defd: %b" 
                 (exp_to_string index)
                 loc_defd;
               rv := !rv && loc_defd
          );
          DoChildren

        method visit_exp e =
          match e with
          | Unknown _ -> 
              rv := false;
              SkipChildren
          | _ ->
              DoChildren

        method visit_stmt s =
          match s with
          | Move(Temp _, rhs) ->
              rv := !rv && is_defd rhs;
              DoChildren
          | Move(Mem(_,e,_), rhs) ->
              rv := !rv && is_defd rhs && is_defd e;
              DoChildren
          | Special _ ->
              rv := false; (* XXX do anything else with specials? *)
              SkipChildren
          | _ ->
              DoChildren
      end
      in
      let _ = stmt_accept vis s in
      !rv
    and is_defd e =
      stmt_is_defd (ExpStmt(e))
    in
    
    let define lv =
      match lv with
      | Temp(v) ->
          D.dprintf "Defining tmp %s" (var_to_string v);
          defd_tmps := VarSet.add v !defd_tmps;
      | Mem(v,e,t) ->
          if is_defd e then
            let e = exp_to_const e in
            D.dprintf "Defining mem loc %s" (exp_to_string e);
            defd_mems := PMap.add e () !defd_mems
          else
            (* write to unknown address. everything gets invalidated *)
            defd_mems := PMap.empty
    in

    let undefine lv =
      match lv with
      | Temp(v) ->
          D.dprintf "Undefining tmp %s" (var_to_string v);
          defd_tmps := VarSet.remove v !defd_tmps;
      | Mem(v,e,t) ->
          if is_defd e then
            let e = exp_to_const e in
            D.dprintf "Undefining mem loc %s" (exp_to_string e);
            defd_mems := PMap.remove e !defd_mems
          else (
            (* write to unknown address. everything gets invalidated
            *)
            D.dprintf "Undefining all mem locs";
            defd_mems := PMap.empty
          )
    in
    
    let prop_const_vis = object (self)
      inherit nop_vine_visitor
      method visit_exp exp =
        match exp with
        | Let _ -> 
            raise (Unimplemented "Let expressions in prop_constants")
        | _ -> DoChildren
        
      method conc_mem_idx exp =
        match exp with
        | Mem(v,e,t) ->
            if is_defd e then
              ChangeTo(Mem(v,exp_to_const e,t))
            else
              DoChildren
        | _ -> DoChildren

      method visit_alvalue lv =
        self#conc_mem_idx lv

      method visit_rlvalue rv =
        self#conc_mem_idx rv
        
      method visit_stmt s =
        match s with
        | Move _ ->
            ChangeDoChildrenPost(
              s,
              fun mov ->
                match mov with
                | Move(lhs, rhs) ->
                    if is_defd rhs then
                      let rhs' = exp_to_const rhs in
                      define lhs;
                      if mem_idxs_only then
                        mov
                      else (
                        D.dprintf "Replacing rhs %s with %s"
                          (exp_to_string rhs)
                          (exp_to_string rhs');
                        Move(lhs, rhs'))
                    else (
                      D.dprintf "Can't replace rhs %s"
                        (exp_to_string rhs);
                      undefine lhs;
                      mov)
                | _ -> failwith "expected a mov"
            )
        | _ -> 
            DoChildren
    end
    in

    let ecode = evaluator#get_ecode () in

    (* simplify asserts if possible, but don't pass them on
       to evaluator *)
    let rec f sl =
      let pc = evaluator#get_pc () in
      let stmt = Vine_eval.pc_to_stmt ecode pc in
      let stmt = stmt_accept prop_const_vis stmt in
    
      (* skip past undefined statements *)
      if not (stmt_is_defd stmt) then (
        evaluator#set_pc (pc + 1);
        f (stmt::sl)
      ) else (
        stmt::sl
      )
    in
    let sl = List.rev (f []) in
    sl, (!defd_tmps, !defd_mems)
  in
  let old_allow = !Vine_ceval.allow_uninitialized_reads in
  Vine_ceval.allow_uninitialized_reads := true;
  let prog, _, _ = 
    execute_trace_rewrite_cb 
      prog 
      callback
      (VarSet.empty, PMap.empty)
  in
  Vine_ceval.allow_uninitialized_reads := old_allow;
  prog

module StringMap = Map.Make(String) ;;  

(** Rewrites labels and jumps.
    Labels are renamed to make them unique.
    All jumps are rewritten to jump forward in the trace.
    Jumps to non-existent labels instead jump to the
    special label FAIL.
    @param stmt_block A Vine Block of IR stmts from an execution
    trace.
    @return A Vine Block with jumps and labels rewritten
*)
let rewrite_labels prog =
  let rewrite_jmp_label l labelcounts =
    try 
      l ^ "z" ^ (string_of_int (StringMap.find l labelcounts))
    with
    | Not_found -> l ^ "_FAIL"
  in

  let rec _rewrite_labels _stmts labelcounts = 
    List.fold_left
      (fun (a_rewritten, a_labelcounts) stmt ->
         match stmt with
         | Vine.Label(l) -> (
             try
               let i = StringMap.find l a_labelcounts in
               ((Vine.Label(l ^ "z" ^ string_of_int (i+1)) :: a_rewritten), 
                StringMap.add l (i+1) a_labelcounts)
             with
             | Not_found -> 
                 ((Vine.Label(l ^ "z0") :: a_rewritten),
                  StringMap.add l 0 a_labelcounts)
           )

         (* let indirect jmps fall through *)
         | Vine.Jmp(Vine.Name(l)) -> 
             ((Vine.Jmp(Vine.Name(rewrite_jmp_label l a_labelcounts)) 
               :: a_rewritten), 
              a_labelcounts)

         | Vine.CJmp(c,Vine.Name(l1),Vine.Name(l2)) ->
             (Vine.CJmp(c,
                        Vine.Name(rewrite_jmp_label l1 a_labelcounts), 
                        Vine.Name(rewrite_jmp_label l2 a_labelcounts)
                       )
              :: a_rewritten),
             a_labelcounts
               
         (* not handling indirect cjmps. shouldn't get these. *)
         | Vine.CJmp(_,_,_) -> raise (Invalid_argument "_rewrite_labels")

         | Vine.Block(d,s) ->
             let s', a_labelcounts' = _rewrite_labels s a_labelcounts in
             (Vine.Block(d,s') :: a_rewritten),
             a_labelcounts'
         | _ -> (stmt :: a_rewritten, a_labelcounts)
      )
      ([], labelcounts)
      (List.rev _stmts)
  in
  match prog with (dl, stmts) ->
    let final_stmts, _ = _rewrite_labels stmts StringMap.empty in
    (dl, final_stmts)

(** rewrite assignments to post_vars as conditional jumps,
    such that execution will jump straight to a FAIL node
    as soon as a post_var is not satisfied.
    @param stmt_block Vine Block to examine
    @return rewritten Vine Block
*)
let post_vars_to_cjmps post_var stmt_block =
  let rec loop newstmts stmts = 
    match stmts with
    | Block(dl, sl)::st ->
        let block_newstmts = loop [] sl in
        loop
          (Block(dl,block_newstmts)::newstmts)
          st
    | (Move((Temp(i, v, REG_1)) as post_tmp, rhs)) as mov::st 
        when (String.starts_with v "post") ->

        let next_label_str = newlab "continue_" in
        let exit_block = 
          [mov;
           CJmp(Lval(post_tmp), Name(next_label_str), Name("FAIL"));
           Label(next_label_str)] in
        loop
          (List.rev_append exit_block newstmts)
          st
    | s :: st ->
        loop (s::newstmts) st
    | [] -> 
        List.rev newstmts
  in
  let end_block =
    [Jmp(Name("DONE"));
     Label("FAIL");
     Move(Temp(post_var), Constant(Int(REG_1, 0L)));
     Label("DONE")] in

    match stmt_block with
	Block(dl, sl) ->
	  let newsl = loop [] sl in
	    Block(dl, newsl @ end_block)
      | _ -> raise (Invalid_argument "post_vars_to_cjmps")

(** Read a TEMU-generated execution trace,
    eliminating duplicated instructions    

    Statements from disassembled x86 are not modified.
    In particular, there may be jumps to non-existent labels, etc.

    @param tracename file name of execution trace
    @param skip_addrs list of instruction addresses to ignore.
    (using skip_addrs is a hack, and not recommended)
    @return a vine block of unique instructions in the trace
*)
let taintlog_to_ir_prog tracename skip_addrs =
  let tif = Trace.open_trace tracename in
  let arch = Asmir.arch_i386 in (* FIXME *)

  let rec handle_entries stmts visited_ips =
    let inst_o = 
      try 
        Some(tif#read_instruction)
      with
	| IO.No_more_input -> None
    in
      match inst_o with
	| None -> (stmts, visited_ips)
	| Some(inst) ->
            if (List.mem inst#address visited_ips)
            then 
              handle_entries stmts visited_ips
            else
              let inst_stmts = 
                Asmir.asm_bytes_to_vine 
                  (Asmir.gamma_for_arch arch)
		  arch
                  inst#address 
		  inst#rawbytes 
	      in
	      let deended_stmts = inst_stmts in 
	      (* let (_, deended_stmts) = 
                Vine_memory2array.coerce_prog([],
                                              inst_stmts) 
              in *)
	      handle_entries (deended_stmts@stmts) (* XXX ick *)
		(inst#address::visited_ips)
		  
  in
  let rstmts, visited_ips = 
    handle_entries [] [] in
  let stmts = List.rev rstmts in
  let decls = 
    Asmir.x86_mem ::
      (List.rev_append 
          Asmir.x86_regs []) in
    (decls, stmts)
;;

(** rewrite assert stmts to instead assign to variable "post".
    this is for backwards compatibility. *)
let asserts_to_post (dl, sl) =
  let post_var = newvar "post" REG_1 in
  let vis =
object (self)
  inherit nop_vine_visitor
  method visit_stmt s =
    match s with
    | Assert(c) ->
        ChangeTo(Move(Temp(post_var), 
                      BinOp(BITAND,
                            Lval(Temp(post_var)),
                            c)))
    | _ -> 
        DoChildren
end
  in
  let sl = List.map (stmt_accept vis) sl in
  let dl = post_var :: dl in
  let sl = (Move(Temp(post_var), const_of_int REG_1 1)) :: sl in
  (dl, sl), post_var

(* Modified version of asserts_to_post that works on SSA *)
let asserts_to_post_ssa (dl, sl) =
  let vis =
object (self)
  inherit nop_vine_visitor

  val mutable post_var_l = [(newvar "post" REG_1)];

  method get_post_var_l = post_var_l

  method visit_stmt s =
    match s with
    | Assert(c) ->
        let post_var = newvar "post" REG_1 in
        let curr_post_var = List.hd post_var_l in
        post_var_l <- post_var :: post_var_l;
        let post_stmt =
          Move(Temp(post_var),BinOp(BITAND,Lval(Temp(curr_post_var)),c))
        in
        ChangeTo(post_stmt)
    | _ ->
        DoChildren
end
  in
  let sl = List.map (stmt_accept vis) sl in
  let post_var_l = vis#get_post_var_l in
  let last_post_var = List.hd post_var_l in
  let post_var_l = List.rev post_var_l in
  let first_post_var = List.hd post_var_l in
  let dl = List.rev_append dl post_var_l in
  let sl = (Move(Temp(first_post_var), const_of_int REG_1 1)) :: sl in
  (dl, sl), last_post_var

