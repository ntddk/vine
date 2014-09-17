(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

open Vine
open Vine_util
open Exectrace ;;
module List = ExtList.List ;;
module String = ExtString.String ;;

type cmdlineargs_t = {
(*   mutable bins_file : string; *)
(*   mutable irin_file : string; *)
(*   mutable first_eip : int64; *)
(*   mutable last_eip : int64; *)
(*   mutable taintcheck : bool; *)
(*   mutable skip_addrs : int64 list; *)
(*   mutable block_by_block : bool; *)
  mutable trace_file : string;
  mutable irout_file : string;
  mutable wpout_file : string;
  mutable stpout_file : string;
  mutable state_file : string;
  mutable state_ranges : (int64 * int64) list;
  mutable concrete : bool;
  mutable typecheck : bool;
  mutable eval : bool;
  mutable dead : bool;
  mutable early_exit : bool;
  mutable simplify : bool;
  mutable include_all : bool;
  mutable use_thunks : bool;
  mutable use_post_var : bool;
  mutable assertion_on_var : bool; 
  mutable deend : bool;
  mutable deend_multi : bool;
  mutable verify_expected : bool;
  mutable conc_mem_idx : bool;
  mutable prop_consts : bool;
  mutable remove_unknowns : bool;
  mutable flatten : bool;
} ;;

let findvar dl vname =
  List.find
    (fun (_,s,_) -> s = vname)
    dl


let debug s =
  print_endline s

let rec is_straightline stmts =
  match stmts with
  | Jmp(_)::tail -> false
  | CJmp(_,_,_)::tail -> false
  | Block(dl, sl)::tail ->
      is_straightline (List.rev_append sl tail)
  | Attr(s,_) :: tail ->
      is_straightline (s::tail)
  | s :: tail ->
      is_straightline tail
  | [] -> true

let writestp filename exp =
  let fd = open_out filename in
    debug("Writing STP file '"^filename^"'...");
    flush stdout;
    Stp.to_file fd exp;
    close_out fd

let writeir filename prog =
  let oc = open_out filename in
  let ft = Format.formatter_of_out_channel oc in
  format_program ft prog;
  Format.pp_print_newline ft ()

let findpost sl =
  let retval = ref (0, "", REG_1) in
  let revsl = List.rev sl in
  let rec is_post stmt =
    match stmt with
	Move(lv, rv) -> ( 
	  match lv with
	      Temp((_, name, _) as var) when String.length name >= 4 -> 
		let result = String.sub name 0 4 = "post" in
		  if result then retval := var; 
		  result
	    | _ -> false
	)
      | Block(dl, sl) -> (
	  try  
	    let revsl = List.rev sl in
	      ignore(List.find is_post revsl);
	      true
	  with
	      Not_found -> false 
	)
      | _ -> false
  in
    try 
      match List.find is_post revsl with
	  Move(Temp _, _) -> Lval(Temp(!retval))
	| _ -> raise (Invalid_argument "findpost")
    with 
       Not_found -> Constant(Int(REG_1, 1L))
	  
let inline_func prog =
  let (dl, sl) = prog in
  let funchash = Hashtbl.create 1000 in
  let rec process_stmt stmt = 
    match stmt with
      | Block(dl, sl) -> 
	  let newsl = List.map process_stmt sl in
	    Block(dl, newsl)
      | Function(name, t, dl, ext, ostmt) ->
	  (
	    match ostmt with
		None -> stmt
	      | Some(blk) -> Hashtbl.add funchash name blk; stmt
	  )
      | Call (_, e, _)  -> 
	  (
	    match e with
		Name(l) ->
		  let blk = Hashtbl.find funchash l in
		    process_stmt blk
	      | _ -> stmt
	  )
      | _ -> stmt
  in
  let inlinedsl = List.map process_stmt sl in
    (dl, inlinedsl)

let appreplay args trace =
(*   uncomment this to inline functions *)
(*   let trace = inline_func trace in *)
    (* to SSA *)
  let cfg = Vine_cfg.prog_to_cfg trace in
(*   let (dl,sl) = trace in  *)
(*   let qvar = findvar dl "post" in *)
(*   let _ = Vine_dataflow.do_dce cfg ~globals:[qvar] in *)
  let () = Vine_cfg.coalesce_bb cfg in
  let cfg = Ssa.cfg2ssa cfg in
  let cfg = Ssa.to_vine cfg in
  let trace = Vine_cfg.normal_cfg_to_prog cfg in
  let (dl,sl) = trace in
  let () = debug "Tranlating trace to GCL..." in
  let gcl = 
    if is_straightline sl then
      Gcl.of_straightline sl
    else
      Gcl.of_trace trace in
(*   let qvar = findvar dl "post" in *)
  let q = findpost sl in
(*   let q = Lval(Temp(qvar)) in *)
  let () = debug "Computing wp..." in
  let (simp1, simplast) =
    if args.simplify
    then
      (Vine_opt.constant_fold_more (fun _->None),
       Vine_opt.simplify_faster <@ Vine_alphavary.alpha_vary_exp)
    else (id,id)
  in
  let wp = simplast(Wp.calculate_wp simp1 q gcl) in
  let () =
    if args.wpout_file <> "" then writeir args.wpout_file (dl, [ExpStmt wp])
  in
  let () =
    if args.stpout_file <> "" then writestp args.stpout_file wp
  in
  (* let () = print_endline "Computing independent clauses..." in
  let indeps = Vine_indepclauses.split_indep wp in
  let write_one_indep i e =
    let si = string_of_int i in
    let () =
      if args.wpout_file <> ""
      then writeir (args.wpout_file^"."^si) (dl, [ExpStmt e])
    in
      writestp (model_output^"."^si) e
  in
    List.iteri write_one_indep indeps *)
   () 
  (* disabling for now. getting right answer via stp file,
   * and wrong one via library api *)
  (*
  let () = debug "Building expression in STP library..." in
  let vc = Stpvc.create_validity_checker () in
  let ctx = Vine_stpvc.new_ctx vc (get_req_ctx wp) in
  let stp_wp = Vine_stpvc.vine_to_stp vc ctx wp in
  let () = debug "Querying STP..." in
  if Stpvc.query vc (Stpvc.e_not vc (Stpvc.e_bvbitextract vc stp_wp 0))
  then debug "Valid"
  else (debug "Invalid";
	(* this is for debugging only. Do something cleaner later. *)
	flush_all();Libstp.vc_printCounterExample vc)
  *)
;;

let lval_to_decl lv =
  match lv with
  | Temp(v) -> v
  | Mem(v, _,_) -> v

let unwrap_lets exp =
  let decl_set = ref VarSet.empty in

  let vis =
object(s)
  inherit nop_vine_visitor
  method visit_alvalue l =
    let v = lval_to_decl l in
    assert (not (VarSet.mem v !decl_set));
    decl_set := VarSet.add v !decl_set;
    DoChildren

  method visit_rlvalue l = 
    s#visit_alvalue l
end 
  in

  let rec loop stmts exp =
    match exp with
    | Let(lv, rv, lexp) ->
        let mov = Move(lv,rv) in
        let _ = stmt_accept vis mov in
        loop
          (mov::stmts)
          lexp
    | _ -> 
        let decls = 
          VarSet.elements !decl_set in
        (List.rev stmts), decls, exp
  in
  loop [] exp
    

let deadcode trace =
  let (dl, sl) = trace in
  let post_var = findvar dl "post" in
  let mem_var = findvar dl "mem_arr" in

  let cfg = Vine_cfg.prog_to_cfg trace in
  ignore(Vine_dataflow.do_dce cfg ~globals:[post_var; mem_var]); 
  Vine_cfg.normal_cfg_to_prog cfg

(*   let exit_node = Vine_cfg.exit_node cfg in *)
(*   let entry_node = Vine_cfg.entry_node cfg in *)
(*   let gcl = Gcl.of_cfg cfg exit_node entry_node in *)
(*   let q = Lval(Temp(post_var)) in *)
(*   let simp = (fun x->x) in *)
(*   let wp = Wp.calculate_wp simp q gcl in *)
(*   let stmts, decls, exp = unwrap_lets wp in *)
(*   (\*  Vine.Block(decls, stmts @ [Move(Temp("post", bool_t), exp)]) *\) *)
(*   (\* drop the last exp, which is just "post" *\) *)
(*   (decls, stmts) *)
;;

if true
then Gc.set
  { (Gc.get()) with
    (* Gc.verbose = 0x00c; *)
    Gc.max_overhead = 999999 };;

let _ = Sys.signal Sys.sigusr1 (Sys.Signal_handle (fun _ ->
  prerr_endline "Manually triggered heap compaction."; Gc.compact())) ;;


let parse_cmdline =
  let cmdlineargs = {
(*     bins_file = ""; *)
(*     irin_file = ""; *)
(*     first_eip = Int64.zero; *)
(*     last_eip = Int64.zero; *)
(*     taintcheck = false; *)
(*     skip_addrs = []; *)
(*     block_by_block = false; *)
    trace_file = "";
    irout_file = "";
    wpout_file = "";
    stpout_file = "";
    state_file = "";
    state_ranges = [];
    typecheck = false;
    eval = false;
    concrete = false;
    dead = false;
    early_exit = false;
    simplify = false;
    include_all = false;
    use_thunks = false;
    use_post_var = true;
    assertion_on_var = false; 
    deend = true;
    deend_multi = false;
    verify_expected = false;
    conc_mem_idx = true;
    prop_consts = false;
    remove_unknowns = false;
    flatten = false;
  } in
  
  let arg_spec = 
    [
      (* XXX deprecated options *)
(*      ("-bins",  *)
(*       Arg.String (fun s -> cmdlineargs.bins_file <- s), *)
(*       "FILE\tread list of executables from FILE") ; *)

(*      ("-ir-in",  *)
(*       Arg.String (fun s -> cmdlineargs.irin_file <- s), *)
(*       "FILE\tread ir trace from FILE") ; *)

(*      ("-first", *)
(*       Arg.String (fun s -> cmdlineargs.first_eip <- (Int64.of_string s)), *)
(*       "EIP\tignore trace entries before EIP is encountered") ; *)
     
(*      ("-last", *)
(*       Arg.String (fun s -> cmdlineargs.last_eip <- (Int64.of_string s)), *)
(*       "EIP\tignore trace entries after EIP is encountered") ; *)

(*      ("-taintcheck", *)
(*       Arg.Unit (fun () -> cmdlineargs.taintcheck <- true), *)
(*       "\tApply dynamic taint analysis to the trace"); *)

(*      ("-skip-addr", *)
(*       Arg.String (fun s ->  *)
(*                     failwith "-skip-addr is deprecated" *)
(* (\*                     cmdlineargs.skip_addrs  *\) *)
(* (\*                     <- ((Int64.of_string s) :: cmdlineargs.skip_addrs) *\) *)
(*                  ), *)
(*       "EIP\tdon't disassemble this address (DEPRECATED)") ; *)

(*      ("-block-by-block", *)
(*       Arg.Bool (fun b -> cmdlineargs.block_by_block <- b), *)
(*       "\tmake straightline block-by-block. experimental."); *)



      (**** input sources ****)
     ("-trace", 
      Arg.String (fun s -> cmdlineargs.trace_file <- s),
      "FILE\tread trace from FILE") ;

     ("-state", 
      Arg.String (fun s -> cmdlineargs.state_file <- s),
      "FILE\tread process state from FILE") ;

     (**** options and transformations ****)
     ("-state-range",
      Arg.String 
        (fun s -> 
           assert (cmdlineargs.state_file <> "");
           Scanf.sscanf 
             s 
             "0x%Lx:0x%Lx" 
             (fun x y -> 
                cmdlineargs.state_ranges <- (x,y)::cmdlineargs.state_ranges)
        ),
      "0xDEAD:0xBEEF\tinitialize range 0xDEAD to 0xBEEF") ;

     ("-conc-mem-idx",
      Arg.Bool (fun b -> cmdlineargs.conc_mem_idx <- b),
      "\trewrite non-tainted mem indexes to literal values");

     ("-prop-consts",
      Arg.Bool (fun b -> cmdlineargs.prop_consts <- b),
      "\tUse evaluator to do constant propagation");

     ("-flatten",
      Arg.Bool (fun b -> cmdlineargs.flatten <- b),
      "\tflatten IR");

     ("-use-thunks",
      Arg.Bool (fun b -> cmdlineargs.use_thunks <- b),
      "\tuse eflag thunks (lazy eflag computation).");

     ("-use-post-var",
      Arg.Bool (fun b -> cmdlineargs.use_post_var <- b),
      "\tuse a post-condition variable instead of asserts.");

     ("-assertion-on-var",
      Arg.Bool (fun b -> cmdlineargs.assertion_on_var <- b),
      "\tcreate a unique boolean variable for each assertion.");

     ("-deend",
      Arg.Bool (fun b -> cmdlineargs.deend <- b),
      "\tDeendianize all memory accesses");

     ("-deend_multi",
      Arg.Bool (fun b -> cmdlineargs.deend_multi <- b),
      "\tWhen de-endianizing, use separate arrays by access size");

     ("-verify-expected",
      Arg.Bool (fun b -> cmdlineargs.verify_expected <- b),
      "\tAdd asserts to check whether propagated inputs have expected"
      ^ " values.\n\t\t(Only makes sense with -concrete)"
     );

     ("-include-all",
      Arg.Unit (fun () -> cmdlineargs.include_all <- true),
      "\tDisasm and include all instructions, not just tainted.");

     ("-remove-unknowns",
      Arg.Bool (fun b -> cmdlineargs.remove_unknowns <- b),
      "\tRemoves some unsupported instructions");

     ("-typecheck",
      Arg.Unit (fun () -> cmdlineargs.typecheck <- true),
      "\tType check the generated IR");

     ("-concrete",
      Arg.Unit (fun () -> cmdlineargs.concrete <- true),
      "\tAssign concrete values to input (when building from exec trace)");

     ("-dead",
      Arg.Unit (fun () -> cmdlineargs.dead <- true),
      "\tperform dead code elimination");

     ("-early-exit",
      Arg.Unit (fun () -> cmdlineargs.early_exit <- true),
      "\tadd early exits when post-condition cannot be satisfied");

     ("-simplify",
      Arg.Unit (fun () -> cmdlineargs.simplify <- true),
      "\tapply simplifications to the WP");

     (**** outputs ****)
     ("-ir-out", 
      Arg.String (fun s -> cmdlineargs.irout_file <- s),
      "FILE\toutput trace ir to FILE") ;
     
     ("-wp-out",
      Arg.String (fun s -> cmdlineargs.wpout_file <- s),
      "FILE\toutput WP to FILE in IR format");

     ("-stp-out",
      Arg.String (fun s -> cmdlineargs.stpout_file <- s),
      "FILE\toutput trace to FILE in stp format") ;

     ("-eval",
      Arg.Unit (fun () -> cmdlineargs.eval <- true),
      "\trun trace through the evaluator");

    ]
  in
  let () = 
    Arg.parse 
      arg_spec 
      (fun s -> ()) 
      "Usage: appreplay [options] <tracefile> " 
  in
  cmdlineargs
;;




if not !Sys.interactive then
  let args = parse_cmdline in

  deend_use_multi args.deend_multi;
  deend_use args.deend;

  (* get trace from one source *)
  print_string "Getting trace\n";
  flush stdout;
  let prog =
    Printf.printf "statefile %s\n" args.state_file;
    List.iter 
      (fun (x,y) -> Printf.printf "%Lx %Lx\n" x y)
      args.state_ranges;


    (* build ir trace from execution trace *)
    if (args.trace_file <> "")
    then (
      let tracker = new track_opval_filter in
      let filters = 
        [if args.include_all then new disasm_all else new disasm_tainted;
         new print_filter;
         new handle_outsw;
         new constrain_mem_accesses args.state_ranges;
         new make_straightline_filter;
         (tracker :> eh_filter);
         new initialize_operands_small tracker args.verify_expected;
         new uniqify_labels;
        ]
      in
      let filters = 
	if args.remove_unknowns then 
	  filters @ [new unknowns_filter]
	else 
	  filters
      in

(*       let filters = *)
(*         if args.conc_mem_idx then *)
(*           filters @ [new conc_idx_filter] *)
(*         else *)
(*           filters *)
(*       in *)
      let filters =
        if args.concrete then
          filters @ [new initialize_input_symbols tracker]
        else
          filters
      in
      let filters =
        if true then (* XXX make optional *)
          filters @ [new break_dep_chains_filter tracker]
        else
          filters
      in
      let trace_prog, trace_insns = 
        disasm_trace ~use_thunks:args.use_thunks args.trace_file filters in
      
      (* Initialize memory regions from state file *)
      let trace_prog = 
        if (args.state_file <> "") && (List.length args.state_ranges > 1) then
          let memvar = Asmir.gamma_lookup trace_prog.gamma "$mem" in
          let mem_inits = 
            Temu_state.generate_range_inits args.state_file args.state_ranges
              memvar
          in
          {trace_prog with prog_setup_ir =
              Block([],mem_inits)::trace_prog.prog_setup_ir}
        else trace_prog
      in

      let prog = 
        trace_to_prog ~deend:args.deend trace_prog trace_insns in

      (* XXX should make this a transformation step.
         may need to make asserts_to_post more generic, though *)
      let prog =
        if args.assertion_on_var then
          let prog = create_assert_variable prog in
            prog
        else
          prog
      in

      let prog =
        if args.use_post_var then
          let prog, post_var = asserts_to_post prog in
          prog
        else
          prog
      in


      prog
    )
    (* read ir trace from file *)
(*     else if (args.irin_file <> "")  *)
(*     then ( *)
(* (\*       Vine_absyn.strip_nums := true;  *\) *)
(*       Vine_parser.parse_file args.irin_file  *)
(*     ) *)
    else
      raise (Arg.Bad "No input specified")
  in

(********** tranformations ***************)
  (* propagate all constants forward. *)
  let prog = 
    if (args.prop_consts || args.conc_mem_idx ) then
      prop_constants prog (not args.prop_consts) 
    else
      prog
  in

  (* flatten ir *)
  let prog =
    if (args.flatten) then
      flatten_blocks prog
    else
      prog
  in

  if (args.typecheck) then (
    print_string "Typechecking\n";
    flush stdout;
    (* type check *)
    Vine_typecheck.typecheck prog
  );

  print_string "Performing transformations\n";
  let prog = 
    if (args.dead) then 
      deadcode prog
    else 
      prog
  in

  let prog =
    if (args.early_exit) then
      let (dl,sl) = prog in
	match Exectrace.post_vars_to_cjmps (findvar dl "post") (Block(dl,sl))
	with
	    Block(dl,sl) -> (dl,sl)
	  | _ -> raise (Invalid_argument "appreplay")
    else
      prog
  in
  (* type check *)
  if (args.typecheck) then (
        print_string "Typechecking again\n";
        flush stdout;
        Vine_typecheck.typecheck prog
  );

(*************** outputs **********************)
  print_string "Outputting\n";
  flush stdout;

  (* evaluate *)
  if (args.eval) then (
    Printf.printf "Evaluating\n%!";
    let evaluator = new Vine_ceval.concrete_evaluator prog in
    let _ = evaluator#run in
    ()
  );

  (* output ir file *)
  if (args.irout_file <> "") then (
    Printf.printf "Writing ir to %s\n%!" args.irout_file;
    let ir_channel = open_out args.irout_file in
      pp_program (output_string ir_channel) prog;
      close_out ir_channel
   ) ;
      
  (* output stp file *)
  if (args.stpout_file <> "" || args.wpout_file <> "")
  then (
    let t1 = Unix.gettimeofday () in
    appreplay args prog;
    let t2 = Unix.gettimeofday () in
    Printf.printf "Time to create sym constraint from TM: %f\n%!"
	(t2 -. t1)
  )

;;


(*
   if Array.length Sys.argv < 3
   then  
   print_string "Usage: appreplay <tracefile> <model_output>\n"; exit 0;;
   
   let (trace, fout) = 
   if Sys.argv.(1) = "-m"
   then (Marshal.from_channel(open_in Sys.argv.(2)), Sys.argv.(3))
   else 
   (Vine_parser.parse_channel (open_in Sys.argv.(1)), Sys.argv.(2))
   ;;
   debug(Printf.sprintf "Read %d statements.\n" (List.length trace));;
   appreplay trace fout
*)
