(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

open Vine

module D = Debug.Make(struct let name=Sys.argv.(0) and default=`Debug end)
open D

let to_cfile debug filename prog = 
  let oc = open_out filename in
  let ft = Format.formatter_of_out_channel oc in 
  let pp = new To_c.pp ~debug_labels:debug ft in
    pp#format_program prog;
    Format.pp_print_flush ft ();
    close_out oc


let format_to filename prog =
  let oc = open_out filename in
  let () = Vine.format_program (Format.formatter_of_out_channel oc) prog in
    close_out oc

let to_theorem_prover to_file file prog =
  (* The translation only does assertions, so the input is assumed to be a 
   * single ExpStmt of type reg1_t, to be asserted
   * (If there are more than one ExpStmt, with the same free variables, they
   * will be declared multiple times in the STP file. *)
  let ch = open_out file in
  let printer = to_file ch in
  let visitor =
object
  inherit nop_vine_visitor
  method visit_stmt s = 
    match s with
	ExpStmt e -> printer e; SkipChildren
      | _ -> DoChildren
end in
    List.iter (fun s -> ignore(stmt_accept (visitor :> vine_visitor) s)) (snd prog);
    close_out ch
    

let to_indep prefix ((dl,sl) as prog) =
  let visitor =
object(self)
  inherit nop_vine_visitor
  val mutable count = 0
  method next = count <- count+1; prefix^"."^string_of_int count
  method visit_stmt s = 
    match s with
	ExpStmt e ->
	  let indeps = Vine_indepclauses.split_indep e in
	    List.iter (fun e -> format_to self#next (dl,[ExpStmt e])) indeps;
	    SkipChildren
      | _ -> DoChildren
end in
    List.iter (fun s -> ignore(stmt_accept (visitor :> vine_visitor) s)) (snd prog)


let write_cfg cfg os =
  Vine_graphviz.VineStmtsDot.output_graph os cfg

let write_ssacfg cfg os =
  Vine_graphviz.SsaStmtsDot.output_graph os cfg

    


module CDG = Vine_pdg.CDG(struct type t = Vine.stmt list end)
module CdgVineStmtsPrinter =
  Vine_graphviz.MakeOtherCfgPrinter(CDG)(Vine_cfg.G)(Vine_graphviz.PrintVineStmts)
module CdgVineStmtsDot = Graph.Graphviz.Dot(CdgVineStmtsPrinter)

module DDG = Vine_pdg.SSA_DDG
module DdgSsaStmtPrinter =
  Vine_graphviz.MakeOtherCfgPrinter(DDG)(Ssa.G)(Vine_graphviz.PrintSsaStmts)
module DdgSsaStmtDot = Graph.Graphviz.Dot(DdgSsaStmtPrinter)


let prog2ssa prog =
  let cfg = Vine_cfg.prog_to_cfg prog in
  let () = Vine_cfg.remove_unreachable cfg in
  let () = if not(Vine_cfg.well_defined cfg) then
    failwith "CFG not well defined"
  in
  let () = Vine_cfg.coalesce_bb cfg in
  Ssa.cfg2ssa cfg


(* get the cfg of a function body *)
let get_func_cfg prog = 
  let (dl, sl) = prog in 
  let firstfunstmt sl = 
    let is_fun stmt =
      match stmt with 
	  Vine.Function(_) -> true
	| _ -> false 
    in
      List.find is_fun sl
  in
    Vine_cfg.func_to_cfg (firstfunstmt sl) 

(* print loop information *)
let printloopinfo loops rcfg =
  let printer = Printf.printf "%s" in
  let get_label lset bb = 
    List.fold_left ( fun lset stmt -> 
      match stmt with  
	  Vine.Label(s)-> s::lset 
	| _ -> lset
    )
      lset (rcfg#get_info bb)
  in
  let printexitedge loop =
    let exitedges = Vine_loop.get_loop_exit_edges loop rcfg in
    let showedge exitedge =
      let srclabel = get_label [] (fst exitedge) in
      let dstlabel = get_label [] (snd exitedge) in 
	Printf.printf "%s -> %s\n" 
	  (List.hd srclabel) (List.hd dstlabel)
    in
      Printf.printf "\n** Exit edges:\n";
      List.iter showedge exitedges 
  in
  let printlabels loop =
    let labellist = 
      List.fold_left get_label [] (Vine_loop.get_loop_body loop) 
    in
      Printf.printf "\n** Loop body lables:\n";
      List.iter (fun l -> Printf.printf "%s\n" l) (List.rev labellist)
  in
  let printloopbody loop = 
    Printf.printf "**Loop body:\n";
    Vine_loop.pp_loop printer loop 
  in 
  let counter = ref 0 in
  let separater () = 
    counter := !counter+1; 
    Printf.printf "\n========== Loop %d =========\n" !counter
  in
    Printf.printf "Number of loops: %d\n" (List.length loops);
    List.iter 
      (fun l -> separater (); printloopbody l; printlabels l; printexitedge l) 
      loops


    
(* parse options *)
let parse_args () =
  let input = ref None in
  let output = ref [] in
  let usage = "Reads IR from any source and outputs it in one or more formats.\n"
    ^"Optionally, can perform transformations along the way. Transformation\n"
    ^"order now resembles a shell pipeline: Transformations/printing on the left\n"
    ^"is performed first and passed on to later ones."
  in
  let add_trans f =
    output := f:: !output
  in
  let add_output f =
    output := (fun x -> f x; x):: !output
  in
  let output_c file = 
    add_output (to_cfile false file)
  in
  let output_debug_c file = 
    add_output (to_cfile true file)
  in
  let output_human file =
    add_output (Vine.pp_program (output_string (open_out file)))
  in
  let output_format file =
    add_output (format_to file)
  in
  let output_marshal file =
    add_output (fun ir -> Marshal.to_channel (open_out file) ir [])
  in
  let output_stp file =
    add_output (to_theorem_prover Stp.to_file file)
  in
  let output_smt file =
    add_output (to_theorem_prover Smt_lib.to_file file)
  in
  let output_indep prefix =
    add_output (to_indep prefix)
  in
  let output_cfg file =
    let fd = open_out file in
      add_output (fun ir-> write_cfg (Vine_cfg.prog_to_cfg ir) fd)
  in
  let output_tracecfg file =
    let fd = open_out file in
      add_output (fun ir-> write_cfg (Vine_cfg.prog_to_cfg ir) fd)
  in
  let output_cfgsimp file =
    add_output (fun ir->
      let fd = open_out file in
      let cfg = Vine_cfg.prog_to_cfg ir in
      let (cfg,_) = Vine_dataflow.simplify_graph cfg 100 in
	write_cfg cfg fd)
  in
  let output_tracecfgsimp file =
    add_output (fun ir->
      let fd = open_out file in
      let cfg = Vine_cfg.trace_to_cfg ir in
      let (cfg,_) = Vine_dataflow.simplify_graph cfg 100 in
	write_cfg cfg fd)
  in
  let output_cdg file =
    add_output (fun ir->
      let fd = open_out file in
      let cfg = Vine_cfg.prog_to_cfg ir in
      let () = Vine_cfg.coalesce_bb cfg in
      let cdg = CDG.compute_cdg cfg in
	CdgVineStmtsDot.output_graph fd (cdg,cfg)
    )
  in
  let output_ddg file =
    add_output (fun ir->
      let fd = open_out file in
      let ssa = prog2ssa ir in
      let ssa' = Ssa.ssalist_to_ssa ssa in
      let ddg = DDG.compute_true_dependence ssa' in
	DdgSsaStmtDot.output_graph fd (ddg,ssa')
    )
  in
  let output_pdg file =
    add_output (fun ir->
      let fd = open_out file in
      let ssa = prog2ssa ir in
      let ssa' = Ssa.ssalist_to_ssa ssa in
      let pdg = Vine_pdg.SSA_PDG.compute_pdg ssa' in
	Vine_pdg.PdgSsaStmtDot.output_graph fd (pdg,ssa')
    )
  in
  let output_ssa file =
    add_output (fun ir->
      let ssa = prog2ssa ir in
      let () = pdebug "Writting output" in
      let fd = open_out file in
	write_ssacfg ssa fd)
  in
  let output_ssaback file =
    (* this actually converts to SSA and back before outputing*)
    (* deprecated *)
    add_output (fun ir->
      let ssa = Ssa.cfg2ssa(Vine_cfg.prog_to_cfg ir) in
      let () = Ssa.rm_phis ssa in
      let cfg = Ssa.cfg2vine ssa in
      let p = Vine_cfg.normal_cfg_to_prog cfg in
	format_to file p)
  in
  let output_dce file =
    let fd = open_out file in
      add_output (fun ir-> 
	let cfg = Vine_cfg.prog_to_cfg ir in
	let () = ignore(Vine_dataflow.do_dce cfg) in
	  write_cfg cfg fd )
  in
  let do_typecheck () =
    let typecheck_prog p =
      Vine_typecheck.typecheck p
    in
      add_output typecheck_prog
  in
  let trans_unlet  () = 
    add_trans (To_c.unlet)
  in
  let trans_simplify () =
    add_trans (fun (dec,stmts) -> (dec, List.map Vine_opt.simplify_stmt stmts))
  in
  let trans_simplify_faster () =
    let vis = object 
      inherit nop_vine_visitor
      method visit_exp e = ChangeTo(Vine_opt.simplify_faster e)
    end
    in
      add_trans(prog_accept vis)
  in
  let trans_mems () =
    add_trans Vine_memory2array.coerce_prog
  in
  let trans_mems_multi () =
    add_trans Vine_memory2array.coerce_prog_multi
  in
  let trans_ofcfg () =
    add_trans (fun prog -> Vine_cfg.normal_cfg_to_prog(Vine_cfg.prog_to_cfg prog))
  in
  let trans_ofssa () =
    add_trans
      (fun prog ->
	let ssa = prog2ssa prog in
	let () = Ssa.rm_phis ssa in
	let cfg' = Ssa.to_vine ssa in
	  Vine_cfg.normal_cfg_to_prog cfg'
      )
  in
  let trans_slice () =
    add_trans
      (fun prog ->
	let ssa = prog2ssa prog in
	let ssa = Ssa.ssalist_to_ssa ssa in
	let () = Vine_slice.slice ~keep_asserts:true ssa [] in
	let () = Ssa.rm_phis ssa in
	let cfg' = Ssa.to_vine ssa in
	  Vine_cfg.normal_cfg_to_prog cfg'
      )
  in
  let trans_descope () =
    add_trans Vine_alphavary.descope_program
  in
  let trans_alphavary () =
    add_trans Vine_alphavary.alpha_vary_program
  in
  let trans_globalizewp () =
    add_trans
      (fun ((dl,sl) as prog) ->
	let globals = ref [dl] in
	let vis = object
	  inherit nop_vine_visitor
	  method visit_stmt = function
	    | ExpStmt e -> 
		let (vars,e') = Wp.globalize_wp e in
		  globals := vars :: !globals;
		  ChangeTo(ExpStmt e')
	    | _ ->
		pwarn "-globalize-wp: Ignoring unexpected statement type";
		SkipChildren
	end in
	let (_,sl) = prog_accept vis prog in
	  (List.concat !globals, sl)
      )
  in
  let trans_inline1 () =
    add_trans
      (fun prog ->
	 let defs = Vine_inline.find_function_definitions prog in
	   prog_accept (new Vine_inline.inlining_visitor defs) prog
      )
  in
  let trans_inline () =
    add_trans
      (fun prog ->
	 let defs = Vine_inline.find_function_definitions prog in
	 let is_rec = Vine_callgraph.is_recursive prog in
	   prog_accept (new Vine_inline.recursive_inlining_visitor defs is_rec) prog
      )
  in
  let trans_postvar () =
    add_trans 
      (fun prog -> 
	let prog, _ = Exectrace.asserts_to_post prog in
	  prog
      )
  in
  let trans_opt () =
    add_trans
      (fun prog ->
	 let ssaref =
	   let cfg = Vine_cfg.prog_to_cfg prog in
	     Vine_cfg.coalesce_bb cfg;
	     (* we may want to do this more than just once at the beginning... *)
	     ignore(Vine_dataflow.constant_propagation_and_folding cfg Vine_opt.simplify_stmt);
	     Vine_cfg.remove_unreachable cfg;
	     ref (Ssa.cfg2ssa cfg)
	 in
	 let changed = ref true in
	   while !changed; do
	     let ssacfg = !ssaref in
	       (* alias analysis *)
	       D.dprintf "Performing VSA alias analysis";
	       let alias = Vine_alias.vsa_alias ssacfg in
	       let x = Vine_alias.alias_replace alias ssacfg in
	       let y = Vine_alias.remove_dead_stores alias ssacfg in
	       D.dprintf "Done performing VSA alias analysis";
		 
	       D.dprintf "Simplifying graph";
	       (*   let globals = V.freerefs_exp post in *)
	       let (ssacfg,z) = Vine_dataflow.SsaDataflow.simplify_graph ssacfg 1 in
		 D.dprintf "Done simplifying graph";
		 ssaref := ssacfg;
		 changed := x || y || z
	   done;

	   let cfg = Ssa.to_vine !ssaref in
	     Vine_cfg.normal_cfg_to_prog cfg
      )
  in
  let trans_tracessa () =
    add_trans Ssa.trace_ssa
  in
  let get_cfg = ref get_func_cfg in 
  let show_all_loops () = 
    add_output (fun prog -> 
      let cfg = !get_cfg prog in
      let rcfg = Vine_loop.get_reducible cfg in 
      let loops = Vine_loop.get_loops rcfg in
	printloopinfo loops rcfg
    )
  in
  let show_addr_loops addr = 
    add_output (fun prog -> 
      let label = "pc_" ^ addr in
      let cfg = !get_cfg prog in
      let rcfg = Vine_loop.get_reducible cfg in 
      let bb = cfg#find_label label in
      let loops = Vine_loop.get_loops_from_bb rcfg bb in
	printloopinfo loops rcfg
    ) 
  in
  let unroll_addr = ref "" in
  let unroll_loop times =
    add_trans (fun prog ->
      let label = "pc_" ^ !unroll_addr in
      let cfg = !get_cfg prog in
        (* let simply assume the input cfg is already reducible *)
      let rcfg = Vine_loop.get_reducible cfg in
	(*let rcfg = cfg in*)
      let bb = cfg#find_label label in
      let loops = Vine_loop.get_loops_from_bb rcfg bb in
        (* assume that there is exactly one loop *)
	match loops with
	    [] -> 
	      failwith "There is no loop that contains the specified address"
	  | loop :: _ ->
	      let () = Vine_loop.unroll_loop rcfg loop times in
		(* don't put CFG_ERROR in prog *)
		rcfg#remove_bb (cfg#find Vine_cfg.BB_Error);
		Vine_cfg.normal_cfg_to_prog rcfg
    )
  in
  let rec arg_spec =
    (* outputs *)
    ("-marshal", Arg.String output_marshal, "Marshal the parsed XML to a file.")
    ::("-print", Arg.Unit (fun()->add_output (Vine.pp_program print_string)), "Print on stdout.")
    ::("-human", Arg.String output_human, "Output human readable text to a file.")
    ::("-format", Arg.String output_format, "Output nicely formatted text to a file.")
    ::("-stp", Arg.String output_stp, "Output an STP assertion for the ExpStmt. For this to be handled by STP, only one ExpStmt should exist in the program.")
    ::("-smtlib", Arg.String output_smt, "Output an SMT-Lib benchmark for the ExpStmt. For this to be handled by STP, only one ExpStmt should exist in the program.")
    ::("-indep", Arg.String output_indep, "Output independent clauses of the ExpStmt to <file>.num.")
    ::("-cfg", Arg.String output_cfg, "Compute the Control Flow Graph and output a .dot file.")
    ::("-tracecfg", Arg.String output_tracecfg, "Compute the Control Flow Graph for a trace and output a .dot file.")
    ::("-cfgsimp", Arg.String output_cfgsimp, "Simplify the Control Flow Graph and output a .dot file.")
    ::("-tracecfgsimp", Arg.String output_tracecfgsimp, "Simplify the Control Flow Graph and output a .dot file. Input must be a trace.")
    ::("-cdg", Arg.String output_cdg, "Compute the Control Dependence Graph and output a .dot file.")
    ::("-ddg", Arg.String output_ddg, "Compute the Data Dependence Graph and output a .dot file.")
    ::("-pdg", Arg.String output_pdg, "Compute the Program Dependence Graph and output a .dot file.")
    ::("-dce", Arg.String output_dce, "Compute the CFG, perform DCE, and output a .dot file.")
    ::("-ssa", Arg.String output_ssa, "Compute the CFG, translate to SSA, and output a .dot file.")
    ::("-ssaback", Arg.String output_ssaback, "Deprecated. Use -ofssa and -format.")
    ::("-to-c", Arg.String output_c, "Output program in C syntax")
    ::("-to-debug-c", Arg.String output_debug_c, "Output program in C syntax with extra debugging statements")
    ::("-typecheck", Arg.Unit do_typecheck, "Typecheck program")
    ::("-all-loops", Arg.Unit show_all_loops, "Find all loops in the first function")
    ::("-addr-loops", Arg.String show_addr_loops, "Find loops containing the specifies address in the first function")
    ::("-loop-no-func", Arg.Unit (fun()-> get_cfg := Vine_cfg.prog_to_cfg),
      "Let 'all-loops' and 'addr-loops' search loops in whole IR")
      (* transformations *)
    ::("-simplify", Arg.Unit trans_simplify, "Simplify expressions")
    ::("-simplify-faster", Arg.Unit trans_simplify_faster, "Simplify expressions. (only fast simplifications)")
    ::("-opt", Arg.Unit trans_opt, "Optimize the program. (Uses simplify_graph and VSA alias analysis)")
    ::("-fixmems", Arg.Unit trans_mems, "Attempt to fix broken mem expressions")
    ::("-deend-multi", Arg.Unit trans_mems_multi, "De-endianize using multiple arrays")
    ::("-ofcfg", Arg.Unit trans_ofcfg, "Convert to CFG and back.")
    ::("-ofssa", Arg.Unit trans_ofssa, "Convert to SSA and back.")
    ::("-descope", Arg.Unit trans_descope, "Descope the program.")
    ::("-alphavary", Arg.Unit trans_alphavary, "Alpha-vary the program.")
    ::("-globalize-wp", Arg.Unit trans_globalizewp, "Globalize a weakest precondition from SSA form.")
    ::("-inline1", Arg.Unit trans_inline1, "Inline all function calls once.")
    ::("-inline", Arg.Unit trans_inline, "Inline function calls as much as necessary.")
    ::("-tracessa", Arg.Unit trans_tracessa, "Convert a trace to SSAish form.")
    ::("-slice", Arg.Unit trans_slice, "Slice a program WRT given variables. (For now, put the variables in an assert.)")
    ::("-unlet", Arg.Unit trans_unlet, "Rewrite lets into assignments")
    ::("-use-post-var", Arg.Unit trans_postvar, "Convert assertions to post variables")
    ::("-unroll-loop", Arg.Tuple [Arg.Set_string unroll_addr; Arg.Int unroll_loop], "<haddr> <n>: unroll the first loop found containing the address haddr for n times")
    ::("-fromhuman", Arg.String input_human, "Read from a human readable syntax file.")
    ::("-frombin", Arg.String input_bin, "Dissasemble from a binary")
    ::("-frombininit", Arg.String input_bin_with_init, "Dissasemble from a binary, initializing memory locations.")
    ::("-nothunks", Arg.Unit (fun()->Libasmir.set_use_eflags_thunks false), "Don't use EFLAGS thunks when dissasembling a binary.")
    :: Vine_parser.defspecs
  and exit_usage s = raise(Arg.usage arg_spec usage; Arg.Bad s)
  and set_input f = match !input with
      None -> input := Some f
    | _ -> exit_usage "too many inputs"
  and input_human file =
    set_input (fun () -> Vine_parser.parse_file file)
  and input_bin file =
    set_input
      (fun () -> Asmir.asmprogram_to_vine(Asmir.disassemble_program file))
  and input_bin_with_init file =
    set_input
      (fun () ->
	 let asmp = Asmir.disassemble_program file in
	   Asmir.asmprogram_to_vine ~init_mem:true asmp )
  and input_marshaled file =
    set_input (fun () -> Marshal.from_channel (open_in file) )
  in
    (Arg.parse arg_spec input_human usage;
     match (!input, !output) with
	 (Some i, []) -> (i, fun x -> Vine.pp_program print_string x; x)
       | (Some i, o::os) -> (i, List.fold_left (fun f n x-> f(n x)) o os)
       | (None, _) ->
	   (exit_usage "No input specified";
	    failwith "FIXME: why does exit_usage not have type string -> 'a ?") )

let (input, output) = parse_args()
;;

output (input())


