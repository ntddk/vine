(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(**
*  Compute the weakest precondition to get to some line of code.
*)

module D = Debug.Make(struct let name=Sys.argv.(0) and default=`Debug end)
open D;;

open Vine;;
open Vine_util;;
open Vine_cfg;;
open Gcl;;

module List = ExtList.List;;

let version = "$Id$";;


type args = {
  mutable solve : bool;
  mutable postcond : string; 
  mutable outfile : string;
  mutable stpfile : string;
};;

type wptype = Leino | Dijkstra | Default;;

(* statistic-related variables *)
let flag_dostats = ref false;;
let starttime = ref 0.0;;
let endtime = ref 0.0;;
let stpstart = ref 0.0;;
let stpend = ref 0.0;;
let num_wp_exp = ref 0;;
let flag_wp = ref Default;;




let count_exp  wp = 
  let vis = object(self)
    inherit nop_vine_visitor

    method visit_exp e = 
      num_wp_exp := !num_wp_exp + 1;
      DoChildren

  end
  in
    if !flag_dostats then ignore(exp_accept vis wp)


let query_stp oc  wp = 
  let vc = Stpvc.create_validity_checker () in 
  let ctx = Vine_stpvc.new_ctx vc (get_req_ctx wp) in 
  let stp_wp = Vine_stpvc.vine_to_stp vc ctx wp in 
  let query = Stpvc.e_not vc (Stpvc.e_bvbitextract vc stp_wp 0) in  
  let () = Printf.fprintf oc "Querying STP...\n%!" in 
  let () = stpstart := Unix.gettimeofday () in 
  let b = Stpvc.query vc query in
  let () = stpend := Unix.gettimeofday () in 
    if b then
      Printf.fprintf oc "Valid.\n%!" 
    else (
      Printf.fprintf oc "Invalid.\n%!";
      flush_all ();
      Libstp.vc_printCounterExample vc
    )

let print_stp filename wp = 
  let oc = open_out filename in 
    Stp.to_file oc wp;
    output_string oc "\n";
    output_string oc "QUERY(FALSE);\n";
    output_string oc "COUNTEREXAMPLE;\n";
    close_out oc

let print_ir filename wp =
  let oc = open_out filename in
  let dl = Vine.get_req_ctx wp in
    pp_program (output_string oc) (dl, [ExpStmt wp]);
    close_out oc

let coerce_exp mem arr s = 
  let vis = new Vine_memory2array.memory2array_visitor mem arr in 
    exp_accept vis s 


let deend_exp e dl = 
  let mems,nonmems = List.partition
    (function (_,_,Vine.TMem _) -> true | _ -> false) dl
  in 
  let arrs = List.map 
    (function (_,nm,TMem(typ,_)) -> 
       newvar nm
       (Vine.Array(REG_8,Vine.array_idx_type_to_size typ))) mems in 
    List.fold_left2 
      (fun exp oldmem newarr -> coerce_exp  oldmem newarr exp ) e mems arrs

let leino_wp cfg post = 
  let ssa = Ssa.cfg2ssa cfg in 
  let () = Ssa.rm_phis ssa in 
  let cfg = Ssa.cfg2vine ssa in 
  let gcl = Gcl.remove_skips (Gcl.of_cfg cfg BB_Exit BB_Entry) in  
    Wp.leino_wp (fun x -> x) post  gcl

let default_wp cfg post = 
  let gcl = Gcl.remove_skips (Gcl.of_cfg cfg BB_Exit BB_Entry) in  
    Wp.calculate_wp (fun x -> x)  post gcl

let dijkstra_wp cfg post = 
  let gcl = Gcl.remove_skips (Gcl.of_cfg cfg BB_Exit BB_Entry) in  
    Wp.dijkstra_wp (fun x -> x) post gcl



let doit args (dl,sl) post = 
  let (dl,sl) = Vine_memory2array.coerce_prog (dl,sl) in  
  let cfg = prog_to_cfg (dl,sl) in 
  let () = Vine_cfg.remove_unreachable cfg in 
  let () = if well_defined cfg then () else 
    failwith "CFG for program is not well defined. Aborting." in
    (* XXX: need to add check for CFG being acyclic *)
  let wp = 
    match !flag_wp with
	Dijkstra -> dijkstra_wp cfg post
      | Leino -> leino_wp cfg post
      | _ -> default_wp cfg post
  in
  let wp = Vine_alphavary.alpha_vary_exp wp in  
  let () = count_exp wp in
    if args.outfile = "" then (
      print_endline "WP:";
      pp_exp print_string wp;
      print_endline "";
    ) else
      print_ir args.outfile wp;

    if args.stpfile <> "" then 
      print_stp args.stpfile  wp;

    if args.solve then
      query_stp stdout wp 
    else
      ()
;;



let printstats () = 
  if !flag_dostats then (
    Printf.printf "Total elapsed time (not counting parsing): %f sec\n%!" 
      (!endtime -. !starttime);
    Printf.printf "Number of expressions: %u\n%!" (!num_wp_exp);
    Printf.printf "STP Query time: %f sec\n%!" (!stpend -. !stpstart)
  ) else ()
;;

let usage = "wp [options]* file" in
let args = {
  outfile = "";
  solve = false;
  postcond = "true";
  stpfile = "";
} in
let infile = ref "" in 
let infile_set = ref false in 
let set_irfile f = 
  if !infile_set then (
    raise(Arg.Bad "wp only accepts one input file")
  ) else (
    infile_set := true;
    infile := f
  )
in 
let checkargs () =
  !infile_set || (prerr_endline "Must supply input file"; false)
in 
let print_version () = 
  Printf.printf "Version: %s\n%!" version
in
let main argc argv =
  let speclist = Vine_parser.defspecs @ [
    ("-o",
     Arg.String (fun s -> args.outfile <- s),
     "<file> output to file <file>"
    );
    ("-solve",
     Arg.Unit (fun () -> args.solve <- true),
     " Ask the theorem prover to solve"
    );
    ("-post",
     Arg.String (fun s -> args.postcond <- s),
     "<exp> postcondition. Interepreted wrt end label typing context"
    );
    ("-stpout",
     Arg.String (fun s -> args.stpfile <- s),
     "<string> output STP formula to file"
    );
    ("-stats",
     Arg.Unit (fun () -> flag_dostats := true),
     "Print out statistics"
    );
    ("-version",
     Arg.Unit (fun () -> print_version (); exit 0),
     " print version and exit"
    );
    ("-dijkstra",
     Arg.Unit (fun () -> flag_wp := Dijkstra),
     "Use dijkstras WP algorithm (very inefficient!)"
    );
    ("-leino",
     Arg.Unit (fun () -> flag_wp := Leino),
     "Use leino's WP algorithm (efficienty, but doesn't work with STP)"
    );
  ] in
  let () = Arg.parse speclist set_irfile usage in
  let _ = checkargs () || (Arg.usage speclist usage; exit(1)) in 
  let (dl,sl) = Vine_parser.parse_file !infile in 
  let post = Vine_parser.parse_exp_from_string dl args.postcond in
      starttime := Unix.gettimeofday (); 
      doit args (dl,sl) post;
      endtime := Unix.gettimeofday (); 
      printstats ()
in
  main (Array.length Sys.argv) Sys.argv;;
