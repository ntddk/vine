(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

let usage = "evaluator [options]* file\n" in 
let infile = ref "" in 
let infile_set = ref false in
let arg_name s = infile := s; infile_set := true in
let main argc argv = 
  (
      let speclist = Vine_parser.defspecs in 
      let speclist = [] @ speclist in 
	Arg.parse speclist arg_name usage;
	if(!infile_set = false) then  (
	  Arg.usage speclist usage; exit(-1)
	);
	let prog = (
	  let p = Vine_parser.parse_file !infile in 
	  let () = if !Vine_parser.flag_typecheck then
	    Vine_typecheck.typecheck p else () in 
	    p
	) in 
	let () = if !Vine_parser.flag_pp then 
	  Vine.pp_program (print_string) prog in
	let r =  
	  (let ce = new Vine_ceval.concrete_evaluator prog in
	   let r = ce#run () in 
	     Printf.printf "Halt value is %s\n"
	       (Vine_ceval.value_to_string r);
	     match r with
	      Vine_ceval.Int(_,x) -> Int64.to_int x
	       | _ -> -1
	  ) in
	  exit(r)
	  
  )
in
main (Array.length Sys.argv) Sys.argv;;
