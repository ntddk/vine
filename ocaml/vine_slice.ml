(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Program Slicing.

    TODO:
    * Make sure not to remove jump targets
    * How to deal with edges in complicated cases
*)

open Ssa

module H = Hashtbl.Make(Ssa.G.V)
module VH = Vine.VarHash
module PDG = Vine_pdg.SSA_PDG


let in_slice ?(keep_asserts=true) cfg variables =
  let pdg = PDG.compute_pdg cfg in
  let live = H.create cfg#length in
  let is_live = H.mem live in
  let rec add_to_slice id =
    if not(is_live id) then (
      H.add live id ();
      PDG.iter_pred add_to_slice pdg id
    )
  in
  let vh = VH.create (List.length variables) in
  let () = List.iter (fun x -> VH.add vh x ()) variables in
  let rec live_stmt = function
    | Attr(s,_) -> live_stmt s
    | Move(v,_) -> VH.mem vh v
    | Assert _ -> keep_asserts
    | Label _
    | Comment _ -> true
    | _ -> false
  in
  let live_bb bb =
    List.exists live_stmt (cfg#get_info bb)
  in
    cfg#iter_bb (fun bb -> if live_bb bb then add_to_slice (cfg#get_id bb));
    add_to_slice Vine_cfg.BB_Exit;
    is_live


let slice ?(keep_asserts=false) cfg variables =
  let id_live = in_slice ~keep_asserts:keep_asserts cfg variables in
  let bb_dead bb = not(id_live (cfg#get_id bb)) in
  let for_later = ref [] in
  let remove_simple bb =
    let action = match cfg#succ bb with
      | [s] -> `Remove s
      | [s;s'] when s = s' -> `Remove s
      | [s;s'] when s' = bb -> `Remove s
      | [s';s] when s' = bb -> `Remove s
      | [] -> `Drop
      | _ -> `Postpone
    in
      match action with
	| `Remove s ->
	    cfg#iter_pred (fun p -> cfg#add_edge p s) bb;
	    cfg#remove_bb bb
	| `Drop ->
	    cfg#remove_bb bb
	| `Postpone ->
	    for_later :=  bb :: !for_later
  in
    cfg#iter_bb (fun bb -> if bb_dead bb then remove_simple bb);
    while !for_later <> [] do
      let toremove = !for_later in
      let size = cfg#length in
	for_later := [];
	List.iter remove_simple toremove;
	(* Hmm, there exist cases this won't be able to deal with... 
	   Would requiring reducible loops be enough? *)
	assert(size <> cfg#length)
    done
