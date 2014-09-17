(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

open Vine

(* Extlib ExtList has more efficient tail-recursive list functions *)
module List = ExtList.List 

(**
  Rename variables. 
*)
class var_renamer = 
object (self)

  inherit nop_vine_visitor 

  (** maps variables in scope to their new names *)
  val ctx  = Hashtbl.create 113

  method init_ctx dl = 
    let tbl = Hashtbl.create 113 in 
      List.iter (fun ((num,name,_) as v) -> 
		   let v = 
		     if Hashtbl.mem tbl name then renewvar v else v 
		   in
		     Hashtbl.add tbl name ();
		     Hashtbl.add ctx num v) dl


  method ctx_to_lst () = Hashtbl.fold (fun _ v a -> v::a) ctx []

  method alpha_vary_decl ((num,_,_) as v) =
    let newname = renewvar v in
      Hashtbl.add ctx num newname;
      newname

  method alpha_vary_decls = 
    List.map self#alpha_vary_decl

  method exit_decl (num,_,_)=
    Hashtbl.remove ctx num

  method exit_decls =
    List.iter self#exit_decl

  method visit_stmt s =
    begin
    match s with
	Block(dl,sl) ->
	  let dl' = self#alpha_vary_decls dl in 
	  let f x = self#exit_decls dl; x
	  in
	    ChangeDoChildrenPost(Block(dl', sl), f)
      | Function(v,topt, dl, b, Some(blk)) ->
	  let dl' = self#alpha_vary_decls dl in 
	  let f x = self#exit_decls dl; x 
	  in
	    ChangeDoChildrenPost(Function(v,topt,dl',b,
					  Some(blk)),f)
      | _ -> DoChildren
    end

  method visit_rlvalue lv = 
    match lv with
      | Temp(num,_,_) ->
	    (try 
	      let newname = Hashtbl.find ctx num in
		ChangeDoChildrenPost(Temp newname, (fun x -> x))
	      with
		  Not_found ->
		    DoChildren
	    )
      | Mem((num,_,_), idx,t) ->
	  try 
	     let newname = Hashtbl.find ctx num in
	       ChangeDoChildrenPost(Mem(newname,idx,t), (fun x -> x))
	  with Not_found -> DoChildren




  method visit_alvalue = self#visit_rlvalue

       (* Note: this depends on the order the visitor visits children.
         * in particular, visit_binding must be called before visiting
         * the expression in which the variable is bound *)
  method visit_exp e = 
    begin
      match e with
	  Let(Temp var,e1,e2) ->  
	    let f x = self#exit_decl var; x in
	      ChangeDoChildrenPost(e, f)
	| _ -> DoChildren
    end

  (* when we visit a binding, we first visit the rhs, then add the
     binding to the context *)
  method visit_binding (l,e) =  
    begin
      match l with
	  Temp name -> 
	    let e' = exp_accept self e in 
	    let name' = self#alpha_vary_decl name in 
	      ChangeTo (Temp name' ,e')
	| _ -> DoChildren
    end

end

let alpha_vary_exp = 
  let vis = new var_renamer in 
    exp_accept vis

let alpha_vary_stmt =
  let vis = new var_renamer in 
    stmt_accept vis

let alpha_vary_stmts sl = 
  let vis = new var_renamer in 
  let sl' = List.map (fun x -> stmt_accept vis x) sl in 
    sl'


let alpha_vary_program (dl,sl) = 
  let vis = new var_renamer in 
  let sl' = List.map (fun x -> stmt_accept vis x) sl in 
    (dl,sl')

(* remove blocks from stmts. Returns a new program. Internal
   use only! *)
let rec remove_blks decls rstmts stmts =
  match stmts with
    | s :: stmts_t ->
	(match s with
	   | Block(dl,blksl) ->
	       (* grab decls, and push block statements onto
		  stmts-left-to-process, careful to use tail-recursive
		  functions to handle large blocks *)
	       remove_blks (List.rev_append dl decls) 
		 rstmts
		 (List.rev_append (List.rev blksl) stmts_t)
	   | Function(v, topt, dl, b, Some(Block(bdl, bsl))) ->
	       let (bdl',bsl') = remove_blks bdl [] bsl in 
	       let f = Function(v,topt, dl, b, Some(Block(bdl',bsl'))) in
		 remove_blks decls (f::rstmts) stmts_t
	   | Attr(Block _ as s',a) ->
	       remove_blks decls rstmts (s'::stmts_t)  
	   | Attr(Function(v,topt,dl,b,Some(Block(bdl, bsl))), a) ->
	       let (bdl',bsl') = remove_blks bdl [] bsl in 
	       let f = Function(v,topt, dl, b, Some(Block(bdl',bsl'))) in
		 remove_blks decls (f::rstmts) stmts_t
		   
	   | _ -> remove_blks decls (s::rstmts)  stmts_t
	)
    | [] -> (List.rev decls, List.rev rstmts)


let descope_stmts (ctx:var_renamer) sl = 
  let sl' = ExtList.List.map (fun x -> stmt_accept ctx x) sl
  in 
    remove_blks [] [] sl'

  

let descope_program (dl,sl) = 
  let vis = new var_renamer in
  let sl' = ExtList.List.map (fun x -> stmt_accept vis x) sl
  in 
    remove_blks dl [] sl' 
    
let descope_function  = function
    Function(v,topt, dl, b, Some(blk)) as f ->
      let (dl', sl') = descope_program ([], [f]) in 
	assert((List.length dl') = 0);
	assert((List.length sl') = 1);
	List.hd sl'
  | _ -> raise (Invalid_argument "descope_function")


