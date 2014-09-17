(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Functions for dataflow analysis of VinE programs.

    Stuff here is based on the {i Data-Flow Analysis} chapter from
    {u Advanced Compiler Design & Implemenation} by Steven Muchnick.
    
*)

val live_variables :
  ?globals:Vine.var list ->
  Vine.stmt list #Vine_cfg.cfg -> Vine_cfg.bbid -> Vine.var list

(** [do_dce ?globals graph]  perform dead code elimination on
    [graph]. If [globals] is provided, the list of variables is
    considered live-out.  Return true iff dead code was detected and
    eliminated.  
*)
val do_dce : ?globals:Vine.var list -> Vine.stmt list #Vine_cfg.cfg -> bool

module ConstantProp :
  sig
    type cptype = Unknown | NonConst | Const of Vine.exp
    module L : sig
      type t = Top | Ht of cptype Vine.VarHash.t
      val lookup : cptype Vine.VarHash.t -> Vine.var -> cptype
    end
    val stmt_transfer : cptype Vine.VarHash.t -> Vine.stmt -> unit
  end


(** perform dataflow constant propagation. return two functions for
    in[bb] and out[bb] *)
val constant_propagation : 
  ?init:ConstantProp.L.t -> 
  (Vine.stmt list Vine_cfg.cfg) ->
  (Vine_cfg.bbid -> ConstantProp.L.t) *
  (Vine_cfg.bbid -> ConstantProp.L.t) 

(** [constant_propagation_and_folding ?init graph f]
    perform constant propagation and folding on graph
    [graph]. [init] the initial state for the dataflow algorithm;
    when not provided, Top is used.  [f] is a simplification
    function, e.g., use "id" if you don't want simplification, or
    Vine_opt.simplify_stmt would be a reasonable thing to use when you
    want to simplify.
    
    Returns true iff constant propagation replaced some variable
    with a constant. *we do not monitor whether the simplification
    changed a statement*. 
*)
val constant_propagation_and_folding :
  ?init:ConstantProp.L.t -> 
  Vine.stmt list Vine_cfg.cfg -> (Vine.stmt -> Vine.stmt) -> bool

module SetOfInt64 : Set.S with type elt = int64
module AbsVar :
sig
  type t = RegOrTemp of Vine.var | LocalVar of int64 * int64
  val hash : t -> int
  val equal : t -> t -> bool
(*  val compare : Vine.var -> Vine.var -> int *)
end
  
module AbsVarHash : Hashtbl.S with type key = AbsVar.t
  
module StackOffProp :
  sig
    type rangetype = IntVal of int64 | Infinity 
    type staddrtype = ConstStackAddr of int64 | RangeStackAddr of rangetype * rangetype
    type stofftype =  NonConstOff | ConstOff of int64   
    type cptype = Unknown | OffType of stofftype | StackAddr of staddrtype | Top
    module L : sig
      type t = Ht of (cptype AbsVarHash.t)
    end
  end


(** [stackoff_propagation_and_folding ?init graph] perform simple
    abstract interpretation using an abstract value for the ESP to be
    able to compute the value of the ESP from the base of the stack,
    at the start of each block in the cfg  [graph]. [init] the
    initial state for the dataflow algorithm; when not provided, Top
    is used. 
    
    The output is printed on stdout. For each basic block, it prints
    the offset in the ESP from the base of the activation record for
    the given cfg.
*)
val stackoff_propagation_and_folding :
  ?init:StackOffProp.L.t ->
  Vine.stmt list Vine_cfg.cfg  -> unit

(** [get_max_stack_depth ?init graph] perform simple abstract
    interpretation using an abstract value for the ESP to be able to
    compute the value of the ESP from the base of the stack, at the
    start of each block in the cfg [graph]. [init] the initial state
    for the dataflow algorithm; when not provided, Top is used.
    
    Returns the maximum depth of ESP seen in the function. If the max
    depth is unknown due to imprecison in the modelling of abstract
    values, a special value -10000 is returned.
    
*)  
val get_max_stack_depth :
  ?init:StackOffProp.L.t ->
  Vine.stmt list Vine_cfg.cfg -> int64

(** [get_local_vars ?init graph] perform simple abstract
    interpretation using an abstract value for the ESP to be able to
    compute the value of the ESP from the base of the stack, at the
    start of each instruction in the cfg [graph]. [init] the initial
    state for the dataflow algorithm; when not provided, Top is used.

    Returns the set of all local variable base offsets seen. 
*)  
val get_local_vars :
  ?init:StackOffProp.L.t ->
  Vine.stmt list Vine_cfg.cfg -> SetOfInt64.t
  

module SsaDataflow :
  sig

    (** calculates a limited form of available expressions. We only
	keep track of available expressions that are a binary
	arithmetic operation on a scalar, e.g., z1= a+2, z2=b*5, 
	z3= c << 4  are in type t as [(z1,a+2); (z2,b*5);(z3,c<<4)]. 
	We do not keep track of non-scalars such as when a, b, or c
	would be a memory operation, or relational operations such as
	<,==, etc.
	This dataflow is used to simplify simple binary operations in
	scalar_available_simplify.
    *)
    module ScalarAvailable : 
    sig
        module SASetT :
          sig
            type t = Ssa.var * Ssa.exp
            val compare : Vine.var * Ssa.exp -> Vine.var * Ssa.exp -> int
          end
	  
	module L :
	sig
	  type t = Set.Make(SASetT).t 
	end
    end

    (** simplifies based upon scalar available expressions.  
	For example, if you have:
	  x = y + 2;
          z = x + 3;
        This will simplify the last statement to:
          z = y + 5;
    *)
    val scalar_available_simplify : 
      ?init:ScalarAvailable.L.t -> Ssa.stmt list Vine_cfg.cfg -> bool

    module ConstantProp :
      sig
        type cptype = Unknown | NonConst | Const of Ssa.value
	module L : sig
          type t = Top | Ht of cptype Vine.VarHash.t
          (* val cpmeet : cptype -> cptype -> cptype *)
          val lookup : cptype Vine.VarHash.t -> Ssa.var -> cptype
          (* val t_to_ht : t -> cptype Vine.VarHash.t *)
          (* val meet : t -> t -> t *)
	end
        val stmt_transfer : cptype Vine.VarHash.t -> Ssa.stmt -> unit
      end
    
      (** constant_propagation dataflow analysis, returning two
	  functions for in[bb] and out[bb]. *)
    val constant_propagation : 
      ?init:ConstantProp.L.t -> 
      Ssa.stmt list Vine_cfg.cfg ->
      (Vine_cfg.bbid -> ConstantProp.L.t) *
	(Vine_cfg.bbid -> ConstantProp.L.t) 
	
    (** [constant_propagation_and_folding ?init graph f]
	perform constant propagation and folding on graph
	[graph]. [init] the initial state for the dataflow algorithm;
	when not provided, Top is used.  [f] is a simplification
	function, e.g., use "id" if you don't want simplification.
	
	Returns true iff constant propagation replaced some variable
	with a constant. *we do not monitor whether the simplification
	changed a statement*. 
    *)
    val constant_propagation_and_folding :  
      ?init:ConstantProp.L.t -> Ssa.stmt list Vine_cfg.cfg -> 
      (Ssa.stmt -> Ssa.stmt) -> bool

    (** [do_dce ?globals graph] will do dead code elimination on
	[graph].  [globals] contains a list of globals whose values
	should be considered live at the graph exit. This modifies 
	[graph] in place. We return true iff dead code was detected
	and deleted.

	in SSA, a variable is live at its definition site iff its list of
	uses is not empty. Therefore, calculating live variables is really
	just a matter of calculating whether or not a variable has any
	uses. (p445  ML Tiger book ). 
	
    *)      
    val do_dce :
      ?globals:Ssa.var list -> Ssa.stmt list Vine_cfg.cfg -> bool

    (** Global Value Numbering *)
    module GVN :
      sig
        type content =
            Val of Ssa.value
          | BinOp of Ssa.binop_type
          | UnOp of Ssa.unop_type
          | Cast of Ssa.cast_type * Ssa.typ
          | Unknown of string
          | Get of Ssa.typ
          | Set of Ssa.typ
          | Phi
          | Fun of Ssa.lvalue * Ssa.label
        type vgnode' = {
          mutable label : content;
          mutable ops : vgnode list;
          mutable lvs : Ssa.var list;
	  pos : Vine_cfg.bbid * int
        }
        and vgnode = vgnode' ref
        type vginfo = {
          lval2node :  vgnode Vine.VarHash.t;
          mutable nodelist : vgnode list;
          mutable partitions : vgnode list ref list;
        }

(* 	val get_v2n_tbl : vginfo -> (Ssa.lvalue, vgnode) Hashtbl.t *)
	val pp_vginfo : (string -> unit) -> vginfo -> unit
	val pp_vgnode : (string -> unit) -> vgnode -> unit
	val get_vginfo_of_ssa : Ssa.stmt list Vine_cfg.cfg -> vginfo
	val get_gvn_of_value : vginfo -> Ssa.value -> int
	val get_gvn_of_exp : vginfo -> Ssa.exp -> int
	val gvn_replacer : Ssa.stmt list Vine_cfg.cfg ->  vginfo
      end

    (** [do_gvn cfg] will perform global value numbering
	simplifications on [cfg] using reasonable default
	values. [cfg] is modified in place. *)
    val do_gvn : Ssa.stmt list Vine_cfg.cfg -> unit

    (** Strongly Connected Component based Value Numbering *)
    module SCCVN :
    sig
      (** Performs simplifications based on SCCVN analysis.
	  Returns true iff it changed anything *)
      val replacer : Ssa.stmt list Vine_cfg.cfg -> bool
	
      (** Tells whether two SSA values are the same *)
      val aliased
	: Ssa.stmt list Vine_cfg.cfg -> Ssa.value -> Ssa.value -> bool option
    end

    (** [simplify_graph cfg n] will simplify [cfg] by
	iterating up to [n] times through a list of common optimizations
	using common default arguments. We return the new graph, and
	whether or not the last set of optimizations  changed any code. If
	the bool value is true, this may indicate that further
	optimizations are possible. If the bool value is false, this
	likely indicates the optimizations reached a steady state.
	
	Note [cfg] is modified in place.
    *)
    val simplify_graph : Ssa.stmt list Vine_cfg.cfg -> 
      ?globals:Ssa.var list -> int -> 
      (Ssa.stmt list Vine_cfg.cfg * bool)

  end

(** [simplify_graph cfg n] will simplify [cfg] by
    iterating up to [n] times through a list of common optimizations
    using common default arguments. We return the new graph, and
    whether or not the last set of optimizations  changed any code. If
    the bool value is true, this may indicate that further
    optimizations are possible. If the bool value is false, this
    likely indicates the optimizations reached a steady state.

    This is currently implemented by essentially converting cfg to SSA
    form, then calling SsaDataflow.simplify_graph.  
*)
val simplify_graph : Vine.stmt list Vine_cfg.cfg -> 
  ?globals:Vine.var list -> int -> 
  Vine.stmt list Vine_cfg.cfg * bool



(** 6 Helper functions for writing other dataflow problems *)

val transfer_stmt_to_block : ('s -> 'l -> 'l) -> ('n -> 's list) -> 'n -> 'l -> 'l

(** Like [transfer_stmt_to_block] but for the functor interface *)
val fwd_transfer_stmt_to_block : 
  ('s -> 'l -> 'l) -> 's list #Vine_cfg.cfg -> Vine_cfg.bbid -> 'l -> 'l

(** Like [fwd_transfer_stmt_to_block] but for the functor interface *)
val rev_transfer_stmt_to_block : 
  ('s -> 'l -> 'l) -> 's list #Vine_cfg.cfg -> Vine_cfg.bbid -> 'l -> 'l
