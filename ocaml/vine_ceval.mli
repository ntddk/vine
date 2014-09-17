(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** This is the interface for concrete evaluation of our IR. 
*)


(** Whether to allow reading of uninitialized values. The default is
    false. Change to true with care...you will be allowing reads 
    of uninitialized values. We currently return 0 for all such reads.
 *)
val allow_uninitialized_reads : bool ref

(** a value type of our evaluator.  Note that array's are copied when
    assied, e.g.,
   {v
    var a,b:reg32_t[reg32_t]
    ...
    a = b;
    v}
    means copy the contents of array b to a.

    @deprecated We only define our own
    value types since Vine.value does not have arrays and labels as
    value types. Also, we keep our type with our integer value,
    instead of typ * Int(int64) as in Vine. *)
type value = 
    Int of Vine.typ * int64 
  | Str of string
  | Arr of (value, value) Hashtbl.t
  | Lbl of Vine.label

(** [value_to_string v] returns the string representation of a
    value [v]. the string is annotated with the correct type, e.g., it
    will output 0:reg8_t for Int(Vine.REG_8, 0L). *)
val value_to_string : value -> string

(** [value_to_const v] returns a {Vine.exp}  for a value [v]. The exp
    will always be a Vine Constant. *)
val value_to_const : value -> Vine.exp


(** our concrete evaluator class. You can instantiate an evaluator on
    some code, then evaluate it. This implements
    {!Vine_eval.evaluator} *)
class concrete_evaluator :
  Vine.program ->
  object

    (** return whether the machine is halted or not *)
    method is_halted : unit -> bool

    (** See {!Vine_eval.evaluator.get_pc} *)
    method get_pc : unit -> Vine_eval.pc 

    (** See {!Vine_eval.evaluator.set_pc} *)
    method set_pc : Vine_eval.pc -> unit  

    (** run a program to produce a final value *)
    method run : unit -> value 

    (** evaluate an expression in the current machine state *)
    method eval_exp : Vine.exp -> value

    (** take one step in the current machine state *)
    method step : unit -> bool

    (** return the {!Vine_eval.ecode} for the machine *)
    method get_ecode : unit -> Vine_eval.ecode

  end
