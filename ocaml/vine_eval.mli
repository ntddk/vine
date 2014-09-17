(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(**
   The functions and type descriptions for evaluation. We try and not
   be specific to a particular type of evaluation. In particular, we
   parameterized the {! Vine_eval.frame}, {! Vine_eval.callstack},
   and {! Vine_eval.evaluator} with the type of value for
   evaluation.
*)


(** A generic evaluation exception.
    The string is a textual description of what went wrong. *)
exception EvalError of string

(** Exception raised when accessing a non-existent label.
    The label is the missing label.
*)
exception NoSuchLabel of Vine.label


(** Exception raised when an assert fails .
    The expression is the expression that was asserted.
*)
exception AssertFailure of Vine.exp

(** When we evaluate code, we translate the global scope into a
    function, then issue a call to that function. This allows us to
    treat globally executed code just like function code in the
    evaluator. 
*)

(** The name of the function we translate the global scope into *)
val global_function_name : string

(** The return variable for the global function
    [Vine_eval.global_function_name]
 *)
val global_return_var : Vine.var


(** [functify_global_scope program] takes a [program] returns a new
    Vine.program in which executable statements in the global scope
    are moved to a function called {!Vine_eval.global_function_name}.
*)
val functify_global_scope : Vine.program -> Vine.program





(** [pc] is the type of our program counter in the machine. *)
type pc = int 


(** ecode stands for executable code, and is what is
    actually  executed by an evaluator *)
type ecode 

(** mk_ecode translates a vine program into the executable code
    suitable for an evaluator *)
val mk_ecode : Vine.program -> ecode 

(** [label_to_pc ecode label] returns the pc for a label in the
    vine. 
    @raise Not_found if [label] doesn't exist*)
val label_to_pc : ecode -> Vine.label -> pc

(** [pc_to_statement ecode pc] returns the Vine statement for [ecode]
    statement at [pc].
    @raise Not_found if [pc] doesn't exist
*)
val pc_to_stmt : ecode -> pc -> Vine.stmt

(** [get_start_pc code] returns the pc an evaluator of [code] will
    begin executing at.
*)
val get_start_pc : ecode -> pc

(** [print_ecode e f] prints the ecode [e] using printer function [f].
    [f] is called on the string representation of each ecode
    statement.
*)
val print_ecode : ecode -> (string -> unit) -> unit

(** [funname_to_proto_pc code v] returns a function definition for
    function [v] and the pc value for the first executable statement
    in the function.  

    @return a function definition Vine.stmt and the first pc for
    executing a function instance.

    @raise Not_found if [v] does not exist.
*)
val funname_to_proto_pc : ecode -> Vine.label -> (Vine.stmt * pc)

(** the type of a stack frame.  A stack frame is parameterized by the
    type of values in the frame, e.g., a concrete evaluator would
    likely use a concrete value for 'a 
    
    A frame has a return lvalue where the result of a call should be
    put.  It is up to the user whether the caller or callee
    sets the lvalue. 

    A frame also has a return pc, which is the pc to resume when the
    frame is removed.  Again, whether it is callee or caller saved is
    up to the user.
*)
type 'a frame

(** [mk_empty_frame v] returns a new empty frame with frame name
    [v]. the return pc in [v] is set to {!Vine_eval.default_pc} *)
val mk_empty_frame : Vine.label -> 'a frame

(** [mk_activation_frame vl slink retlvopt retpc stmt] makes an 
    activation frame with static link frame number [slink],
    return lvalue option [retlvopt], return pc [retpc], created from
    function definition statement [stmt]. Frame variables are
    initialized with values [vl], i.e., [vl] is the actuals to a
    function call.  The length of [vl] should match the number of
    declared arguments to the function. Usually the static link
    [slink] is 0, meaning look in the global frame for any variables
    not in the current frame.
    @raise Invalid_argument if stmt is not a Vine.Function
*)
val mk_activation_frame : 'a list -> int option -> 
  Vine.lvalue option -> pc -> Vine.stmt -> 'a frame

(** [remove_frame_variable frame n] removes the variable [n] from the
    frame [frame]. It does nothing (does not raise an error) if [n]
    does not exist.
*)
val remove_frame_variable : 'a frame -> Vine.var -> unit

(** [new_frame_variable frame n] creates a new variable [n] in frame
    [frame], initialized with value None. *)
val new_frame_variable : 'a frame -> Vine.var -> unit

(** [get_frame_value frame n] gets the value associated with variable
    [n] in frame [frame].  
    
    @return a value, which is None if [n] has not been
    initialized, else Some('a)
*)
val get_frame_value : 'a frame -> Vine.var -> 'a option

(** [set_frame_value frame n v] sets the value of [n] in frame [frame]
    to be [a]. This is how you would initialize or update a frame
    variable value. 
*)
val set_frame_value : 'a frame -> Vine.var -> 'a -> unit

(** [set_frame_return_pc frame pc] sets the saved return value of
    [frame] to [pc] *)
val set_frame_return_pc : 'a frame -> pc -> unit

(** [get_frame_return_pc frame] returns the saved return value for
    [frame] *)
val get_frame_return_pc : 'a frame -> pc

(** [set_frame_return_lvalue frame lv] sets the return lvalue for a
    frame to [lv]. Note that whether the lvalue is set by a callee or
    caller is up to a particular implementation. *)
val set_frame_return_lvalue : 'a frame -> Vine.lvalue -> unit

(** [get_frame_return_lvalue frame] returns the return lvalue for
    frame [frame]. It returns None if no return lvalue has been set.
*)
val get_frame_return_lvalue : 'a frame -> Vine.lvalue option

(** a callstack is a stack of frames, each of type 'a. The callstack
    acts like a stack in that it is LIFO, but also allows us to look
    at particular stack frames. In addition, {!Vine_eval.get_callstack_value}
    and {!Vine_eval.set_callstack_value} follow the static link through the
    callstack to find and set a value. *)
type 'a callstack

(** mk_callstack returns a new callstack, empty callstack *)
val mk_callstack : unit -> 'a callstack

(** [push_frame cs frame] pushes a new [frame] onto the callstack [cs] *)
val push_frame : 'a callstack -> 'a frame -> unit

(** [nth_frame cs i] returns the {! Vine_eval.frame} at position [i] in the
    callstack [cs]

    Note: Position [i] is relative to the bottom of the stack.
 *)
val nth_frame : 'a callstack -> int -> 'a frame

(** [pop_frame cs] removes a frame from the callstack [cs] *)
val pop_frame : 'a callstack -> unit

(** [top_frame cs] returns the top-most frame on callstack [cs] *)
val top_frame : 'a callstack -> 'a frame

(** [Vine_eval.get_callstack_value cs n] returns the value associated with [n]
    in the callstack [cs].  We follow the static link down the
    callstack, e.g., if [n] is not in the top-most frame, but the
    frame has static link 0, we will then look in frame at position 0
    for the value. We return None if [n] has not been initialized,
    else Some(value).
    @raise  EvalError if [n] is not found.
*)
val get_callstack_value : 'a callstack -> Vine.var -> 'a option

(** [set-callstack_value cs n v] behaves like {!Vine_eval.get_callstack_value}
    to locate the variable position in the callstack of [n], then sets
    the variable [n]'s value to [v]
    @raise EvalError if [n] is not found
*)
val set_callstack_value : 'a callstack -> Vine.var -> 'a -> unit

(** [new_callstack_variable cs n] creates a new callstack variable in
    the top-most frame. [n]'s value is initially None *)
val new_callstack_variable : 'a callstack -> Vine.var -> unit

(** [remove_callstack_variable cs n] removes variable [n] from the
    callstack [cs]. It follows the static link (as described in
    !{Vine_eval.get_callstack_value} when necessary. 
    @raise EvalError if [n] is not found
*)
val remove_callstack_variable : 'a callstack -> Vine.var -> unit

(** [get_frame_pointer cs] returns the current frame pointer for
    callstack [cs] *)
val get_frame_pointer : 'a callstack -> int

(** our basic evaluator class. Specific evaluators, e.g., a concrete
    evaluator, will instantiate this class and define at least
    eval_exp, step, and run.
*)
class virtual ['a] evaluator :
object

  (** the pc value. this is not available directly to users of this
  class, but can be manipulated with set_pc, get_pc. (Put here so
  users of interface can use the get_pc and set_pc members do not have
  to be virtual) *)
  val mutable pc : pc 

  (** get the current pc value of this evaluator *)
  method get_pc : unit -> pc
    
  (** set the current pc value of this evaluator *)
  method set_pc : pc -> unit

  (** evaluate an expression in the current machine context. We allow
  expressions to be evaluated since they don't change the machine
  state, which we like to keep hidden. *)
  method virtual eval_exp : Vine.exp -> 'a

  (** Take one step in the evaluation. 
      @return if the machine is in the halt state, else false if the
      machine can take another step.  Taking a step of the machine in
      the halt state does nothing but still returns true *)
  method virtual step : unit -> bool

  (** run the machine until it produces a final value (or raises an
      exception).
      @return the final value 
  *)
  method virtual run : unit -> 'a
end


