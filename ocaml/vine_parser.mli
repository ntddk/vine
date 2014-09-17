(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** Higher-level functions for invoking the lexer and parser.
    For instance, most programs are going to want to use
    our defspecs as part of the default program arguments
*)

(** the CPP preprocessor flags to pass in. You shouldn't need to
    change this unless you are working with something other than cpp *)
val flag_cppFlags : string list ref

(** if true, the user has requested to pretty-print the program *)
val flag_pp : bool ref

(** if true, the user has requested that we try and coerce bad memory
    writes to the correct type (we run the deendizer and break
    everything down to byte-level read/writes) *)
val flag_coerce : bool ref

(** whether to typecheck the input code *)
val flag_typecheck : bool ref

(** whether to keep track of line numbers. By default this is off
    since the code is inefficient *)
val flag_track_lines : bool ref

(** a default spec list for Arg. Supports -cpp to set the cpp flags,
    -pp to request pretty printing, -coerce to enable coercion,
    -nocheck to disable typechecking, and -linenums to enable line numbers  *)
val defspecs : (string * Arg.spec * string) list


(** [parse_exp dl lexbuf] parses lexbuf as an expression under
    declarations [dl] (dl are added to the scope before parsing) *)
val parse_exp : Vine.decl list -> Lexing.lexbuf -> Vine.exp

(** [parse_exp dl lexbuf] parses lexbuf as an expression under
    declarations [dl] (dl are added to the scope before parsing.)

   Does not reset the global scoping context. See source for
   parse_file on how to do this if needed
 *)
val parse_exp_from_string : Vine.decl list -> string -> Vine.exp


(** Parse a program given the lexbuf.
   Does not reset the global scoping context. See source for
   parse_file on how to do this if needed
*)
val parse_lexbuf : Lexing.lexbuf -> Vine.program

(** Parse  a channel given the lexbuf. Does not reset
the global scoping context. See source for parse_file on
how to do this if you need it. *)
val parse_channel : in_channel -> Vine.program

(** Preprocess a file using cpp. The returned channel should
    be closed with {!Unix.close_process_in} *)
val preprocess : string -> in_channel

(** Parse a file and return the program.  Does reset
the global parsing context on each call, i.e., each file
is considered completely separate. *)
val parse_file : string -> Vine.program

