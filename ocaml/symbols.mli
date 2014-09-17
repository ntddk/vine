(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

(** A functional-style symbol table implementation using red/black
    trees.  This should have O(log n) max depth, O(log n) insert,
    O(log n) mem, and O(log n) get.

    This should act as a drop-in replacement for the most part for
    Hashtbl. 

    red/black tree code was derived from "Introduction to the
    Objective Caml Programming Language" by Jason Hickey, available
    at: http://files.metaprl.org/doc/ocaml-book.pdf

    @deprecated Use Map or PMap instead. 
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.Make.html
    http://ocaml-lib.sourceforge.net/doc/PMap.html

*)


(** the type of our table. 'a must have =, <, and > implemented. 'b
    can be anything. *)
type ('a, 'b) t

(** get all keys in the table *)
val keys : ('a, 'b) t -> 'a list

(** [mem tbl x] returns true if [x] is a key in [tbl], else false *)
val mem : ('a, 'b) t -> 'a -> bool

(** [get tbl x] returns the current binding for [x] in the [tbl]
    @raise Not_found if [x] is not in the table [tbl].
*)
val find :  ('a, 'b) t -> 'a -> 'b

(** [add tbl x y] returns  a new table where key x is bound to
    value y *)
val add : ('a, 'b) t  -> 'a -> 'b -> ('a, 'b) t

(** an empty table *)
val empty : ('a, 'b) t



module type S = 
  sig
    type key
    type 'a t
      
    val empty : 'a t
    val add : 'a t -> key -> 'a -> 'a t
    val find : 'a t -> key -> 'a
    val mem : 'a t -> key -> bool
    val keys : 'a t -> key list
  end

(** Functor to make a symbol table for a specific kind of key *)
module Make(H:Set.OrderedType) : S with type key = H.t

