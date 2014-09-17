(*
Vine is Copyright (C) 2006-2009, BitBlaze Team.

You can redistribute and modify it under the terms of the GNU GPL,
version 2 or later, but it is made available WITHOUT ANY WARRANTY.
See the top-level README file for more details.

For more information about Vine and other BitBlaze software, see our
web site at: http://bitblaze.cs.berkeley.edu/
*)

type color = Red | Black

type ('a, 'b) t =  
    Node of color * 'a * 'b * ('a,'b) t * ('a,'b) t
  | Leaf 


let mem_ compare =
  let rec mem tree x = 
  match tree with
      Leaf -> false
    | Node(_,y,_,left,right) ->
	match compare x y with
	    0 -> true
	  | i when i < 0 -> mem left x
	  | i (* when i > 0 *) -> mem right x
  in
    mem


let find_ compare =
  let rec find tree x =
    match tree with
	Leaf -> raise Not_found
      | Node(_,y,data,left,right) ->
	  (match compare x y with
	       0 -> data
	     | i when i < 0 -> find left x
	     | i (* when i > 0 *) -> find right x
	  )
  in
    find

let balance = function
    Black, z, dataz, Node (Red, y, datay, Node (Red, x, datax, a, b), c), d 
  | Black, z, dataz, Node (Red, x, datax, a, Node (Red, y, datay, b, c)), d
  | Black, x, datax, a, Node (Red, z, dataz, Node (Red, y, datay, b, c), d)
  | Black, x, datax, a, Node (Red, y, datay, b, Node (Red, z, dataz, c, d)) ->
      Node (Red, y, datay, Node (Black, x, datax, a, b), 
	    Node (Black, z, dataz, c, d))
  | a, b, data, c, d ->
      Node (a, b, data, c, d)

let add_ compare s x d  =
  let rec ins = function
      Leaf -> Node (Red, x, d, Leaf, Leaf)
    | Node (color, y, datay, a, b) ->
	match compare x y with
	    0 -> Node(color,y,d, a, b)
	  | i when i < 0 -> balance (color, y, datay, ins a, b)
	  | _ (* when i > 0 *) -> balance (color, y, datay, a, ins b)
  in
    match ins s with
      | Node (_, y, datay, a, b) -> Node (Black, y, datay, a, b)
      | Leaf -> raise (Invalid_argument "add");;


let keys t = 
  let rec decend vacc nacc = function
    | Leaf -> (match nacc with
		   [] -> vacc
		 | x::ys -> decend vacc ys x
	      ) 
    | Node(_,a,_, f, g) -> decend (a::vacc) (g::nacc) f
  in
    decend [] [] t


let empty = Leaf
let add x = add_ compare x
let find x = find_ compare x
let mem x = mem_ compare x



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

module Make(H:Set.OrderedType) : (S with type key = H.t) = 
struct
  type key = H.t
  type 'a tree = (key, 'a) t
  type 'a t = 'a tree

  let empty =  empty
  let add x = add_ H.compare x
  let find x = find_ H.compare x
  let mem x = mem_ H.compare x
  let keys x = keys x 
end
