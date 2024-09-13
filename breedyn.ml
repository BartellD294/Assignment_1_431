(* Assignment01 Questions 10-13 
*  Author: Noah Breedy
*)

let arr = [3;4;1;2;5];;

(* Question 10 *)
let rec smallest x = match x with
                | [] -> invalid_arg "Empty list"
                | h::[] -> h
                | h::t -> smallest (min h (List.hd t) :: (List.tl t));;
smallest arr;;

(* Question 11.a *)
type btree = 
          Empty
          |  Leaf of float
          |  Node of (float * btree * btree);;

(* Question 11.b *)
let myTree = Node(12., Node (4., Leaf 5., Empty), Leaf 45.);;

(* Question 12 *)
let rec treeDepth = function 
      | Empty  -> 0
      | Leaf _ -> 1 
      | Node(_,l,r) -> 1 + max (treeDepth l) (treeDepth r);;

treeDepth myTree;;

(* Question 13 *)
let rec numLeafs = function 
      | Empty  -> 0
      | Leaf _ -> 1 
      | Node(_,l,r) -> 1 + numLeafs l + numLeafs r;;

numLeafs myTree;;