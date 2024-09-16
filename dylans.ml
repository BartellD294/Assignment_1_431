(*  CS 431 Fall 2024      *)
(*  Group 3 Assignment 1  *)


(* Problem 1 Solution     *)
(* Author:  Dylan Bartell *)
let list_cardinality lst =
  match List.length lst with
    | 0 -> 1
    | 1 -> 2
    | _ -> 0;; (*2 or greater is the only remaining possibility since lists can't be negative length*)

(* example/testing code for problem 1 *)
(*
let a = [];;
let w = list_cardinality a ;;
let b = [1];;
let x = list_cardinality b ;;             
let c = [1;2];;
let y = list_cardinality c ;;
let d = [1;2;3;4;5;6;7;8;9;10];;
let z = list_cardinality d ;;
*)


(* Problem 2 Solution     *)
(* Author:  Dylan Bartell *)
open List
let rotate_list lst =
  match lst with
    | [] -> []
    | hd::tl -> tl @ [hd];;

(* example/testing code for problem 2 *)
(*
let a = [];;
let w = rotate_list a ;;
let b = [1];;
let x = rotate_list b ;;             
let c = [1;2];;
let y = rotate_list c ;;
let d = [1;2;3;4;5;6;7;8;9;10];;
let z = rotate_list d ;;
*)

(* Problem 3 Solution     *)
(* Author:  Dylan Bartell *)
open List
let remove_last lst =
  let tsl = rev lst in  (* reverse list so that I can remove just the first element as the head*)
    match tsl with
      | [] -> []
      | hd::tl -> rev tl;;  (* exclude head element (original last element), then reverse the *)
                            (* remaining list again so that it is no longer in reverse order*)

(* example/testing code for problem 3 *)
(*
let a = [1;2;3;4;5;6;7;8;9;10];;
remove_last a ;;
let b = [10;9;8;7;6;5;4;3;2;1];;
remove_last b ;;             
let c = [];;
remove_last c ;;
*)


(* Problem 4 Solution     *)
(* Author:  Dylan Bartell *)
open List
let rec remove_element lst element =
  match lst with
    | [] -> []
    | hd::tl -> if hd = element
                  then (remove_element tl element)
                  else hd:: (remove_element tl element);;

(* example/testing code for problem 4 *)
(*
let a = [1;2;3;4;5;6;7;8;9;10];;
remove_element a 3;;             
let b = [];;
remove_element b 0;;
*)

(* Problem 5 Solution     *)
(* Author:  Dylan Bartell *)
open List
let rec remove_0s_1s lst =
  match lst with
  | [] -> []
  | 0::tl | 1::tl -> remove_0s_1s tl
  | hd::tl -> hd::remove_0s_1s tl;;

(* example/testing code for problem 5 *)
(*
let a = [0;1;2;3;4;5;6;7;8;9;10];;
let x = remove_0s_1s a;;             
let b = [];;
let y = remove_0s_1s b;;
let c = [0;1;0;1;1;1;3;5;0;0];;
let z = remove_0s_1s c;;
*)