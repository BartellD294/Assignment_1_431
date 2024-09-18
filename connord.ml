(* Problem 6 Solution *)
(* Author: Connor Damato *)
let makepairs x input = List.map (fun y -> (x,y)) input;;

(* makepairs 5 [1;2;3;4];; *)

(* Problem 7 Solution *)
(* Author: Connor Damato *)
let rec binomial n k =
  match k with
  | 0 -> 1
  | x when n = k -> 1
  | x -> binomial (n - 1) (k - 1) + binomial (n - 1) k;;

binomial 5 3;;

(* Problem 8 Solution *)
(* Author: Connor Damato *)
let rec dup input = 
  match List.length input with
  | 0 -> []
  | n -> [List.hd input;List.hd input]@dup (List.tl input);;

(* dup [1;2;3;4];; *)

(* Problem 9 Solution *)
(* Author: Connor Damato *)
let rec undup input = 
  match input with
  | [] -> []
  | x :: [] -> raise (Failure "uneven length list")
  | x :: y :: rest -> (match x != y with | true -> raise (Failure "non-symmetrical list") | false -> x::undup rest);;

(* undup [1;1;2;2;3;3;4;4];;
undup (dup [1;2;3;4]);;
undup [];;
undup [1;1;2;2;3;0;4;4];;
undup [1;1;2;2;3;3;4];; *)
