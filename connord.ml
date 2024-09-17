(* Question 6 *)
let makepairs x input = List.map (fun y -> (x,y)) input;;

makepairs 5 [1;2;3;4];;

(* Question 7 *)
let rec binomial n k =
  if n = k || k = 0 then 1
  else binomial (n - 1) (k - 1) + binomial (n - 1) k;;

binomial 5 3;;

(* Question 8 *)
let rec dup input = 
  match List.length input with
  | 0 -> []
  | n -> [List.hd input;List.hd input]@dup (List.tl input);;

dup [1;2;3;4];;

(* Question 9 *)
let rec undup input = 
  match input with
  | [] -> []
  | x :: [] -> raise (Failure "uneven length list")
  | x :: y :: rest -> if x != y then raise (Failure "non-symmetrical list") else x::undup rest;;

undup [1;1;2;2;3;3;4;4];;
undup (dup [1;2;3;4]);;
undup [];;
undup [1;1;2;2;3;0;4;4];;
undup [1;1;2;2;3;3;4];;
