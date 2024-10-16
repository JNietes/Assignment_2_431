
(* Problem 1 *)
(* Author: Joaquin Nietes *)

(* Non Tail-Recursive*)
let rec replicate x n =
  match n with
  | n when n < 0 -> failwith "n must be a non-negative integer"
  | 0 -> []
  | _ -> x :: replicate x (n-1)
;;

(* Tail-Recursive *)
let rec tailReplicate x n = 
  let rec helper acc x n =
    match n with
    | n when n < 0 -> failwith "n must be a non-negative integer"
    | 0 -> acc
    | _ -> helper (x :: acc) x (n-1)
  in helper [] x n
;;

replicate "Alan" 100000000;;
replicate "Alan" (-3);;
tailReplicate "Turing" (-3);;
tailReplicate "Turing" 100000000;;
