(* Derrick Pavia 
   CS431
   assignment 2 even Ocaml problems*)

(* Derrick problem 2 *)
let rec makeList n = 
  match n with
  | _ when n < 0 -> failwith "Negative number" (* n is negative exception is thrown *)
  | 0 -> [] (* zero return empty *)
  | _ -> makeList (n - 1) @ [n];; (* cycle through decrementing n and concatinating with n*)
makeList 5;;

let makeListTail n = 
  let rec helper x lst = 
    match x with
    | _ when x < 0 -> failwith "Negative number" (* n is negative exception is thrown *)
    | 0 -> lst (* When x equals 0 return our list *)
    | _ -> helper (x - 1) (x::lst) (* Decrement x and add it to our list*)
  in
  helper n [];; (* starting from n with empty list *)
makeListTail 5;;

(* Derrick Problem 4 *)
