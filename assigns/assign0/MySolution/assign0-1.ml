(*
Assign0-1: 10 points
Please find the first integer N such that the
evaluation of fact(N) in OCaml yields an Overflow
exception.
*)


(* ****** ****** *)


#use "./../assign0.ml"
;;

(* calculate the factorial of n *)

let rec fact(n : int) =
if n > 0 then n * fact(n - 1) 
else 1
;;

(* check for overflow *)

let rec myloop(x : int) =
if fact(x) = 0 then x 
else myloop(x + 1) 

let myans = myloop(0)
;;


(* ****** ****** *)