
(*
Assign0-1: 10 points
Please find the first integer N such that the
evaluation of fact(N) in OCaml yields an Overflow
exception.
*)

(* 
find first number that causes fact to return 0
val fact100 : int = 0

fact100 is 2^64 (which is 64 bits)
fact100 / 2^64 remainder is 0
*)


let rec
fact(N: int): int =
if N > 0 then N * fact(N - 1) else 1
;;

let rec 
myloop(x: int): int =
if fact(x) = 0 then x else myloop(x + 1) 
;;





if n / 2^64 