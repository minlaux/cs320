(*
Assign0-2: 10 points
Please implement a function that tests whether a
given natural number is a prime:
fun isPrime(n0: int): bool
*)


(* ****** ****** *)


#use "./../assign0.ml"
;;

(* checks for x divisor in n0 *)

let rec myloop(n0 : int)(x : int): bool =
if x <= 1 then true 
else if n0 mod x = 0 then false
else myloop n0 (x - 1)
;;

(* asserts if n0 is prime *)

let rec isPrime(n0 : int): bool =
if n0 <= 1 then false
else if n0 = 2 then true
else myloop n0 (n0 - 1)
;;


(* ****** ****** *)