(*
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.

fun intrev10(n: int): int

For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.
*)


(* ****** ****** *)


#use "./../assign1.ml"
;;

#use "./../../../classlib/OCaml/MyOcaml.ml"
;;


(*
tail recursively reverses digits of n until n < 10
*)

let intrev10(n : int): int =
    let rec tail n acc =
        if n < 10 then
            (acc * 10) + n
        else
            let last = n mod 10 in
            let rest = n / 10 in
            tail rest ((acc * 10) + last) 
    in
    if n mod 10 = 0 then
        failwith "n must not be a multiple of 10"
    else
        tail n 0
;;


(* ****** ****** *)