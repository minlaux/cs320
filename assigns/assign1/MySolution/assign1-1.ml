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





#use "./../assign1.ml"
;;

#use "./../../../classlib/OCaml/MyOcaml.ml"
;;


(*

*)

let rec intrev10(n : int): int =
    if n mod 10 = 0 then
        failwith "n must not be a multiple of 10"
    else
        let rev =
        int1_rforeach n (fun work -> n * 10)
        intrev10 rev

