(*
assign1-4: 20 points
Given two strings ds1 and ds2 of digits, please
implement intrep_add(ds1)(ds2) that returns the
string representing the sum of the two integers
represented by ds1 and ds2. Note that the returned
string should contain no leading zeros. (And we
use the empty string to represent the number zero).

fun
intrep_add(ds1: string)(ds2: string): string

For instance, intrep_add("1116123")("222987") = "1339110"

Note that ds1 and ds2 can be arbitrarily long. Thus,
converting ds1 and ds2 to integers can cause overflow.
*)



(* ****** ****** *)

#use "./../../assign0/MySolution/assign0-3.ml"
;;

#use "./../../assign0/MySolution/assign0-4.ml"
;;

#use "./../assign1.ml"
;;

#use "./../../../classlib/OCaml/MyOcaml.ml"
;;


let intrep_add (ds1 : string) (ds2 : string): string =
    let ds1_int = str2int ds1 in
    let ds2_int = str2int ds2 in 
    let sum = ds1_int + ds2_int in 
    int2str sum


(* ****** ****** *)