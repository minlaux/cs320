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

#use "./../assign1.ml"
;;

#use "./../../../classlib/OCaml/MyOcaml.ml"
;;


(*
appends character c to begining of string s
*)

let combine_str_char (s : string) (c : char): string =
    let len_s = string_length s in
    let result = string_init (len_s + 1) (fun i ->
        if i = 0 then c

        else 
            string_get (s, (i - 1))) in
        result


let intrep_add (ds1 : string) (ds2 : string): string =
    




(* ****** ****** *)