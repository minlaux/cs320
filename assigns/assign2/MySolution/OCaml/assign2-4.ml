(*
//
Assign2-4: 10 points
//
Please given a combinator-based implementation of
string_sepjoin_list:
let
string_sepjoin_list
(sep: string)(xs: string list): string = ...

For instance,
string_sepjoin_list("")(["1";"22";"333"]) = "122333"
For instance,
string_sepjoin_list(",")(["1";"22";"333"]) = "1,22,333"
For instance,
string_sepjoin_list(";;")(["11";"22";"33"]) = "11;;22;;33"
*)


(* ****** ****** *)

#use "./../../assign2.ml"
;;

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;





(* ****** ****** *)