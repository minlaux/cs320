(*
//
Assign2-4: 10 points
//
Please give a combinator-based implementation of
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


let string_sepjoin_list(sep: string)(xs: string list): string =
    
    let rec inner(str_list: string list): string = 
        match str_list with
        | MyNil -> ""
        | MyCons(x, MyNil) -> x
        | MyCons (x, rest) -> string_append(x)(string_append(sep)(inner(rest)))
        | _ -> ""

    in 
    inner(xs)

;;

(*

list_foreach(str_list)(fun s -> string_append(s)(sep))
*)
(* ****** ****** *)