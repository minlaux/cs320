(*
//
Assign2-1: 10 points
//
Please implement mylist_length based
on pattern matching that computes the
length of a given mylist.
//
let rec
mylist_length(xs: 'a mylist): int = ...
//
*)


(* ****** ****** *)


#use "./../../assign2.ml"
;;

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;



let rec mylist_length(xs: 'a mylist): int =
    match xs with
    | MyNil -> 0
    | MyCons(_, tail) -> 1 + mylist_length(tail)
    | MySnoc(head, _) -> 1 + mylist_length(head)
    | MyReverse (x0) -> mylist_length(x0) 
    | MyAppend2(xs1, xs2) -> mylist_length(xs1) + mylist_length(xs2)

;;


(* ****** ****** *)