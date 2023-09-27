(*
//
Assign2-2: 10 points
//
Please implement mylist_get_at based
on pattern matching that computes the
element of a given mylist at a given
position.
//
You should call mylist_subscript_exn if
the give position is out-of-bounds.
//
let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a = ...
//
*)


(* ****** ****** *)

#use "./../../assign2.ml"
;;

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;


(*
helper function to find list length 
from assign2-1.ml
*)

let rec mylist_length(xs: 'a mylist): int =
    match xs with
    | MyNil -> 0
    | MyCons(_, tail) -> 1 + mylist_length(tail)
    | MySnoc(head, _) -> 1 + mylist_length(head)
    | MyReverse (x0) -> mylist_length(x0) 
    | MyAppend2(xs1, xs2) -> mylist_length(xs1) + mylist_length(xs2)


let rec mylist_get_at(xs: 'a mylist)(i0: int): 'a =
    if i0 < 0 then
        mylist_subscript_exn ()
    
    else
        match xs with
        | MyNil -> mylist_subscript_exn ()
        | MyCons(x, tail) when i0 = 0 -> x
        | MyCons(_, tail) -> mylist_get_at(tail)(i0 - 1)
        | MySnoc(head, x) when i0 = mylist_length(head) -> x
        | MySnoc(_, _) -> mylist_get_at(xs)(i0 - 1)
        | MyReverse(xs1) -> mylist_get_at(xs1)((mylist_length(xs1)) - 1 - i0)
        | MyAppend2(xs1, xs2) -> 
            let len1 = mylist_length(xs1) in
            if i0 < len1 then
                mylist_get_at(xs1)(i0)
            else
                mylist_get_at(xs2)(i0 - len1)

;;



(* ****** ****** *)