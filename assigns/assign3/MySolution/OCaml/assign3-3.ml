(*
Assign3-3:
HX-2023-09-26: 10 points
//
The function [list_nchoose(xs)(n0)]
returns all the subsequences of xs that are
of length n0.
//
let
list_nchoose
(xs: 'a list)(n0: int): 'a list list =
//
Please give a NON-RECURSIVE implementation of
list_nchoose based on list-combinators. Note that
the order of the elements in a list representation
of a subsequenc is SIGNIFICANT. For instance, [1;2]
and [2;1] are DIFFERENT.
//
*)


(* ****** ****** *)

#use "./../../assign3.ml"
;;

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;

(*
list_map function from lecture
*)

let rec
list_map
(xs: 'a list)(fopr: 'a -> 'b): 'b list =
match xs with
| [] -> []
| x1 :: xs -> fopr(x1) :: list_map(xs)(fopr)


(*
returns subsets of a given set list 
from assign3-2.ml
*)

let list_subsets (xs: 'a list): 'a list list =
    let initial = [[]] in
    let fopr acc x =
        let mapped = list_map(acc)(fun sub -> x :: sub) in
        list_append(acc)(mapped)
    in
    list_foldleft xs initial fopr


let list_nchoose(xs: 'a list)(n0: int): 'a list list =

    let len = foreach_to_length(list_foreach(xs)(fun i -> 
        list_foldleft(xs)
        ))
    let map_len = list_map len in 

(*  
List.map (foreach_to_length list_foreach) subsets

    let combine acc x =
        let append_x_to_subsets subset =
            if List.length subset < n then
                (x :: subset) :: acc
            else
                subset :: acc
        in
        List.fold_left append_x_to_subsets [] acc
    in
    list_foldleft xs [[]] combine
*) 

(* ****** ****** *)