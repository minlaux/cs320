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
helper function to get list length
*)

let rec length_list (xs: 'a list): int =
  list_foldleft xs 0 (fun acc _ -> acc + 1)


let list_nchoose (xs: 'a list)(n0: int): 'a list list =
    let initial = [[]] in
    let fopr(acc)(x) =
    let mapped = list_map(acc)(fun sub -> 
        x :: sub
    ) 
    in
    list_append(acc)(mapped)

  in
  let subsets = list_foldleft xs initial fopr in
  let fil_sub = list_foldleft subsets [] (fun acc subset ->
      if length_list subset = n0 then 
      subset :: acc else acc
  )
  in
  list_reverse(fil_sub)

;;


(* ****** ****** *)