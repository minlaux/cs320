(* ************************************************ *)

(*
Q2-6: 10 points

The function list_reverse return the reverse of a given list.
Please give an implementation of list_reverse based on list_foldright
(not list_foldleft).
*)

(* ************************************************ *)

exception Empty 

let list_reverse(xs: 'a list): 'a list = 
    match xs with 
    | [] -> raise Empty 
    | x1 :: xs -> x1 :: list_foldright(xs)
;;

