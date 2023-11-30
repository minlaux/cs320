(*
Assign3-4:
HX-2023-09-26: 20 points
Given a word x of length n, another word is a buddy
of x if x and y differ exactly at one position. For
instance, "live" is a buddy "love" (and "love" is also
a buddy of "live").
//
Please give a NON-RECURSIVE implementation of
list_of_buddies that returns a list of all the buddies
of a given word.
//
let
list_of_buddies(word: string): string list = ...

(*
FYI. The concept of word buddies is used in the following game:
https://xanadu-lang.github.io/xats2js/docgen/CodeBook/Doublet/2020-11-29/
https://github.com/xanadu-lang/xats2js/tree/master/docgen/CodeBook/Doublet
*)
*)


(* ****** ****** *)


#use "./../../assign3.ml"
;;

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;



let list_x2foreach(xs: 'a list)(ys: 'b list)(work: 'a -> 'b -> unit): unit =
    list_foreach(xs)(fun x -> 
        list_foreach(ys)(fun y -> 
            work x y))


let list_cross(xs: 'a list)(ys: 'b list): ('a * 'b) list =
    list_make_fwork (fun work ->
        list_x2foreach(xs)(ys)(fun x y -> 
            work(x, y)))


(*
list_map function from lecture
*)

let rec list_map(xs: 'a list)(fopr: 'a -> 'b): 'b list =
    match xs with
    | [] -> []
    | x1 :: xs -> fopr(x1) :: list_map(xs)(fopr)


let list_of_buddies(word: string): string list =
    let word_list = string_listize(word) in
    let alphabet = "abcdefghijklmnopqrstuvwxyz" in 
    let alpha_list = string_listize(string_foreach(alphabet)(fun i ->
        string_make_fwork (fun work -> 
            str(i))
    )) in

    list_cross(word_list)(alpha_list)

;;


(* ****** ****** *)