(*
//
Assign4-4:
//
HX-2023-10-05: 20 points
//
Please enumerate all the permuations of a
given list. The enumeration is required to be
in order. For instance, say xs = [1;2;3], then
the enumeration should be of the following order
[1;2;3], [1;3;2], [2;1;3], [2;3;1], [3;1;2], [3;2;1].
//
let list_permute(xs: 'a list): 'a list stream
*)


(* ****** ****** *)


#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;

let rec insert_everywhere x lst =
  match lst with
  | [] -> [[x]]
  | hd :: tl -> (x :: lst) :: (List.map (fun l -> hd :: l) (insert_everywhere x tl))

let rec permutations lst =
  match lst with
  | [] -> [[]]
  | hd :: tl -> List.flatten (List.map (insert_everywhere hd) (permutations tl))

let rec list_permute xs =
  let rec insert_everywhere x lst =
    match lst with
    | [] -> [[x]]
    | hd :: tl -> (x :: lst) :: (List.map (fun l -> hd :: l) (insert_everywhere x tl))
  in
  match xs with
  | [] -> [[]]
  | hd :: tl ->
    let rec insert_everywhere_in_permutations x perms =
      match perms with
      | [] -> []
      | perm_hd :: perm_tl -> (insert_everywhere x perm_hd) @ (insert_everywhere_in_permutations x perm_tl)
    in
    insert_everywhere_in_permutations hd (list_permute tl)

