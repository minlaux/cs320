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


let list_permute(xs: 'a list): 'a list stream =
    match xs with
    | [] -> StrCons ([], (fun () -> StrNil))
    | x :: rest -> StrCons (list_foreach(xs)(fun i ->
        let rem1 = StrCons( :: i) 
        let rem2 = StrCons(i + 1 :: )
        list_foreach(list_permute(remain))(stream_append(rem1)(rem2)))
    )
;;