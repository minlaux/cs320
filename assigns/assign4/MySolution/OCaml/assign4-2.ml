(*
//
Assign4-2:
//
HX-2023-10-05: 10 points
//
Please enumerate all the pairs of natural
numbers. Given pairs (i1, j1) and (i2, j2),
(i1, j1) should be enumerated ahead of (i2, j2)
if i1+j1 < i2+j2.
//
let theNatPairs: (int*int) stream = fn () => ...
//
*)


(* ****** ****** *)


#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;


let theNatPairs: (int*int) stream = fun () ->
    let rec pairs(i)(j) =
        StrCons((i, j), fun () ->
            if j = 0 then 
                pairs(0)(i + 1)

            else
                pairs(i + 1)(j - 1)

        )
    in 
    pairs(0)(0)

;;

(*
else if j = 0 then 
    pairs(0)(i + 1)
*)

(* ****** ****** *)