(*
//
Assign2-3: 10 points
//
Please implement foldleft_to_iforeach that turns a
foldleft-loop into a iforeach-loop:
let
foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = ...
*)


(* ****** ****** *)

#use "./../../assign2.ml"
;;

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;


let foldleft_to_iforeach 
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  
    match xs with
    | MyNil -> ()
    | MyCons() -> 
    | MySnoc() ->
    | MyReverse() ->
    | MyAppend2() ->
    
;;



(* ****** ****** *)