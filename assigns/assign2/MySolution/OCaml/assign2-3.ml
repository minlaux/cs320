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
    
    let iforeach (list: 'xs mylist) (work: 'x0 -> unit): unit =
        
        let rec fold_fn(acc)(element) =
            let new_acc = foldleft list acc (fun a b -> b) in
            work element;
            
            match new_acc with
            | Some acc' -> fold_fn acc' element
            | None -> ()
            in

            let () = fold_fn None None in ()
        in

        iforeach

;;



(*
 match xs with
    | MyNil -> ()
    | MyCons() -> 
    | MySnoc() ->
    | MyReverse() ->
    | MyAppend2() ->
*)

(* ****** ****** *)