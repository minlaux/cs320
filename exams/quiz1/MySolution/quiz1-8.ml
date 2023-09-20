(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)


(*
sorts a tuple of 2 integers
*)

let sort2 (a, b) =
  if a < b then (a, b)
  else (b, a)


(*
sorts a tuple of 3 integers
*)

let sort3 (a, b, c) =
    if a <= b && a <= c then
        if b <= c then 
            (a, b, c)
        else 
            (a, c, b)
    
    else if b <= a && b <= c then
        if a <= c then 
            (b, a, c)
        else 
            (b, c, a)
    
    else
        if a <= b then 
            (c, a, b)
        else 
            (c, b, a)


(*
sorts a tuple of 4 integers
*)

let sort4 (a, b, c, d) =
    let (a, b) = sort2 (a, b) in
    let (a, c) = sort2 (a, c) in
    let (a, d) = sort2 (a, d) in

    let (b, c) = sort2 (b, c) in
    let (b, d) = sort2 (b, d) in

    let (c, d) = sort2 (c, d) in

    let (a, b, c) = sort3 (a, b, c) in
    let (a, b, d) = sort3 (a, b, d) in
    let (a, c, d) = sort3 (a, c, d) in

    let (b, c, d) = sort3 (b, c, d) in
    (a, b, c, d)


(*
sorts a tuple of 5 integers
*)

let sort5 (a, b, c, d, e) =
    let (a, b, c, d) = sort4 (a, b, c, d) in
    let (a, b, c, e) = sort4 (a, b, c, e) in
    let (a, b, d, e) = sort4 (a, b, d, e) in
    let (a, c, d, e) = sort4 (a, c, d, e) in

    let (b, c, d, e) = sort4 (b, c, d, e) in

    let (a, b, c, d, e) = sort5 (a, b, c, d, e) in
    (a, b, c, d, e)
    
;;


(* ************************************************ *)

