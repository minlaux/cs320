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



let sort_below (x, y, z) =
    if x < y then 
        if x < z then x 
        else z 
    else if y < z then y 
    else z


let sort_above (x, y, z) =
    if x > y then 
        if x > z then x 
        else z 
    else if y > z then y 
    else z


let sort2 (x, y) =
  if x < y then (x, y)
  else (y, x)


let sort5 (a, b, c, d, e) =
    let (n0, n4) = sort2 (a, e) in
    let (n1, n3) = sort2 (b, d) in
    let n2 = sort_below (c, n3, n4) in
    let min = sort_below (n0, n1, n2) in
    let max = sort_above (n0, n1, n2) in
    (min, n0, n1, n2, max)
    
;;


(* ************************************************ *)

