(*
Assign3-1:
HX-2023-09-26: 10 points
A matrix can be represented as a list of lists.
For instance, [[1;2;3];[4;5;6];[7;8;9]] represents
the following 3x3 square matrix:
1 2 3
4 5 6
7 8 9
Please implement matrix_transpose that returns
the transpose of a given matrix:
let rec
matrix_transpose(xss: 'a list list): 'a list list
For instance, the transpose of the above matrix
is given as follows:
1 4 7
2 5 8
3 6 9
You are allowed to define recursive functions to
solve this problem.
*)


(* ****** ****** *)

#use "./../../assign3.ml"
;;

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;

exception MySubscript

(*
if empty, raise exception
if not empty, return list x1
*)

let list_head(xs : 'a list): 'a =
match xs with 
| [] -> raise MySubscript
| x1 :: xs -> x1


(*
if empty, raise exception
if not empty, return list xs
*)

let list_tail(xs : 'a list): 'a list =
match xs with 
| [] -> raise MySubscript
| x1 :: xs -> xs


(*
helper function that maps list
*)

let rec map_list(xs)(f) =
  match xs with
  | [] -> []
  | x :: rest -> f x :: map_list(rest)(f)


let rec matrix_transpose(xss: 'a list list): 'a list list =
    match xss with
    | [] -> []
    | [] :: _ -> []
    | _ ->
        let column = map_list(xss)(list_head) in
        let rest = map_list(xss)(list_tail) in
        column :: matrix_transpose(rest)
;;


(* ****** ****** *)