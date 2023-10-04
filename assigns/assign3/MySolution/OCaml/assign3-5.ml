(*
//
Assign3-5:
HX-2023-09-28: 30 points (bonus)
//
Here is an implementation of the famous 8-queen puzzle:
https://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/x631.html
//
Please give a NON-RECURSIVE implementation that solves the 8-queen puzzle.
//
Hint: Please think of programming in terms of combinators.
//
//
type board_t =
int * int * int * int * int * int * int * int
//
fun
queen8_puzzle_solve(): board_t list =
(*
returns a list of boards consisting of all the solutions to the puzzle.
*)
*)


(* ****** ****** *)

#use "./../../assign3.ml"
;;

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;

let N = 8 in 


let rec print_dots i =
    if i > 0 then (
    print_dots (i - 1)
  )


let print_row i =
    print_dots i;
    print_string "Q ";
    print_dots (N - i - 1);
    print_newline ()


let board_get bd i =
    List.nth bd i


let board_set bd i j =
    let rec update_list lst i j =
        match lst with
        | [] -> []
        | hd :: tl ->
            if i = 0 then j :: tl
            else hd :: update_list tl (i - 1) j
    
    in
    update_list bd i j


let safety_test1 i0 j0 i j =
    j0 <> j && abs (i0 - i) <> abs (j0 - j)


let safety_test2 i0 j0 bd i =
    let rec check_safety i =
        if i >= 0 then (
            if safety_test1 i0 j0 i (board_get bd i) then
                check_safety (i - 1)
            else false
    )   else true
    
    in
    check_safety (i - 1)


let rec search bd i j nsol =
    if j < N then (
        let test = safety_test2 i j bd (i - 1) in
        if test then (
            let bd1 = board_set bd i j in
            if i + 1 = N then (
                search bd i (j + 1) (nsol + 1)
      ) else (
            search bd1 (i + 1) 0 nsol
      )
    ) else (
        search bd i (j + 1) nsol
    )
  ) else (
        if i > 0 then (
            search bd (i - 1) (board_get bd (i - 1) + 1) nsol
        ) else nsol
  )


let queen8_puzzle_solve () =
    let bd = [0; 0; 0; 0; 0; 0; 0; 0] in
    search bd 0 0 0


;;


(* ****** ****** *)