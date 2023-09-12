(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
*)


(* ****** ****** *)


#use "./../assign0.ml"
;;


(*
recursevly reverses input string
finds index of last character to place first when initialising string
*)

let stringrev(cs: string): string =
let len = String.length cs in
let reversed = String.init len (fun i ->
let index = len - i - 1 in
String.get cs index) in
reversed

;;



(* ****** ****** *)