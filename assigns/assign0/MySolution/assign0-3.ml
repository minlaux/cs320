(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)


(* ****** ****** *)


#use "./../assign0.ml"
;;


(* converting int to string from an int to char list *)


let int2str(i0: int): string =

let rec int2char (n : int) (acc : char list) =
if n < 10 then 
Char.chr (Char.code '0' + n) :: acc

else 
let num = n mod 10 in

let num_char = Char.chr (Char.code '0' + num) in

int2char (n / 10) (num_char :: acc) in

let chars = 
if i0 = 0 then ['0']

else if i0 < 0 then 
'_' :: (int2char (-i0) [])

else int2char i0 [] in 

let len = List.length chars in

let str_chars = Array.make len ' ' in 

let rec fill_array i chars =
match chars with 
| [] -> ()
| c :: rest ->
str_chars.(i) <- c;
fill_array(i + 1) rest in

fill_array 0 chars;

String.init len (fun i -> str_chars.(i))

;;


(* ****** ****** *)