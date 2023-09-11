(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
*)


(* ****** ****** *)


#use "./../assign0.ml"
;;


(* assigns each character in string to an array index in reverse order *)

let char_arr (str : string): char array =
let str_len = String.length str in
let arr = Array.make str_len ' ' in
let inc = ref 0 in

for i = str_len - 1 downto 0 do

let char_at_i = String.get str i in
arr.(!inc) <- char_at_i;
inc := !inc + 1;

done;

arr

;;

(* converts character array to string *)

let arr2str (ch : char array): string =
let length = Array.length ch in
String.init length (fun i -> ch.(i))

;;


(* returns input string in reverse order *)

let stringrev (cs : string): string =
if String.length cs = 0 then ""

else
arr2str(char_arr (cs))

;;


(* ****** ****** *)