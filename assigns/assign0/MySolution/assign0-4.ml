(*
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
*)


(* ****** ****** *)


#use "./../assign0.ml"
;;


(* converting string to int from an int to char list *)


let str2int (cs: string): int =
let is_num c = Char.code '0' <= Char.code c && Char.code c <= Char.code '9' in

let rec convert acc index =
if index < 0 then acc

else if is_num cs.[index] then
let num_value = Char.code cs.[index] - Char.code '0' in
let power = String.length cs - index - 1 in
let new_acc = acc + num_value * int_of_float (10.0 ** float_of_int power) in
convert new_acc (index - 1)

else
failwith "Invalid input: not a valid integer string" in
let length = String.length cs in
if length = 0 then
failwith "Empty input: not a valid integer string"

else if cs.[0] = '-' then - (convert 0 (length - 1))

else if cs.[0] = '+' then convert 0 (length - 1)

else
convert 0 (length - 1)


(* ****** ****** *)
