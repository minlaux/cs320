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


(*
helper function for str2int
calculates base ** exponent
*)

let rec pow base exponent =
if exponent = 0 then 1

else if exponent < 0 then failwith "Exponent must be non-negative"

else base * pow base (exponent - 1)

;;


(*
converts string of an integer to type int 
contains helper function to retrieve character at each index
raises to the appropriate power of 10 depending on index number
*)

let rec str2int (cs: string): int =
    let rec helper cs i =
        let len = string_length cs in
        
        if i >= len then 0

        else
            let c = string_get (cs, i) in

            if '0' <= c && c <= '9' then
                let num = ord c - ord '0' in
                let power = len - i - 1 in
                num * (pow 10 power) + helper cs (i + 1)

            else
                failwith "Invalid character in the string" in
                let len = string_length cs in

                if len = 0 then
                    failwith "Empty string cannot be converted to int"

                else
                    helper cs 0

;;


(* ****** ****** *)
