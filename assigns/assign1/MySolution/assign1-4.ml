(*
assign1-4: 20 points
Given two strings ds1 and ds2 of digits, please
implement intrep_add(ds1)(ds2) that returns the
string representing the sum of the two integers
represented by ds1 and ds2. Note that the returned
string should contain no leading zeros. (And we
use the empty string to represent the number zero).

fun
intrep_add(ds1: string)(ds2: string): string

For instance, intrep_add("1116123")("222987") = "1339110"

Note that ds1 and ds2 can be arbitrarily long. Thus,
converting ds1 and ds2 to integers can cause overflow.
*)



(* ****** ****** *)


#use "./../assign1.ml"
;;

#use "./../../../classlib/OCaml/MyOcaml.ml"
;;


let intrep_add(ds1: string)(ds2: string): string =
    let len1 = string_length ds1 in
    let len2 = string_length ds2 in

    let rec add_strings i j carry result =
        if i < 0 && j < 0 then
            if carry > 0 then
                let carry_char = char_of_digit carry in
                string_snoc (result) (carry_char)
            
            else
                result
        else
            let num1 = 
                if i >= 0 then digit_of_char (string_get_at ds1 i) 
                
                else 
                    0 
            in
            let num2 = 
                if j >= 0 then digit_of_char (string_get_at ds2 j) 
                
                else 
                    0 
            in

            let sum = num1 + num2 + carry in
            let new_digit = sum mod 10 in
            let new_carry = sum / 10 in
            let result_char = char_of_digit new_digit in
            add_strings (i - 1) (j - 1) new_carry (string_snoc result result_char)
        in
        let result_string = add_strings (len1 - 1) (len2 - 1) (0) ("") in

            if result_string = "" then
                "0"
            
            else
                result_string

;;



(* ****** ****** *)
