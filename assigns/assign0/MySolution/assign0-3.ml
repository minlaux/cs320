(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)


(* ****** ****** *)


#use "./../assign0.ml"
;;


(*
appends character c to begining of string s
*)

let combine_str_char (s : string) (c : char): string =
    let len_s = string_length s in
    let result = string_init (len_s + 1) (fun i ->
        if i = 0 then c

        else 
            string_get (s, (i - 1))) in
        result


(* 
converting integer to string value of the integer 
base case: one-digit integer; return: string of int
calls function recursively until reach base case
builds string from remainders
*)

let rec int2str (i0: int): string =
    if i0 < 0 then 
        let new_i0 = i0 * (-1) in
        let new_str = int2str (new_i0) in
        let neg = chr (ord '-') in
        combine_str_char new_str neg
    
    else if i0 < 10 then 
        let result = chr (ord '0' + i0) in
        string_init 1 (fun i -> result)
    
    else
        let num = i0 mod 10 in
        let num_ch = chr (ord '0' + num) in
        let num_str = string_init 1 (fun i -> num_ch) in
        let rest = int2str (i0 / 10) in
        let len_r = string_length rest in
        let len_result = len_r + 1 in
            string_init len_result (fun i ->
                if i < len_r then 
                    string_get (rest, i)
                else
                    string_get (num_str, (i - len_r)))
;;


(* ****** ****** *)