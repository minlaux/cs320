(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)

(* ****** ****** *)


#use "./../assign1.ml"
;;

#use "./../../../classlib/OCaml/MyOcaml.ml"
;;


let string_longest_ascend(xs: string): string =
    let len = string_length xs in
    
    let rec find_longest(i: int)(curr: string)(max_sub: string)(max_len: int) =
        let curr_len = string_length curr in 

        if i >= len then
            max_sub
        
        else
            let c = string_get_at xs i in
            let prev = 
                if curr_len > 0 then 
                    string_get_at curr (curr_len - 1) 
                
                else 
                    ' ' in
        
            if c >= prev then
                let new_curr = string_snoc curr c in
                let new_curr_len = string_length new_curr in 

                if new_curr_len > max_len then 
                    find_longest(i + 1)(new_curr)(new_curr)(new_curr_len)
                
                else 
                    find_longest(i + 1)(new_curr)(max_sub)(max_len)
            
            else
                find_longest(i + 1)(str c)(max_sub)(max_len)
    in

    if len <= 1 then
        xs
    
    else
        find_longest(1)(str (string_get_at xs 0))(str (string_get_at xs 0))(1)

;;


(* ****** ****** *)