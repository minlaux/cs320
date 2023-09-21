(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)


(* ****** ****** *)


#use "./../assign1.ml"
;;

#use "./../../../classlib/OCaml/MyOcaml.ml"
;;


let string_merge(cs1: string)(cs2: string): string =
    if cs1 = "" then cs2 

    else if cs2 = "" then cs1 

    else
    let result = string_make_fwork (fun append_char ->
        let rec merge i1 i2 =
            if i1 = string_length cs1 && i2 = string_length cs2 then ()
            
            else if i1 = string_length cs1 then
                (append_char (string_get_at cs2 i2); merge i1 (i2 + 1))
            
            else if i2 = string_length cs2 then
                (append_char (string_get_at cs1 i1); merge (i1 + 1) i2)
            
            else
                let char1 = string_get_at cs1 i1 in
                let char2 = string_get_at cs2 i2 in
                
                if char1 < char2 then 
                    (append_char char1; merge (i1 + 1) i2)
                
                else (append_char char2; merge i1 (i2 + 1))
        in
        merge 0 0
    )
    in
    result

;;



(* ****** ****** *)