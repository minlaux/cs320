(*
assign1-3: 10 points
A 3-letter sequence abc is 132-like
if a < c < b holds. For instance, 123 is
not 132-like; but 596 is 132-like.

A string is 132-avoid if there is no subsequence
abc in this string that is 132-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 132-avoid;
For instance, 987654321 is 132-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 165 is 132-like.

Please implement a function string_avoid_132 that
checks if a given string is 132-avoid; the function
returns true if and only if the given string is 132-
avoid.

fun string_avoid_132(cs: string): bool
*)


(* ****** ****** *)


#use "./../assign1.ml"
;;

#use "./../../../classlib/OCaml/MyOcaml.ml"
;;


let string_avoid_132(cs: string): bool =
    if cs = "" then false 

    else
    let len = string_length cs in

    let rec is_132like (i : int): bool =
        if i >= len - 2 then
            false

        else
            let a = string_get_at cs i in
            let b = string_get_at cs (i + 1) in
            let c = string_get_at cs (i + 2) in

            if a < c && c < b then
                true

            else
                is_132like (i + 1)
    in

    let rec has_132like_sub (i : int): bool =
        if i >= len - 2 then
            false
        
        else if is_132like i then
            true
        
        else
            has_132like_sub (i + 1)
    in
    not (has_132like_sub 0)

;;



(* ****** ****** *)