(*
assign1-6: 20 bonus points
A 4-letter sequence abcd is 1324-like
if a < c < b < d holds. For instance, 1234 is
not 132-like; but 2547 is 1324-like.

A string is 1324-avoid if there is no subsequence
abc in this string that is 1324-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 1324-avoid;
For instance, 987654321 is 1324-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 1657 is 1324-like.

Please implement a function string_avoid_1324 that
checks if a given string is 1324-avoid; the function
returns true if and only if the given string is 1324-
avoid.

fun string_avoid_1324(cs: string): bool
*)

(* ****** ****** *)


#use "./../assign1.ml"
;;

#use "./../../../classlib/OCaml/MyOcaml.ml"
;;

#use "./../MySolution/assign1-3.ml"
;;


let string_avoid_1324(cs: string): bool =
    if cs = "" then false 

    else
    let len = string_length cs in

    let rec is_1324like (i : int): bool =
        if i >= len - 3 then
            false

        else
            let a = string_get_at cs i in
            let b = string_get_at cs (i + 1) in
            let c = string_get_at cs (i + 2) in
            let d = string_get_at cs (i + 3) in

            if a < c && c < b && b < d then
                true

            else
                is_1324like (i + 1)
    in

    let rec has_1324like_sub (i : int): bool =
        if i >= len - 3 then
            false
        
        else if is_1324like i then
            true
        
        else
            has_1324like_sub (i + 1)
    in
    not (has_1324like_sub 0)

;;




(* ****** ****** *)