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


(*
returns true if a string c of length 3 is 132-like
*)

let is_132like (s : string): bool =
    let a = string_get_at s 0 in
    let b = string_get_at s 1 in
    let c = string_get_at s 2 in

    if a < c && c < b then true

    else false


(*
string_avoid_132 returns true if string cs is 132-avoid

substr creates substring of length 3 from string s 
starting at index i and checks if the substring is 132-avoid
*)

let string_avoid_132 (cs : string): bool =
    if string_length cs < 3 then 
        true
            
    else if string_length cs = 3 && is_132like cs then 
        false

    else
        let rec substr (s : string) (i : int): bool = 
            if i + 2 < string_length s then
                let sub = string_init 3 (fun i0 -> 
                string_get_at s (i + i0)) in

                if is_132like sub then false
                
                else substr s (i + 1)

            else true
                
        in
        substr cs 0

;;



(* ****** ****** *)