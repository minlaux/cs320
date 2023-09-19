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


let compare_chars (c0 : char) (c1 : char): char =
    if ord c0 > ord c1 then c1
    else c0


let string_merge (cs1 : string) (cs2 : string): string =
    if cs1 = "" then cs2

    else if cs2 = "" then cs1 

    else 
        let merged = 
            string_make_fwork (fun fwork -> 
            string_foreach cs1 (fun c1 -> 
            string_foreach cs2 (fun c2 ->
            let m_char = compare_chars c1 c2 in
            fwork m_char
            ))) in
        merged
    

;;

let () =
  let result = string_merge "135" "2468" in
  print_string result



(* ****** ****** *)