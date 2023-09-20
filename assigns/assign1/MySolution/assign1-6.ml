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


let string_avoid_1324(cs: string): bool =
  let len = string_length cs in
  let rec find_1324_subsequence(i: int) (prevA: char) (prevB: char) (prevC: char) (found1324: bool) =
    if i >= len then
      not found1324  (* Return true if no 1324-like subsequence found *)
    else
      let c = string_get_at cs i in
      let prevA' = min c prevA in
      let prevB' = max c prevB in
      let prevC' = if prevC < c then c else prevC in
      if prevA' < prevC' && prevC' < prevB' && c > prevB' then
        find_1324_subsequence (i + 1) prevA' prevB' prevC' true
      else
        find_1324_subsequence (i + 1) prevA' prevB' prevC found1324
  in

  if len < 4 then
    true  (* Not enough characters for a 1324-like subsequence *)
  else
    find_1324_subsequence 0 (char_of_int 127) (char_of_int 0) (char_of_int 127) false
;;



(* ****** ****** *)