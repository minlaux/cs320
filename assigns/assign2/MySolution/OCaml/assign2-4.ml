(*
//
Assign2-4: 10 points
//
Please give a combinator-based implementation of
string_sepjoin_list:
let
string_sepjoin_list
(sep: string)(xs: string list): string = ...

For instance,
string_sepjoin_list("")(["1";"22";"333"]) = "122333"
For instance,
string_sepjoin_list(",")(["1";"22";"333"]) = "1,22,333"
For instance,
string_sepjoin_list(";;")(["11";"22";"33"]) = "11;;22;;33"
*)


(* ****** ****** *)

#use "./../../assign2.ml"
;;

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;



let string_sepjoin_list(sep: string)(xs: string list): string =
    let concat_list(xs) =
        string_make_fwork (fun append_char ->
            let rec loop = function
                | [] -> ()
                | [x] -> string_foreach(x)(append_char)
                | x :: xs' -> 
                    string_foreach(x)(fun c ->
                        append_char(c);
                    );
                    string_foreach(sep)(append_char);
                    loop(xs')
            in
            loop(xs)
        )
    in
    concat_list(xs)
;;

(* ****** ****** *)