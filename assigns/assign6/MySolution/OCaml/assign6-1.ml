(*
//
Assign6:
//
Please implement a print and parse function. Using parser combinators. When
given a valid string according to the grammar, your parse function returns an
sexpr value encoding the expression.

//
let sexpr_to_string (e : sexpr)  : string       = ...
let sexpr_parse     (s : string) : sexpr option = ...
//

Example (Accepted Strings):
parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])
//
Example (Rejected Strings):
parse "()" = None
parse "(add)" = None
parse "(add 1 2))" = None
parse "((mul 1 2)" = None
//
*)


(* ****** ****** *)


#use "./../../assign6.ml"
;;

#use "./../../../../classlib/OCaml/MyOCaml.ml"
;;


(* HELPER FUNCTIONS*)

(*
returns string representation of int value
*)
let string_of_int(x: int): string = 
    str(char_of_digit x)

(*
acts as String.concat " " sl 
concatenates string list with whitespace " " as dividors
*)
let rec concat sl =
    match sl with
    | [] -> ""
    | [x] -> string_append " " x
    | hd :: tl -> string_append(string_append " " hd)(concat tl)


(* MAIN FUNCTIONS *)

let rec sexpr_to_string(e: sexpr): string =
  match e with
  | SInt x -> string_of_int x
  | SAdd exprs -> 
        let a = foreach_to_map_list list_foreach exprs sexpr_to_string in 
        string_append "(add" (string_append (concat a) ")")
  | SMul exprs -> 
        let a = foreach_to_map_list list_foreach exprs sexpr_to_string in 
        string_append "(mul" (string_append (concat a) ")")

let rec p_sexpr() =
    sint() <|> sadd() <|> smul()

    and sint() =
        let* x = natural in 
        pure (SInt x) << whitespaces

    and sadd() =
        let* _ = keyword "(add" in 
        let* a = many1' p_sexpr in 
        let* _ = keyword ")" in 
        pure (SAdd a)

    and smul() =
        let* _ = keyword "(mul" in 
        let* a = many1' p_sexpr in 
        let* _ = keyword ")" in 
        pure (SMul a)

let sexpr_parse(s: string): sexpr option =
    match string_parse (p_sexpr()) s with
    | Some (x, []) -> Some x
    | _ -> None
