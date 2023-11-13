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

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;


let string_of_int(x: int): string = 
    str(char_of_digit(x))


let string_listize (s : string) : char list =
    list_make_fwork(fun work -> string_foreach s work)


let rec sexpr() = 
    (let* x = natural in 
     let* _ = whitespace in 
     let* _ = char '+' in 
     let* _ = whitespace in 
     let* y = sexpr() in
        pure (SAdd [SInt x; y]))
    <|>  
    (let* x = natural in 
     let* _ = whitespace in 
     let* _ = char '*' in 
     let* _ = whitespace in 
     let* y = sexpr() in
        pure (SMul [SInt x; y]))
    <|> 
    let* x = natural in
    (if (0 <= x) && (x <= 9) then pure (SInt x) else fail  )


let rec sexpr_to_string(e: sexpr): string =
    match e with
    | SInt x -> string_of_int(x)
    | SAdd [t1; t2] -> 
        let first = string_append(sexpr_to_string(t1))("+") in
        ( string_append(first)(sexpr_to_string(t2)))
    | SMul [t1; t2] -> 
        let first = string_append(sexpr_to_string(t1))("*") in
        ( string_append(first)(sexpr_to_string(t2)))
    | _ -> ""


let sexpr_parse(s: string): sexpr option =
    match string_parse(sexpr()) s with
    | Some (result, _) -> Some result
    | _ -> None