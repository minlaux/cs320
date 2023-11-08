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


#use "./../../assign5.ml"
;;

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;


let rec digit() =
    let* x = natural in 
    (if (0 <= x) && (x <= 9) then 
        pure(SInt x)
    else 
        fail)

let rec num() = 
    (let* x = digit() in 
    let* _ = ws in 
    let* y = num() in 
    let* _ = ws in 
    pure(Mul (x, y))
    )
    <|> digit()

let rec expr() = 
    (let* x = num() in 
    let* _ = ws in 
    let* 
    
    )


let rec exprs() = 
    (let* x = expr()
    let* _ = ws in 
    let* y = exprs() in 
    let* _ = ws in 
    pure()
    
    )




let rec sexpr_to_string(e: sexpr): string =
    match e with
    | SInt x -> str(char_of_digit(x))
    | SAdd(t1, t2) -> 
        let first = string_append(sexpr_to_string(t1))("+") in
        (string_append(first)(sexpr_to_string(t2)))
    | SMul(t1, t2) -> 
        let first = string_append(sexpr_to_string(t1))("*") in
        (string_append(first)(sexpr_to_string(t2)))


let sexpr_parse(s: string): sexpr option = 
    let cs = string_listize(s) in 
    match cs with 
    | [] -> Some (x, [])
