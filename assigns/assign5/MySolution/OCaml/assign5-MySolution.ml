(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)


(* ****** ****** *)


#use "./../../assign5.ml"
;;

#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;


let parse (s: string): expr option = 
    let r = (string_listize(s)) in 
    parse_expr(r)

let parse_int(c: char): int option =
    let t = int_of_char(c) - 48 in 
    if (0 <= t) && (t <= 9) then 
        Some (Int t) 
    else 
        None

let rec parse_expr(cs: char list): expr option = 
    match cs with
    | '(' :: x :: ')' ->
        match parse_int(x) with 
        | Some i -> Some (Int(i))
        | None -> None
    | '(' :: 'a' :: 'd' :: 'd' :: x :: xs ->
        match parse_int(x) with 
        | Some i -> 
            match parse_expr(trim(xs)) with
            | Some r -> Some (Add i)
            | None -> None 
        | None -> None
    | '(' :: 'm' :: 'u' :: 'l' :: x :: xs ->
        match parse_int(x) with 
        | Some i -> 
            match parse_expr(trim(xs)) with
            | Some r -> Some (Mul i)
            | None -> None 
        | None -> None
    | None -> None



(* ****** ****** *)