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


(*
main parse function
takes input string
returns expr option of input string
*)

let parse (s: string): expr option = 
    let cs = string_listize(s) in 
    parse_expr(cs)


(*
parse Int function
takes input char 
returns int option of input char
*)

let parse_int(c: char): int option =
    let t = digit_of_char(c) in 
    if (0 <= t) && (t <= 9) then 
        Some (Int t) 
    else 
        None


(*
parse expr function
takes input char list 
returns expr option of input char list
*)

let parse_expr(cs: char List): expr option = 
    match cs with 
    | x :: [] ->                            
        match parse_int(x) with 
        | Some i -> Some (Int(i))
        | None -> None                      
    | x :: '+' :: xs ->                     
        match parse_int(x) with 
        | Some i ->                         
            match parse_expr(xs) with 
            | Some r -> Some(Add(i, r))
            | None -> None                  
        | None -> None                      



(* ****** ****** *)