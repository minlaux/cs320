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


let string_listize (s : string) : char list =
    list_make_fwork(fun work -> string_foreach s work)


let rec trim(cs) =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs


let rec parse_digits(cs: char list) =
    match cs with
    | [] -> ([], cs)
    | c :: cs ->
        let x = ord(c) - ord('0') in 
        if 0 <= x && x <= 9 then
            let xs, cs = parse_digits cs in
            (x :: xs, cs) 
        else 
            ([], c :: cs)
    

let parse_int(cs: char list): sexpr option =
    let xs, cs = parse_digits(cs) in
    match xs with
    | [] -> None
    | _ ->
        let n = list_foldleft(xs)(0)(fun acc x -> acc * 10 + x) in
        Some (SInt n, cs)


let parse_str(s)(cs) =
    let cs0 = string_listize(s) in

    let rec loop(cs)(cs0) =
        match cs, cs0 with
        | c :: cs, c0 :: cs0 -> 
            if c = c0 then 
                loop cs cs0
            else 
                None
        | _, [] -> Some cs
        | _ -> None
    in loop(cs)(cs0)


let attempt(ps)(cs) =
    list_foldleft(ps)(None)(fun acc p ->
        match acc with
        | Some _ -> acc
        | None -> p cs)


let rec sexpr_parse(s: string): sexpr option = 
    let cs = string_listize(s) in
    attempt [parse_int; parse_add; parse_mul] cs 

and parse_exprs(cs) = 
    match sexpr_parse cs with 
    | None -> None 
    | Some (e, cs) ->
        match parse_exprs(trim cs) with 
        | Some (es, cs) -> Some (e :: es, cs)
        | None -> Some ([e], cs)

and parse_add(cs) =
    match parse_str("(add")(cs) with
    | None -> None
    | Some cs ->
        match parse_exprs (trim cs) with
        | Some (es, ')' :: cs) -> Some (SAdd es, cs)
        | _ -> None 

and parse_mul(cs) =
    match parse_str("(mul")(cs) with
    | None -> None
    | Some cs ->
        match parse_exprs(trim cs) with
        | Some (es, ')' :: cs) -> Some (SMul es, cs)
        | _ -> None


let parse(s: string): sexpr option =    
    let cs = string_listize(s) in 
    match cs with 
    | Some (e, []) -> Some e
    | _ -> None