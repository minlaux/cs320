(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

let interp (s : string) : string list option = (* YOUR CODE *)
*)


(* ****** ****** *)


#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*
(*
helper functions
*)

let pure_a(a: 'a) =
   fun xs -> Some [a]


(* 
grammar: constants 

⟨digit⟩ ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
⟨nat⟩ ::= ⟨digit⟩ | ⟨digit⟩⟨nat⟩
⟨int⟩ ::= ⟨nat⟩ | -⟨nat⟩
⟨bool⟩ ::= True | False
⟨const⟩ ::= ⟨int⟩ | ⟨bool⟩ | Unit
*)


(* 
grammar: programs 

⟨prog⟩ ::= ⟨coms⟩
⟨com⟩ ::= Push ⟨const⟩ | Pop | Trace
         | Add | Sub | Mul | Div
         | And | Or | Not
         | Lt | Gt
⟨coms⟩ ::= ϵ | ⟨com⟩; ⟨coms⟩
*)


type prog =
   | Push of prog * const
   | Pop of prog
   | Trace of prog
   | Add of prog * prog
   | Sub of prog * prog
   | Mul of prog * prog
   | Div of prog * prog
   | And of bool
   | Or of bool
   | Not of bool
   | Lt of bool
   | Gt of bool


type const =
   | Int of int 
   | B of bool 
   | U of unit


type mem = string list


let rec fetch(s: string)(m: mem): int =
   match m with
   | x :: xs -> 
      if s = x then 
         v 
      else 
         fetch s xs 


let integer: int parser =
   (let* _ = char '-' in
   let* x = natural in pure_a(-x))
   <|>
   (let* x = natural in pure_a x)


let bool: prog parser =
   (let* x = "True" in 
      pure_a(B true))
   <|>
   (let* x = "False" in 
      pure_a(B false))


let rec eval_step(g: prog)(m: mem): prog =
   match g with 
   | Push c -> Some list_concat[[c]; mem]
   | Pop -> 
   | Trace -> 

   | Not -> 
      match g with 
   | i :: j :: m ->
      match g with 
      | Add -> 
         let v = fetch i m in 
         let e = fetch j m in 
         Push (v + e)
      | Sub -> 
         let v = fetch i m in 
         let e = fetch j m in 
         Push (v - e)
      | Mul ->
         let v = fetch i m in 
         let e = fetch j m in 
         Push (v * e)
      | Div ->
         let v = fetch i m in 
         let e = fetch j m in 
         Push (v / e)
      | And ->
         let v = fetch i m in 
         let e = fetch j m in 
         if v and e then 
            Push True 
         else 
            Push False
      | Or ->
         let v = fetch i m in 
         let e = fetch j m in 
         if v or e then 
            Push True 
         else 
            Push False 
      | Lt ->
         let v = fetch i m in 
         let e = fetch j m in 
         if v < e then 
            Push True 
         else 
            Push False 
      | Gt ->
         let v = fetch i m in 
         let e = fetch j m in 
         if v > e then 
            Push True 
         else 
            Push False


let rec to_string(g: prog): string =
   match g with 
   | Int x -> str(char_of_digit x)
   | B true -> "True"
   | B false -> "False"
   | U unit -> "Unit"
   | _ -> "Panic" 


let interp(s: string): string list option = 


(* ****** ****** *)


De Morgan’s Law:

Push False;
Push False;
And;
Not;
Trace;
Push False;
Not;
Push False;
Not;
Or;
Trace;

Result: Some ["True"; "True"]
*)

type mem = (string * int) list

type prog =
  | Push of const
  | Pop
  | Trace
  | Add of const * const
  | Sub of const * const
  | Mul of const * const
  | Div of const * const
  | And of const * const
  | Or of const * const
  | Not of const
  | Lt of const * const
  | Gt of const * const

and const =
  | Int of int
  | B of bool
  | U of unit


let rec fetch (s : string) (m : mem) : int option =
  match m with
  | (x, v) :: xs ->
    if s = x then
      Some v
    else
      fetch s xs
  | _ -> None

let integer : const parser =
  (let* _ = char '-' in
   let* x = natural in pure (Int (-x)))
  <|>
  (let* x = natural in pure (Int x))

  
let boolean: const parser =
   (let* _ = keyword "True" in pure (B true))
   <|>
   (let* _ = keyword "False" in pure (B false))

  
let const: const parser =
  integer <|> boolean

let rec prog_step (x: prog): prog option =
  match x with
  | And (B false, _) -> Some (B false)
  | And (B true, B v2) -> Some (B v2)
  | And (B true, e2) -> (
      match prog_step e2 with
      | Some e2' -> Some (And (B true, e2'))
      | _ -> None
    )                 
  | And (Int _, _) -> None
  | And (_, Int _) -> None
  | And (e1, e2) -> (
      match prog_step e1 with
      | None -> None
      | Some e1' -> Some (And (e1', e2))
    )
  | Or (B true, _) -> Some (B true)
  | Or (B false, B v2) -> Some (B v2)
  | Or (B false, e2) -> (
      match prog_step e2 with
      | Some e2' -> Some (Or (B false, e2'))
      | _ -> None
    )                 
  | Or (Int _, _) -> None
  | Or (_, Int _) -> None
  | Or (e1, e2) -> (
      match prog_step e1 with
      | None -> None
      | Some e1' -> Some (Or (e1', e2))
    )

let rec prog' (x: prog) (n: int): prog option =
  if n <= 0 then Some x
  else
    match prog_step x with
    | None -> None
    | Some x' -> prog' x' (n - 1)
