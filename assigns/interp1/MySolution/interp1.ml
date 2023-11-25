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

type const =
  | Int of int
  | Bool of bool
  | Unit

type stack = const list

type mem = string list

let string_of_bool(b: bool): string = 
   if b = true then 
      "True"
   else 
      "False"

let int_to_string(i: int): string =
   let c = char_of_digit i in 
   
let string_of_const = function
   | Int x -> int_to_string x
   | Bool true -> "True"
   | Bool false -> "False"
   | Unit -> "Unit"

let eval_step(g: prog)(s: stack)(m: mem): prog =
   match g with 
   | Push c -> 
      match s with 
      | Some x -> c :: x 
      | _ -> x
   | Pop -> 
      match s with 
      | _ :: xs -> xs
      | _ -> s
   | Trace -> 
      match s with 
      | x :: xs -> Some list_concat[[string_of_const x]; m]
      | _ -> Some []
   | Add ->
      match s with 
      | Int i :: Int j :: xs -> (i + j) :: xs 
      | _ -> Some list_concat[["Panic"]; s]
   | Sub ->
      match s with 
      | Int i :: Int j :: xs -> (i - j) :: xs 
      | _ -> Some list_concat[["Panic"]; s]
   | Mul ->
      match s with 
      | Int i :: Int j :: xs -> (i * j) :: xs 
      | _ -> Some list_concat[["Panic"]; s]
   | Div ->
      match s with 
      | Int i :: Int j :: xs -> (i / j) :: xs 
      | _ -> Some list_concat[["Panic"]; s]
   | And -> 
      match m with 
      | Bool true :: Bool true :: xs -> Bool true :: xs 
      | Bool true :: Bool false :: xs -> Bool false :: xs
      | Bool false :: Bool true :: xs -> Bool false :: xs 
      | Bool false :: Bool false :: xs -> Bool false :: xs
      | _ -> Some list_concat[["Panic"]; s]
   | Or -> 
      match m with 
      | Bool true :: Bool true :: xs -> Bool true :: xs 
      | Bool true :: Bool false :: xs -> Bool true :: xs
      | Bool false :: Bool true :: xs -> Bool true :: xs 
      | Bool false :: Bool false :: xs -> Bool false :: xs
      | _ -> Some list_concat[["Panic"]; s]
   | Not ->
      match s with 
      | Bool x :: xs -> not x :: xs
      | _ -> Some list_concat[["Panic"]; s] 
   | Lt ->
      match s with 
      | Int i :: Int j :: xs -> 
         if i < j then 
            Some list_concat[["True"]; s]
         else
            Some list_concat[["False"]; s]
      | _ -> Some list_concat[["Panic"]; s]
   | Gt ->
      match s with 
      | Int i :: Int j :: xs -> 
         if i > j then 
            Some list_concat[["True"]; s]
         else
            Some list_concat[["False"]; s]
      | _ -> Some list_concat[["Panic"]; s]

let interp(s: string): string list option = 
   match parse(prog()) s with
   | Some [program] -> Some (eval program [] [])
   | _ -> None

let () =
  let result = interp "Push 3; Push 3; Mul; Push -4; Push 3; Mul; Add; Push 7; Add; Trace" in
  match result with
  | Some output -> print_endline (String.concat "; " output)
  | None -> print_endline "Invalid program"


(* ****** ****** *)

(*
let rec fetch(s: string)(m: mem): int option =
   match m with
   | x :: xs ->
      if s = x then
         x
      else
         fetch s xs
   | _ -> None

let integer: int parser =
   (let* _ = char '-' in
   let* x = natural in pure(-x))
   <|>
   (let* x = natural in pure x)

let boolean: const parser =
   (let* _ = keyword "True" in 
      pure (B true))
   <|>
   (let* _ = keyword "False" in 
      pure (B false))

let const: const parser =
  integer <|> boolean
*)