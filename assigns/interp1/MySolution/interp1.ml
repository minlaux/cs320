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
*)
⟨digit⟩ ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
⟨nat⟩ ::= ⟨digit⟩ | ⟨digit⟩⟨nat⟩
⟨int⟩ ::= ⟨nat⟩ | -⟨nat⟩
⟨bool⟩ ::= True | False
⟨const⟩ ::= ⟨int⟩ | ⟨bool⟩ | Unit


(* 
grammar: programs 
*)
⟨prog⟩ ::= ⟨coms⟩
⟨com⟩ ::= Push ⟨const⟩ | Pop | Trace
         | Add | Sub | Mul | Div
         | And | Or | Not
         | Lt | Gt
⟨coms⟩ ::= ϵ | ⟨com⟩; ⟨coms⟩


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


type mem = (string * int) list


let rec fetch(s: string)(m: mem): int =
   match m with
   | (x, v) :: xs -> 
      if s = x then 
         v 
      else 
         fetch s xs 
  

let rec eval_step_s (x:expr_v) (m:mem): expr_v =
  match x with
  | V v1 -> D (fetch v1 m)
  | Add (D v1,D v2) -> D(v1+v2)
  | Sub (D v1,D v2) -> D(v1-v2)
  | Add (D v1,e2) -> let e2' = eval_step_s e2 m in Add(D v1,e2')
  | Sub (D v1,e2) -> let e2' = eval_step_s e2 m in Sub(D v1,e2')
  | Add (V v1,e2) -> let v2 = fetch v1 m  in Add(D v2,e2)
  | Sub (V v1,e2) -> let v2 = fetch v1 m in Sub(D v2,e2)


let rec eval_step(g: prog)(m: mem): prog =
   match g with 
   | Push c -> 
   | Pop -> 
   | And (B false, _) -> Some (B false)
   | And (B true, B v2)


let rec parse_com(com_str: string): prog =
   match com_str with
   | "Push" :: rest -> parse_const (List.hd rest)
   | "Pop" :: _ -> Pop
   | "Trace" :: _ -> Trace
   | "Add" :: _ -> Add
   | "Sub" :: _ -> Sub
   | "Mul" :: _ -> Mul
   | "Div" :: _ -> Div
   | "And" :: _ -> And
   | "Or" :: _ -> Or
   | "Not" :: _ -> Not
   | "Lt" :: _ -> Lt
   | "Gt" :: _ -> Gt
   | _ -> failwith "Invalid command"


let rec to_string(g: prog): string =
   match g with 
   | And -> ""
   | And (hd :: tl) ->




    | Add([]) -> "" 
    | Add(hd :: tl) -> 
        list_foldleft(tl)("(add" ^ sexpr_to_string hd)
            (fun acc e -> "(add" ^ sexpr_to_string e) ^ ")"
    | SMul([]) -> "" 
    | SMul(hd :: tl) -> 
        list_foldleft(tl)("(mul" ^ sexpr_to_string hd)
            (fun acc e -> "(mul" ^ sexpr_to_string e) ^ ")"






let interp(s: string): string list option = 





(* ****** ****** *)