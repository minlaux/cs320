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


type mem = (string * int) list


let rec fetch(s: string)(m: mem): int =
   match m with
   | (x, v) :: xs -> 
      if s = x then 
         v 
      else 
         fetch s xs 


let integer: int parser =
   (let* _ = char '-' in
   let* x = natural in pure(-x))
   <|>
   (let* x = natural in pure x)


let bool: prog parser =
   (let* x = "True" in 
      pure(B true))
   <|>
   (let* x = "False" in 
      pure (B false))

let parse_const: const parser =

let rec to_string x: string = 



let rec eval_step(g: prog)(m: mem): prog =
   match g with 
   | Push c -> Some list_concat[[c]; mem]
   | Pop -> 
   | Trace -> 
   | And (B false, _) -> Some (B false)
   | And (B true, B v2) -> Some (B v2)
   | And (B true, e2) -> 
      (match eval_step e1 with 
      | None -> None 
      | Some e1' -> (And (e1', e2)))


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
   | Int x -> str(char_of_digit x)
   | B True -> "True"
   | B False -> "False"
   | U Unit -> "Unit"
   | _ -> "Panic" 


let interp(s: string): string list option = 

*)


(* ****** ****** *)


type const =
  | CInt of int
  | CBool of bool

type stack_op =
  | Push of const
  | Pop
  | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt

type program = stack_op list

let rec parse_const (const_str : string) : const =
  match const_str with
  | "True" -> CBool true
  | "False" -> CBool false
  | _ ->
    if String.length const_str > 0 && const_str.[0] = '-'
    then CInt (-int_of_string (String.sub const_str 1 (String.length const_str - 1)))
    else CInt (int_of_string const_str)

and parse_com (com_str : string) : stack_op =
  let tokens = String.split_on_char ' ' com_str in
  match tokens with
  | "Push" :: rest -> Push (parse_const (String.concat " " rest))
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


and parse_coms (coms_str : string list) : program =
  List.map parse_com coms_str

let interpret_program (program : program) : const list option =
  let rec eval (stack : const list) (ops : stack_op list) : const list option =
    match ops with
    | [] -> Some stack
    | op :: rest ->
      match op with
      | Push c -> eval (c :: stack) rest
      | Pop -> (match stack with _ :: tl -> eval tl rest | _ -> None)
      | Trace ->
        (List.map (function CBool b -> string_of_bool b | CInt i -> string_of_int i) stack |> String.concat " " |> print_endline;
         eval stack rest)
      | Add | Sub | Mul | Div | And | Or | Not | Lt | Gt ->
        (* Handle binary operations *)
        let binary_op (f : int -> int -> int) : const list option =
          match stack with
          | CInt a :: CInt b :: tl -> eval (CInt (f b a) :: tl) rest
          | _ -> None
        in
        (match op with
         | Add -> binary_op (+)
         | Sub -> binary_op (-)
         | Mul -> binary_op ( * )
         | Div -> binary_op (/)
         | And -> binary_op (fun a b -> if a <> 0 && b <> 0 then 1 else 0)
         | Or -> binary_op (fun a b -> if a <> 0 || b <> 0 then 1 else 0)
         | Not -> (match stack with CInt a :: tl -> eval (CInt (if a = 0 then 1 else 0) :: tl) rest | _ -> None)
         | Lt -> binary_op (fun a b -> if b < a then 1 else 0)
         | Gt -> binary_op (fun a b -> if b > a then 1 else 0)
         | _ -> None)
  in
  eval [] program

let interp (program_str : string) : string list option =
  let coms_str = String.split_on_char ';' program_str in
  let program = parse_coms coms_str in
  match interpret_program program with
  | Some result -> Some (List.map (function CBool b -> string_of_bool b | CInt i -> string_of_int i) result)
  | None -> None

let () =
  match interp "Push False; Push False; And; Not; Trace; Push False; Not; Push False; Not; Or; Trace;" with
  | Some result -> List.iter print_endline result
  | None -> print_endline "Error in program"
