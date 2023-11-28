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


(* GRAMMAR *)

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


(* TYPE DEFINITIONS *)

type 'a parser = char list -> ('a * char list) option

type const =
  | Int of int
  | Bool of bool
  | Unit

type prog =
  | Push of const | Pop | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not | Lt | Gt


(* HELPER FUNCTIONS *)

let const_to_string(c: const): string =
   match c with 
   | Int i -> str (char_of_digit i)
   | Bool true -> "True"
   | Bool false -> "False"
   | Unit -> "Unit"

let prog_to_string (p: prog): string = 
   match p with 
   | Push c -> string_append("Push")(const_to_string c) 
   | Pop -> "Pop"
   | Trace -> "Trace"
   | Add -> "Add"
   | Sub -> "Sub"
   | Mul -> "Mul"
   | Div -> "Div"
   | And -> "And"
   | Or -> "Or"
   | Not -> "Not"
   | Lt -> "Lt"
   | Gt -> "Gt"

let ws : unit parser = many whitespace >| ()


(* PARSERS *)

let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (string_listize s)

let int_parser: const parser =
   (let* _ = char '-' in
   let* x = natural in pure (Int (-x)))
   <|>
     (let* x = natural in pure (Int x))

let bool_parser: const parser =
   ((literal "True") >>= fun _ ->
   pure (Bool true)) 
  <|> 
  ((literal "False") >>= fun _ ->
   pure (Bool false))

let unit_parser : const parser =
  (literal "Unit") >>= fun _ ->
  pure Unit

let const_parser: const parser =
  int_parser <|> bool_parser <|> unit_parser

let push_parser = 
  (literal "Push") >>= fun _ ->
  whitespace >>= fun _ ->
  const_parser >>= fun c -> 
  ws >>= fun _ ->
  pure (Push c)

let pop_parser = 
  (literal "Pop") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure Pop

let trace_parser = 
  (literal "Trace") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure Trace

let add_parser = 
  (literal "Add") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure Add

let sub_parser = 
  (literal "Sub") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure Sub

let mul_parser = 
  (literal "Mul") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure Mul

let div_parser = 
  (literal "Div") >>= fun _ ->
  whitespace >>= fun _ ->
  int_parser >>= fun i -> 
  ws >>= fun _ ->
  pure Div

let and_parser = 
  (literal "And") >>= fun _ -> 
  ws >>= fun _ ->
  pure And

let or_parser = 
  (literal "Or") >>= fun _ -> 
  ws >>= fun _ ->
  pure Or

let not_parser = 
  (literal "Not") >>= fun _ -> 
  ws >>= fun _ ->
  pure Not

let prog() =
   push_parser <|> pop_parser <|> trace_parser <|>
   add_parser <|> sub_parser <|> mul_parser <|> div_parser <|>
   and_parser <|> or_parser <|> not_parser 


(* INTERPRETER FUNCTIONS *)

let eval_step(p: prog)(stack: const list) =
  match p with
  | Push x -> x :: stack
  | Pop -> 
      (match stack with 
      | _ :: rest -> rest
      | _ -> failwith "Panic")
  | Add -> 
      (match stack with
      | Int i :: Int j :: rest -> Int (i + j) :: rest
      | _ -> failwith "Panic")
  | Mul -> 
      (match stack with
      | Int i :: Int j :: rest -> Int (i * j) :: rest
      | _ -> failwith "Panic")
  | Sub -> 
      (match stack with
      | Int i :: Int j :: rest -> Int (i - j) :: rest
      | _ -> failwith "Panic")
  | Div -> 
      (match stack with
      | Int i :: Int j :: rest ->
        if i = 0 then 
         failwith "Panic"
        else Int (i / j) :: rest
      | _ -> failwith "Panic")
  | Gt -> 
      (match stack with
      | Int i :: Int j :: rest -> Bool (i > j) :: rest
      | _ -> failwith "Panic")
   | Lt -> 
      (match stack with
      | Int i :: Int j :: rest -> Bool (i < j) :: rest
      | _ -> failwith "Panic")
    
  | And -> 
      (match stack with
      | Bool i :: Bool j :: rest -> Bool (j && i) :: rest
      | _ -> failwith "Panic")
    
  | Or -> 
      (match stack with
      | Bool i :: Bool j :: rest -> Bool (j || i) :: rest
      | _ -> failwith "Panic")
    
  | Not -> 
      (match stack with
      | Bool i :: rest -> Bool (not i) :: rest
      | _ -> failwith "Panic")
    
  | Trace -> 
      match stack with
      | [] -> failwith "Panic: Empty stack"
      | i :: _ -> print_endline (const_to_string i); stack

let rec eval_mem(stack: const list): string list =
  match stack with
  | [] -> []
  | x :: rest -> const_to_string x :: eval_mem rest

let interp (s: string)(stack: const list): string list option =
  match parse(prog())(s) with
  | Some (p, _) -> Some (eval_mem(eval_step p stack))
  | None -> None