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

let parse(p: 'a parser)(s: string) : ('a * char list) option =
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

let eval_step(p: prog)(stack: const list)(trace: string list): const list * string list =
  match p with
  | Push c -> (c :: stack, trace)
  | Pop -> 
      (match stack with 
      | [] -> ([], "Panic" :: trace)
      | c :: rest -> (rest, trace))
   | Trace -> 
      (match stack with
      | [] -> ([], "Panic" :: trace)
      | c :: rest -> (Unit :: rest, const_to_string c :: trace))
  | Add -> 
      (match stack with
      | Int i :: Int j :: rest -> (Int (i + j) :: rest, trace)
      | Int i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Int i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Bool j :: rest -> ([], "Panic" :: trace)
      | Unit :: Unit :: rest -> ([], "Panic" :: trace)
      | [] -> ([], "Panic" :: trace)
      | _ :: [] -> ([], "Panic" :: trace))
   | Sub -> 
      (match stack with
      | Int i :: Int j :: rest -> (Int (i - j) :: rest, trace)
      | Int i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Int i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Bool j :: rest -> ([], "Panic" :: trace)
      | Unit :: Unit :: rest -> ([], "Panic" :: trace)
      | [] -> ([], "Panic" :: trace)
      | _ :: [] -> ([], "Panic" :: trace))
  | Mul -> 
      (match stack with
      | Int i :: Int j :: rest -> (Int (i * j) :: rest, trace)
      | Int i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Int i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Bool j :: rest -> ([], "Panic" :: trace)
      | Unit :: Unit :: rest -> ([], "Panic" :: trace)
      | [] -> ([], "Panic" :: trace)
      | _ :: [] -> ([], "Panic" :: trace))
  | Div -> 
      (match stack with
      | Int i :: Int j :: rest ->
         if j = 0 then 
            ([], "Panic" :: trace)
         else 
            (Int (i / j) :: rest, trace)
      | Int i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Int i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Bool j :: rest -> ([], "Panic" :: trace)
      | Unit :: Unit :: rest -> ([], "Panic" :: trace)
      | [] -> ([], "Panic" :: trace)
      | _ :: [] -> ([], "Panic" :: trace))
   | And -> 
      (match stack with
      | Bool true :: Bool true :: rest -> (Bool true :: rest, trace)
      | Bool false :: Bool true :: rest -> (Bool false :: rest, trace)
      | Bool true :: Bool false :: rest -> (Bool false :: rest, trace)
      | Bool false :: Bool false :: rest -> (Bool false :: rest, trace)
      | Int i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Int j :: rest -> ([], "Panic" :: trace)
      | Int i :: Int j :: rest -> ([], "Panic" :: trace)
      | Int i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Bool j :: rest -> ([], "Panic" :: trace)
      | Unit :: Unit :: rest -> ([], "Panic" :: trace)
      | [] -> ([], "Panic" :: trace)
      | _ :: [] -> ([], "Panic" :: trace))
  | Or -> 
      (match stack with
      | Bool true :: Bool true :: rest -> (Bool true :: rest, trace)
      | Bool false :: Bool true :: rest -> (Bool true :: rest, trace)
      | Bool true :: Bool false :: rest -> (Bool true :: rest, trace)
      | Bool false :: Bool false :: rest -> (Bool false :: rest, trace)
      | Int i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Int j :: rest -> ([], "Panic" :: trace)
      | Int i :: Int j :: rest -> ([], "Panic" :: trace)
      | Int i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Bool j :: rest -> ([], "Panic" :: trace)
      | Unit :: Unit :: rest -> ([], "Panic" :: trace)
      | [] -> ([], "Panic" :: trace)
      | _ :: [] -> ([], "Panic" :: trace))
   | Not -> 
      (match stack with
      | Bool true :: rest -> (Bool false :: rest, trace)
      | Bool false :: rest -> (Bool true :: rest, trace)
      | Int i :: rest -> ([], "Panic" :: trace)
      | Unit :: rest -> ([], "Panic" :: trace)
      | [] -> ([], "Panic" :: trace))
   | Lt -> 
      (match stack with
      | Int i :: Int j :: rest -> (Bool (i < j) :: rest, trace)
      | Int i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Int i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Bool j :: rest -> ([], "Panic" :: trace)
      | Unit :: Unit :: rest -> ([], "Panic" :: trace)
      | [] -> ([], "Panic" :: trace)
      | _ :: [] -> ([], "Panic" :: trace))
  | Gt -> 
      (match stack with
      | Int i :: Int j :: rest -> (Bool (i > j) :: rest, trace)
      | Int i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Bool j :: rest -> ([], "Panic" :: trace)
      | Int i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Int j :: rest -> ([], "Panic" :: trace)
      | Bool i :: Unit :: rest -> ([], "Panic" :: trace)
      | Unit :: Bool j :: rest -> ([], "Panic" :: trace)
      | Unit :: Unit :: rest -> ([], "Panic" :: trace)
      | [] -> ([], "Panic" :: trace)
      | _ :: [] -> ([], "Panic" :: trace))
  

let rec eval_mem(stack: const list): string list =
  match stack with
  | [] -> []
  | x :: rest -> const_to_string x :: eval_mem rest

let interp(s: string): string list option =
  match parse(prog()) s with
  | Some (p, _) -> Some (eval_step p [] [] |> fun (_, trace) -> trace)
  | None -> None

