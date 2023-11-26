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

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (string_listize s)

let ws : unit parser = many whitespace >| ()

type const =
  | Int of int
  | Bool of bool
  | Unit

type prog =
  | Push of const | Pop | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not | Lt | Gt


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
  (literal "()") >>= fun _ ->
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


let str_to_prog (s: string): prog option = 
  match s with
  | "Push" -> Some (Push (Int 0))
  | "Pop" -> Some Pop
  | "Trace" -> Some Trace
  | "Add" -> Some Add
  | "Sub" -> Some Sub
  | "Mul" -> Some Mul
  | "Div" -> Some Div
  | "And" -> Some And
  | "Or" -> Some Or
  | "Not" -> Some Not
  | "Lt" -> Some Lt 
  | "Gt" -> Some Gt
  | _ -> None


let eval_step stack op =
  match op with
  | Push x -> x :: stack
  | Pop -> 
      match stack with 
      | _ :: rest -> rest
      | _ -> failwith "Panic"
  | Add -> (
      match stack with
      | Int i :: Int j :: rest -> Int (i + j) :: rest
      | _ -> failwith "Panic"
    )
  | Mul -> (
      match stack with
      | Int i :: Int j :: rest -> Int (i * j) :: rest
      | _ -> failwith "Panic"
    )
  | Sub -> (
      match stack with
      | Int i :: Int j :: rest -> Int (i - j) :: rest
      | _ -> failwith "Panic"
    )
  | Div -> (
      match stack with
      | Int i :: Int j :: rest ->
        if i = 0 then 
         failwith "Panic"
        else Int (i / j) :: rest
      | _ -> failwith "Panic"
    )
  | Gt -> (
      match stack with
      | Int i :: Int j :: rest -> Bool (i > j) :: rest
      | _ -> failwith "Panic"
    )
   | Lt -> (
      match stack with
      | Int i :: Int j :: rest -> Bool (i < j) :: rest
      | _ -> failwith "Panic"
    )
  | And -> (
      match stack with
      | Bool i :: Bool j :: rest -> Bool (j && i) :: rest
      | _ -> failwith "Panic"
    )
  | Or -> (
      match stack with
      | Bool i :: Bool j :: rest -> Bool (j || i) :: rest
      | _ -> failwith "Panic"
    )
  | Not -> (
      match stack with
      | Bool i :: rest -> Bool (not i) :: rest
      | _ -> failwith "Panic"
    )
  | Trace -> (
      (* Print the top of the stack *)
      match stack with
      | [] -> []
      | Int i :: _ -> print_endline (string_of_int i); stack
      | Bool b :: _ -> print_endline (string_of_bool b); stack
    )

let interp (s: string): string list option =
  match str_to_prog s with
  | Some p -> Some (List.map (fun c -> match c with Int i -> string_of_int i | Bool b -> string_of_bool b | Unit -> "()") (eval_step [] p))
  | None -> None
