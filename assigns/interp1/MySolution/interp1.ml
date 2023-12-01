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

type const =
   | Int of int
   | Bool of bool
   | Unit

type com =
   | Push of const | Pop | Trace
   | Add | Sub | Mul | Div
   | And | Or | Not | Lt | Gt

type coms = com list

type stack = const list 

type trace = string list 

type prog = coms


(* HELPER FUNCTIONS *)

let rec str_of_nat(n: int): string = 
   let d = n mod 10 in 
   let n0 = n / 10 in
   let s = str (chr(d + ord '0')) in 
   if 0 < n0 then
      string_append(str_of_nat n0) s
   else 
      s

let str_of_int(n: int): string = 
   if n < 0 then
      string_append "-" (str_of_nat (-n))
   else 
      str_of_nat n

let toString(c: const): string =
   match c with 
   | Int i -> str_of_int i
   | Bool true -> "True"
   | Bool false -> "False"
   | Unit -> "Unit"


(* PARSERS *)

let nat_parser = 
   let* n = natural << whitespaces in 
      pure n

let int_parser =
   (let* n = nat_parser in 
      pure (Int n))
   <|>
   (keyword "-" >> let* n = nat_parser in 
      pure (Int (-n)))

let bool_parser =
   (keyword "True" >> pure (Bool true)) 
   <|> 
   (keyword "False" >> pure (Bool false))

let unit_parser =
   keyword "Unit" >> pure Unit

let const_parser =
  int_parser
  <|>
  bool_parser
  <|>
  unit_parser

let com_parser =
   (keyword "Push" >> const_parser >>= fun c ->
      pure (Push c))
   <|>
   (keyword "Pop" >> pure Pop)
   <|>
   (keyword "Trace" >> pure Trace)
   <|>
   (keyword "Add" >> pure Add)
   <|>
   (keyword "Sub" >> pure Sub)
   <|>
   (keyword "Mul" >> pure Mul)
   <|>
   (keyword "Div" >> pure Div)
   <|>
   (keyword "And" >> pure And)
   <|>
   (keyword "Or" >> pure Or)
   <|>
   (keyword "Not" >> pure Not)
   <|>
   (keyword "Lt" >> pure Lt)
   <|>
   (keyword "Gt" >> pure Gt)

let coms_parser = 
   many(com_parser << keyword ";")


(* INTERPRETER FUNCTIONS *)

let rec eval_step(s: stack)(t: trace)(p: prog): trace =
   match p with
   | [] -> t
   | Push c :: p0 -> eval_step (c :: s) t p0
   | Pop :: p0 -> 
      (match s with 
      | _ :: s0 -> eval_step s0 t p0
      | [] -> eval_step [] ("Panic" :: t) [])
   | Trace :: p0 -> 
      (match s with
      | c :: s0 -> eval_step (Unit :: s0) (toString c :: t) p0
      | [] -> eval_step [] ("Panic" :: t) [])
   | Add :: p0 -> 
      (match s with
      | Int i :: Int j :: s0 -> eval_step (Int (i + j) :: s0) t p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) []
      | [] -> eval_step [] ("Panic" :: t) []
      | _ :: [] -> eval_step [] ("Panic" :: t) [])
   | Sub :: p0 -> 
      (match s with
      | Int i :: Int j :: s0 -> eval_step (Int (i - j) :: s0) t p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) []
      | [] -> eval_step [] ("Panic" :: t) []
      | _ :: [] -> eval_step [] ("Panic" :: t) [])
  | Mul :: p0 -> 
      (match s with
      | Int i :: Int j :: s0 -> eval_step (Int (i * j) :: s0) t p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) []
      | [] -> eval_step [] ("Panic" :: t) []
      | _ :: [] -> eval_step [] ("Panic" :: t) [])
  | Div :: p0 -> 
      (match s with
      | Int i :: Int 0 :: s0 -> eval_step [] ("Panic" :: t) []
      | Int i :: Int j :: s0 -> eval_step (Int (i / j) :: s0) t p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) []
      | [] -> eval_step [] ("Panic" :: t) []
      | _ :: [] -> eval_step [] ("Panic" :: t) [])
   | And :: p0 -> 
      (match s with
      | Bool a :: Bool b :: s0 -> eval_step (Bool (a && b) :: s0) t p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) []
      | [] -> eval_step [] ("Panic" :: t) []
      | _ :: [] -> eval_step [] ("Panic" :: t) [])
  | Or :: p0 -> 
      (match s with
      | Bool a :: Bool b :: s0 -> eval_step (Bool (a || b) :: s0) t p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) []
      | [] -> eval_step [] ("Panic" :: t) []
      | _ :: [] -> eval_step [] ("Panic" :: t) [])
   | Not :: p0 -> 
      (match s with
      | Bool a :: s0 -> eval_step (Bool (not a) :: s0) t p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) []
      | [] -> eval_step [] ("Panic" :: t) []
      | _ :: [] -> eval_step [] ("Panic" :: t) [])
   | Lt :: p0 -> 
      (match s with
      | Int i :: Int j :: s0 -> eval_step (Bool (i < j) :: s0) t p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) []
      | [] -> eval_step [] ("Panic" :: t) []
      | _ :: [] -> eval_step [] ("Panic" :: t) [])
  | Gt :: p0 -> 
      (match s with
      | Int i :: Int j :: s0 -> eval_step (Bool (i > j) :: s0) t p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) []
      | [] -> eval_step [] ("Panic" :: t) []
      | _ :: [] -> eval_step [] ("Panic" :: t) [])

let interp(s: string): string list option =
   match string_parse (whitespaces >> coms_parser) (s) with
   | Some (p, []) -> Some (eval_step [] [] p)
   | _ -> None