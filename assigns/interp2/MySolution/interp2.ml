(* ****** ****** *)

#use "./../../../classlib/OCaml/MyOCaml.ml";;


(* GRAMMAR *)

(* 
grammar: constants 

⟨digit⟩ ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
⟨nat⟩ ::= ⟨digit⟩ | ⟨digit⟩⟨nat⟩
⟨int⟩ ::= ⟨nat⟩ | -⟨nat⟩
⟨bool⟩ ::= True | False
⟨char⟩ ::= a | b | ... | z
⟨sym⟩ ::= ⟨char⟩ | ⟨sym⟩⟨char⟩ | ⟨sym⟩⟨digit⟩
⟨const⟩ ::= ⟨int⟩ | ⟨bool⟩ | Unit | ⟨sym⟩
*)

(* 
grammar: programs 

⟨prog⟩ ::= ⟨coms⟩
⟨com⟩ ::= Push ⟨const⟩ | Pop | Swap | Trace
          | Add | Sub | Mul | Div
          | And | Or | Not
          | Lt | Gt
          | If ⟨coms⟩ Else ⟨coms⟩ End
          | Bind | Lookup
          | Fun ⟨coms⟩ End | Call | Return
⟨coms⟩ ::= ϵ | ⟨com⟩; ⟨coms⟩
*)


(* TYPE DEFINITIONS *)

type digit = 
   | 0 
   | 1 
   | 2 
   | 3 
   | 4 
   | 5 
   | 6 
   | 7 
   | 8 
   | 9

type sym = 
   | Char of char 
   | SChar of sym * char 
   | SDig of sym * digit

type const =
   | Int of int
   | Bool of bool
   | Unit
   | Sym of sym

type com =
   | Push of const | Pop | Trace
   | Add | Sub | Mul | Div
   | And | Or | Not 
   | Lt | Gt
   | If of coms * coms 
   | Bind | Lookup 
   | Fun of coms | Call | Return

type coms = com list

type stack = const list 

type trace = string list 

type venv = (string * const) list

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

let rec eval_step(s: stack)(t: trace)(v: venv)(p: prog): trace =
   match p with
   | [] -> t
   | Push c :: p0 -> eval_step (c :: s) t v p0
   | Pop :: p0 -> 
      (match s with 
      | _ :: s0 -> eval_step s0 t v p0
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Swap :: p0 -> 
      (match s with 
      | c1 :: c2 :: s0 -> eval_step (c2 :: c1 :: s0) t v p0
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | Trace :: p0 -> 
      (match s with
      | c :: s0 -> eval_step (Unit :: s0) (toString c :: t) v p0
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Add :: p0 -> 
      (match s with
      | Int i :: Int j :: s0 -> eval_step (Int (i + j) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | Sub :: p0 -> 
      (match s with
      | Int i :: Int j :: s0 -> eval_step (Int (i - j) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
  | Mul :: p0 -> 
      (match s with
      | Int i :: Int j :: s0 -> eval_step (Int (i * j) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
  | Div :: p0 -> 
      (match s with
      | Int i :: Int 0 :: s0 -> eval_step [] ("Panic" :: t) v []
      | Int i :: Int j :: s0 -> eval_step (Int (i / j) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | And :: p0 -> 
      (match s with
      | Bool a :: Bool b :: s0 -> eval_step (Bool (a && b) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
  | Or :: p0 -> 
      (match s with
      | Bool a :: Bool b :: s0 -> eval_step (Bool (a || b) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | Not :: p0 -> 
      (match s with
      | Bool a :: s0 -> eval_step (Bool (not a) :: s0) t v p0
      | _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | Lt :: p0 -> 
      (match s with
      | Int i :: Int j :: s0 -> eval_step (Bool (i < j) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
  | Gt :: p0 -> 
      (match s with
      | Int i :: Int j :: s0 -> eval_step (Bool (i > j) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | If :: c1 :: Else :: c2 :: End :: p0 -> 
      (match s with 
      | Bool b :: s0 -> 
         if b = true then 
            eval_step (c1 :: s0) t v p0
         else 
            eval_step (c2 :: s0) t v p0
      | _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Bind :: p0 -> 
      (match s with 
      | Sym x :: v0 :: s0 -> 
         let x = v0 in 
         eval_step s0 t (x :: v) p0 
      | _ :: v0 :: s0 -> eval_step [] ("Panic" :: t) v []
      | _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Lookup :: p0 ->
      (match s with 
      | Sym x :: s0 -> 
         (match v with 
         | x :: v0 -> eval_step (x :: s0) t v p0
         | _ :: v0 -> eval_step [] ("Panic" :: t) v [])
      | _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Fun :: c :: End :: p0 ->
      (match s with 
      | Sym x :: s0 -> 
         let 
      create closure val on s with name x
      | _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Call :: p0 -> 
   | Return :: p0 ->
   







let interp (s : string) : string list option = (* YOUR CODE *)