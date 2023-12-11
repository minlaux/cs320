#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the [compile] function following the
specifications described in CS320_Fall_2023_Project-3.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* ------------------------------------------------------------ *)

(* abstract syntax tree of high-level language *)

type uopr =
  | Neg | Not

type bopr =
  | Add | Sub | Mul | Div | Mod
  | And | Or
  | Lt  | Gt  | Lte | Gte | Eq

type expr =
  | Int of int
  | Bool of bool
  | Unit
  | UOpr of uopr * expr
  | BOpr of bopr * expr * expr
  | Var of string
  | Fun of string * string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Seq of expr * expr
  | Ifte of expr * expr * expr
  | Trace of expr

(* ------------------------------------------------------------ *)

(* combinator for left-associative operators *)

let chain_left (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* init = p in
  let* fms = many (let* f = q in let* m = p in pure (f, m)) in
  let m = list_foldleft fms init (fun acc (f, m) -> f acc m) in
  pure m

let rec chain_right (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* m = p in
  (let* f = q in
   let* rest = chain_right p q in
   pure (f m rest)) <|> 
  (pure m)

let opt (p : 'a parser) : 'a option parser =
  (let* x = p in pure (Some x)) <|> pure None

(* basic constants *)

let parse_int : expr parser =
  let* n = natural in
  pure (Int n) << whitespaces

let parse_bool : expr parser =
  (keyword "true" >> pure (Bool true)) <|>
  (keyword "false" >> pure (Bool false))

let parse_unit : expr parser =
  keyword "(" >> keyword ")" >> pure Unit

(* names *)

let isReserved s =
  let reserved = 
    ["let"; "rec"; "in"; "fun"; "if"; "then"; "else"; "trace"; "mod"; "not"] 
  in
  list_exists reserved (fun s0 -> s0 = s)

let parse_name : string parser =
  let lower = satisfy char_islower in
  let upper = satisfy char_isupper in
  let digit = satisfy char_isdigit in
  let quote = char '\'' in
  let wildc = char '_' in
  let* c = lower <|> wildc in
  let* cs = many (lower <|> upper <|> digit <|> wildc <|> quote) in
  let s = string_make_fwork (list_foreach (c :: cs)) in
  if isReserved s then fail
  else pure s << whitespaces

(* unary operators *)

let parse_neg : (expr -> expr) parser =
  keyword "-" >> pure (fun m -> UOpr (Neg, m))

(* binary operators *)

let parse_add : (expr -> expr -> expr) parser =
  keyword "+" >> pure (fun m n -> BOpr (Add, m, n))

let parse_sub : (expr -> expr -> expr) parser =
  keyword "-" >> pure (fun m n -> BOpr (Sub, m, n))

let parse_mul : (expr -> expr -> expr) parser =
  keyword "*" >> pure (fun m n -> BOpr (Mul, m, n))

let parse_div : (expr -> expr -> expr) parser =
  keyword "/" >> pure (fun m n -> BOpr (Div, m, n))

let parse_mod : (expr -> expr -> expr) parser =
  keyword "mod" >> pure (fun m n -> BOpr (Mod, m, n))

let parse_and : (expr -> expr -> expr) parser =
  keyword "&&" >> pure (fun m n -> BOpr (And, m, n))

let parse_or : (expr -> expr -> expr) parser =
  keyword "||" >> pure (fun m n -> BOpr (Or, m, n))

let parse_lt : (expr -> expr -> expr) parser =
  keyword "<" >> pure (fun m n -> BOpr (Lt, m, n))

let parse_gt : (expr -> expr -> expr) parser =
  keyword ">" >> pure (fun m n -> BOpr (Gt, m, n))

let parse_lte : (expr -> expr -> expr) parser =
  keyword "<=" >> pure (fun m n -> BOpr (Lte, m, n))

let parse_gte : (expr -> expr -> expr) parser =
  keyword ">=" >> pure (fun m n -> BOpr (Gte, m, n))

let parse_eq : (expr -> expr -> expr) parser =
  keyword "=" >> pure (fun m n -> BOpr (Eq, m, n))

let parse_neq : (expr -> expr -> expr) parser =
  keyword "<>" >> pure (fun m n -> UOpr (Not, BOpr (Eq, m, n)))

let parse_seq : (expr -> expr -> expr) parser =
  keyword ";" >> pure (fun m n -> Seq (m, n))

(* expression parsing *)

let rec parse_expr () = 
  let* _ = pure () in
  parse_expr9 ()

and parse_expr1 () : expr parser = 
  let* _ = pure () in
  parse_int <|> 
  parse_bool <|> 
  parse_unit <|>
  parse_var () <|>
  parse_fun () <|>
  parse_letrec () <|>
  parse_let () <|>
  parse_ifte () <|>
  parse_trace () <|>
  parse_not () <|>
  (keyword "(" >> parse_expr () << keyword ")")

and parse_expr2 () : expr parser =
  let* m = parse_expr1 () in
  let* ms = many' parse_expr1 in
  let m = list_foldleft ms m (fun acc m -> App (acc, m)) in
  pure m

and parse_expr3 () : expr parser =
  let* f_opt = opt parse_neg in
  let* m = parse_expr2 () in
  match f_opt with
  | Some f -> pure (f m)
  | None -> pure m

and parse_expr4 () : expr parser =
  let opr = parse_mul <|> parse_div <|> parse_mod in
  chain_left (parse_expr3 ()) opr

and parse_expr5 () : expr parser =
  let opr = parse_add <|> parse_sub in
  chain_left (parse_expr4 ()) opr

and parse_expr6 () : expr parser =
  let opr = 
    parse_lte <|> 
    parse_gte <|>
    parse_neq <|>
    parse_lt <|> 
    parse_gt <|>
    parse_eq
  in
  chain_left (parse_expr5 ()) opr

and parse_expr7 () : expr parser =
  chain_left (parse_expr6 ()) parse_and

and parse_expr8 () : expr parser =
  chain_left (parse_expr7 ()) parse_or

and parse_expr9 () : expr parser =
  chain_right (parse_expr8 ()) parse_seq

and parse_var () : expr parser =
  let* x = parse_name in
  pure (Var x)

and parse_fun () : expr parser =
  let* _ = keyword "fun" in
  let* xs = many1 parse_name in 
  let* _ = keyword "->" in
  let* body = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure m

and parse_let () : expr parser =
  let* _ = keyword "let" in
  let* x = parse_name in
  let* xs = many parse_name in
  let* _ = keyword "=" in
  let* body = parse_expr () in
  let* _ = keyword "in" in
  let* n = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure (Let (x, m, n))

and parse_letrec () : expr parser =
  let* _ = keyword "let" in
  let* _ = keyword "rec" in
  let* f = parse_name in
  let* x = parse_name in
  let* xs = many parse_name in
  let* _ = keyword "=" in
  let* body = parse_expr () in
  let* _ = keyword "in" in
  let* n = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure (Let (f, Fun (f, x, m), n))

and parse_ifte () : expr parser =
  let* _ = keyword "if" in
  let* m = parse_expr () in
  let* _ = keyword "then" in
  let* n1 = parse_expr () in
  let* _ = keyword "else" in
  let* n2 = parse_expr () in
  pure (Ifte (m, n1, n2))

and parse_trace () : expr parser =
  let* _ = keyword "trace" in
  let* m = parse_expr1 () in
  pure (Trace m) 

and parse_not () : expr parser =
  let* _ = keyword "not" in
  let* m = parse_expr1 () in
  pure (UOpr (Not, m))

exception SyntaxError
exception UnboundVariable of string

type scope = (string * string) list

let new_var =
  let stamp = ref 0 in
  fun x ->
    incr stamp;
    let xvar = string_filter x (fun c -> c <> '_' && c <> '\'') in
    string_concat_list ["v"; xvar; "i"; string_of_int !stamp]

let find_var scope s =
  let rec loop scope =
    match scope with
    | [] -> None
    | (s0, x) :: scope ->
      if s = s0 then Some x
      else loop scope
  in loop scope

let scope_expr (m : expr) : expr = 
  let rec aux scope m =
    match m with
    | Int i -> Int i
    | Bool b -> Bool b
    | Unit -> Unit
    | UOpr (opr, m) -> UOpr (opr, aux scope m)
    | BOpr (opr, m, n) -> 
      let m = aux scope m in
      let n = aux scope n in
      BOpr (opr, m, n)
    | Var s -> 
      (match find_var scope s with
       | None -> raise (UnboundVariable s)
       | Some x -> Var x)
    | Fun (f, x, m) -> 
      let fvar = new_var f in
      let xvar = new_var x in
      let m = aux ((f, fvar) :: (x, xvar) :: scope) m in
      Fun (fvar, xvar, m)
    | App (m, n) ->
      let m = aux scope m in
      let n = aux scope n in
      App (m, n)
    | Let (x, m, n) ->
      let xvar = new_var x in
      let m = aux scope m in
      let n = aux ((x, xvar) :: scope) n in
      Let (xvar, m, n)
    | Seq (m, n) ->
      let m = aux scope m in
      let n = aux scope n in
      Seq (m, n)
    | Ifte (m, n1, n2) ->
      let m = aux scope m in
      let n1 = aux scope n1 in
      let n2 = aux scope n2 in
      Ifte (m, n1, n2)
    | Trace m -> Trace (aux scope m)
  in
  aux [] m

(* ------------------------------------------------------------ *)

(* parser for the high-level language *)

let parse_prog (s : string) : expr =
  match string_parse (whitespaces >> parse_expr ()) s with
  | Some (m, []) -> scope_expr m
  | _ -> raise SyntaxError


(******************************)


(* GRAMMAR *)

(* 
⟨digit⟩ ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
⟨nat⟩ ::= ⟨digit⟩ | ⟨digit⟩⟨nat⟩
⟨int⟩ ::= ⟨nat⟩ | -⟨nat⟩
⟨bool⟩ ::= true | false
⟨unit⟩ ::= ()
⟨unop⟩ ::= - | not
⟨binop⟩ ::= + | - | * | / | mod | && | || | < | > | <= | >= | =
⟨char⟩ ::= a | b | .. | z
⟨var⟩ ::= ⟨char⟩ | ⟨var⟩⟨char⟩ | ⟨var⟩⟨digit⟩
⟨expr⟩ ::= ⟨int⟩ | ⟨bool⟩ | ⟨unit⟩
        | ⟨unop⟩ ⟨expr⟩
        | ⟨expr⟩ ⟨binop⟩ ⟨expr⟩
        | ⟨var⟩
        | fun ⟨var⟩ ⟨var⟩ -> ⟨expr⟩
        | ⟨expr⟩ ⟨expr⟩
        | let ⟨var⟩ = ⟨expr⟩ in ⟨expr⟩
        | ⟨expr⟩ ; ⟨expr⟩
        | if ⟨expr⟩ then ⟨expr⟩ else ⟨expr⟩
        | trace ⟨expr⟩
*)


let rec str_of_nat (n: int): string =
  let d = n mod 10 in 
  let n0 = n / 10 in
  let s = str (chr (d + ord '0')) in 
  if 0 < n0 then
    string_append (str_of_nat n0) s
  else s

let str_of_int (n: int): string = 
  if n < 0 then
    string_append "-" (str_of_nat (-n))
  else str_of_nat n

let str_of_bool (b: bool): string = 
  if b = true then 
    "True"
  else 
    "False"
(*
let toString (e: expr): string = 
  match e with 
  | Int i -> str_of_int i
  | Bool b -> str_of_bool b
  | Unit -> "Unit"
  | Fun (s, e1, e2) -> "Fun<" ^ s ^ ">"
*)
(******************************)


let rec translate (s: scope) (e: expr): string =
  match e with
  | Int i -> string_concat_list ["Push (Int "; str_of_int i; "); "]
  | Bool b -> string_concat_list ["Push (Bool "; str_of_bool b; "); "]
  | Unit -> "Push Unit; "

  | UOpr (Neg, e1) -> string_append (translate s e1) "Push (Int -1); Mul; "
  | UOpr (Not, e1) -> string_append (translate s e1) "Not; "

  | BOpr (Add, e1, e2) -> string_concat_list [translate s e1; translate s e2; "Add; "]
  | BOpr (Add, _, _) -> "Error"

  | BOpr (Sub, e1, e2) -> string_concat_list [translate s e1; translate s e2; "Swap; Sub; "]
  | BOpr (Sub, _, _) -> "Error"
  
  | BOpr (Mul, e1, e2) -> string_concat_list [translate s e1; translate s e2; "Mul; "]  
  | BOpr (Mul, _, _) -> "Error"
  
  | BOpr (Div, Int i1, Int 0) -> "Error"
  | BOpr (Div, e1, e2) -> string_concat_list [translate s e1; translate s e2; "Swap; Div; "]
  | BOpr (Div, _, _) -> "Error"
  
  | BOpr (Mod, e1, Int 0) -> "Error"
  | BOpr (Mod, e1, e2) -> 
  string_concat_list [translate s e1; translate s e2; "Swap; Div; "; translate s e2; "Mul; "; translate s e1; "Sub; "]
  | BOpr (Mod, _, _) -> "Error"

  | BOpr (And, e1, e2) -> string_concat_list [translate s e1; translate s e2; "And; "]
  | BOpr (And, _, _) -> "Error"

  | BOpr (Or, e1, e2) -> string_concat_list [translate s e1; translate s e2; "Or; "]
  | BOpr (Or, _, _) -> "Error"
 
  | BOpr (Lt, e1, e2) -> string_concat_list [translate s e1; translate s e2; "Swap; Lt; "]
  | BOpr (Lt, _, _) -> "Error"

  | BOpr (Gt, e1, e2) -> string_concat_list [translate s e1; translate s e2; "Swap; Gt; "]
  | BOpr (Gt, _, _) -> "Error"

  | BOpr (Lte, e1, e2) -> 
  string_concat_list [translate s e1; translate s e2; "Swap; Lt; "; translate s e1; translate s e2; "Swap; Gt; Not; And; "] 
  | BOpr (Lte, _, _) -> "Error"
  
  | BOpr (Gte, e1, e2) -> 
  string_concat_list [translate s e1; translate s e2; "Swap; Gt; "; translate s e1; translate s e2; "Swap; Lt; Not; And; "]  
  | BOpr (Gte, _, _) -> "Error"
  
  | BOpr (Eq, e1, e2) -> 
  string_concat_list [translate s e1; translate s e2; "Swap; Lt; Not; "; translate s e1; translate s e2; "Gt; Not; And; "]
  | BOpr (Eq, _, _) -> "Error"

  | Var s1 -> 
    (match find_var s s1 with 
    | Some v -> string_concat_list ["Push "; v; "; Lookup; "]
    | None -> string_concat_list ["Push "; new_var s1; "; Lookup; "])
  
  | Fun (f, x, m) -> 
    let f_var = new_var f in 
    let f_scope = (f, f_var) :: s in 
    let param = new_var x in 
    let def = (x, param) :: f_scope in
    string_concat_list ["Push "; f_var; "; Fun Push "; param; "; Bind; "; translate def m; "End; "]

  | Let (x, v, m) -> 
    let var_x = new_var x in 
    let x_scope = (x, var_x) :: s in 
    string_concat_list [translate s v; "Push "; var_x; "; Bind; "; translate x_scope m]
  | Let (x, _, _) -> "Error"

  | App (f1, v1) -> string_concat_list [translate s f1; translate s v1; " Bind; "; translate s v1; "Lookup; Call; "]
  | App (_, _) -> "Error"

  | Seq (e1, e2) -> string_concat_list [translate s e1; translate s e2]
  | Seq (_, _) -> "Error"

  | Ifte (e1, n1, n2) -> 
  string_concat_list ["If "; translate s e1; "Then "; translate s n1; "Else "; translate s n2; "End; "]
  | Ifte (_, n1, n2) -> "Error"

  | Trace e1 -> string_concat_list [translate s e1; "Trace; "]
  | Trace _ -> "Error"


let compile(s: string): string = 
  translate [] (scope_expr (parse_prog s))



(* READ FILE FUNCTIONS *)

let read_file (fname: string): string =
   let fp = open_in fname in
   let s = string_make_fwork (fun work ->
      try
         while true do
            work (input_char fp)
         done
      with _ -> ())
   in
   close_in fp; s

let comp_file (fname: string) =
   let src = read_file fname in
   compile src