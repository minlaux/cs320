#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* No other library imports are allowed. *)

(* ************************************************ *)

(* Question 7: 10 points

   Given the following snippet, implement the test
   function so that isPrime returns true for prime
   number inputs and false otherwise. *)

let isPrime(n) =
    let rec test(i:int): bool =
        if i <= 1 then 
            true 
        else if n mod i = 0 then 
            false
        else test (i - 1)
  
   (* YOUR CODE *)
  in
  if n < 2 then false else int1_forall(n)(test)

(* ************************************************ *)
