(*
//
Assign4-3:
//
HX-2023-10-05: 10 points
//
Please enumerate a gtree in the manner of
depth-first search:
//
let rec (* 5 points *)
gtree_streamize_dfs(xs: 'a gtree): 'a stream
//
Please enumerate a gtree in the manner of
breadth-first search:
//
let rec (* 5 points *)
gtree_streamize_bfs(xs: 'a gtree): 'a stream
//
*)


(* ****** ****** *)


#use "./../../../../classlib/OCaml/MyOcaml.ml"
;;

(*
depth-first search
*)

let rec gtree_streamize_dfs(xs: 'a gtree): 'a stream =


let rec gtree_bfs(nxs: node list)(fchildren : node -> node list): node stream = fun () -> 
	(match nxs with
	| [] -> StrNil
	| nx1 :: nxs -> StrCons(nx1, gtree_bfs(nxs @ children(nx1))(fchildren)))


(* 
breadth-first search
level-order
*)

let rec gtree_streamize_bfs(xs: 'a gtree): 'a stream =
    match xs with 
    | 
    | 
    StrCons()



(* ****** ****** *)