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

let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream =
    let rec dfs_rec(nodes) =
        match nodes with
        | [] -> StrNil
        | (Node (data, children)) :: rest ->
            let next_children = fun () -> 
                dfs_rec (children @ rest) in
            StrCons (data, next_children)
        | Leaf :: rest -> dfs_rec rest
    in
        fun () -> dfs_rec [xs]



(*
let rec gtree_bfs(nxs: node list)(fchildren : node -> node list): node stream = fun () -> 
	(match nxs with
	| [] -> StrNil
	| nx1 :: nxs -> StrCons(nx1, gtree_bfs(nxs @ children(nx1))(fchildren)))
*)

(* 
breadth-first search
level-order
*)

let rec gtree_streamize_bfs(xs: 'a gtree): 'a stream =
    let rec bfs_queue queue =
        match queue with
        | [] -> Nil
        | Node (data, children) :: rest ->
            StrCons (data, bfs_queue (rest @ children))
    in
    match xs with
    | Node (root, children) ->
    StrCons (root, bfs_queue children)



(* ****** ****** *)