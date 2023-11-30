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



(* Depth-first search *)
let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream =
  let rec dfs_rec nodes =
    match nodes with
    | [] -> StrNil
    | GTnil :: rest -> dfs_rec rest
    | GTcons (data, children) :: rest ->
      let next_children = fun () -> dfs_rec (children @ rest) in
      StrCons (data, next_children)
  in
  fun () -> dfs_rec [xs]


(* Breadth-first search (level-order) *)
let rec gtree_streamize_bfs (xs: 'a gtree): 'a stream =
  let rec bfs_queue queue =
    match queue with
    | [] -> StrNil
    | GTnil :: rest -> bfs_queue rest
    | GTcons (data, children) :: rest ->
      let next_nodes = List.map (fun x -> x) children in
      StrCons(data, fun () -> bfs_queue (rest @ next_nodes))
  in
  fun () -> bfs_queue [xs]

;;

(* ****** ****** *)