type tree_t = Empty
            | Leaf of int
            | Node of tree_t * int * tree_t

let tree1 = Empty
let tree2 = Leaf (3)
let tree3 = Node (tree1, 4, tree2)
let tree4 = Node (tree2, 5, tree3)
let tree5 = Node (Node (Node (Node (Node (Empty, 0, Empty), 0, Empty), 0, Empty), 0, Empty), 0, Empty)

(* 目的: treeに含まれる整数を全て加える *)
let rec sum_tree tree = match tree with
    Empty -> 0
  | Leaf (n) -> n
  | Node (t1, n, t2) -> sum_tree t1 + n + sum_tree t2

(* テスト *)
let test1 = sum_tree tree1 = 0
let test2 = sum_tree tree2 = 3
let test3 = sum_tree tree3 = 7
let test4 = sum_tree tree4 = 15

let rec tree_double tree = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf (n * 2)
  | Node (t1, n, t2) -> Node ((tree_double t1), n * 2, (tree_double t2))

let test1 = tree_double tree4

(* tree_map : (f tree_t -> tree_t) -> tree_t -> tree_t *)

let rec tree_map tree f = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf (f n)
  | Node (t1, n, t2) -> Node ((tree_map t1 f), f n, (tree_map t2 f))

let test1 = tree_map tree4 (fun n -> n * 10)

let rec tree_length tree = match tree with
    Empty -> 0
  | Leaf (n) -> 1
  | Node (t1, n, t2) -> 1 + tree_length t1 + tree_length t2

let test1 = tree_length tree4

let tree_depth tree =
  let rec dep dep_tree rest_result = match dep_tree with
      Empty -> 1 + rest_result
    | Leaf (_) -> 1 + rest_result
    | Node (t1, _, t2) -> 1 + max (dep t1 rest_result) (dep t2 rest_result) in
      dep tree 0

let test1 = tree_depth tree5
