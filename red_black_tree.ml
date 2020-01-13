(* 赤か黒を示す型 *)
type color_t = Red | Black

type ('a, 'b) rb_tree_t = Empty
                        | Node of ('a, 'b) rb_tree_t * ('a * 'b * color_t) * ('a, 'b) rb_tree_t

(* type ('a, 'b) t = Empty
                | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t *)

(* テスト *)
let test_tree_left_red_left_red =
Node(
  Node (
    Node (
      Empty,
      ("node_x", 50, Red),
      Empty
    ),
    ("node_y", 100, Red),
    Empty
  ),
  ("node_z", 150, Black),
  Empty
)

let test_tree_left_red_right_red =
Node(
  Node (
    Empty,
    ("node_x", 50, Red),
    Node (
      Empty,
      ("node_y", 100, Red),
      Empty
    )
  ),
  ("node_z", 150, Black),
  Empty
)

let test_tree_right_red_left_red =
Node (
  Empty,
  ("node_x", 50, Black),
  Node (
    Node (
      Empty,
      ("node_y", 100, Red),
      Empty
    ),
    ("node_z", 150, Red),
    Empty
  )
)

let test_tree_right_red_right_red =
Node(
  Empty,
  ("node_x", 50, Black),
  Node (
    Empty,
    ("node_y", 100, Red),
    Node (
      Empty,
      ("node_z", 150, Red),
      Empty
    )
  )
)

let correct_tree = 
Node(
  Node (
    Empty,
    ("node_x", 50, Black),
    Empty
  ),
  ("node_y", 100, Red),
  Node (
    Empty,
    ("node_z", 150, Black),
    Empty
  )
)

(* 目的: 受け取った木が赤黒木の定義を満たしていないようであれば、満たすように処理を実施する。満たしていればそのまま返す *)
(* balance rb_tree_t : rb_tree_t *)
let balance tree = match tree with
    Empty -> Empty
  | Node (Node (Node (node_a, (node_x_key, node_x_val, Red), node_b), (node_y_key, node_y_val, Red), node_c), (node_z_key, node_z_val, Black), node_d)
  | Node (Node (node_a, (node_x_key, node_x_val, Red), Node (node_b, (node_y_key, node_y_val, Red), node_c)), (node_z_key, node_z_val, Black), node_d)
  | Node (node_a, (node_x_key, node_x_val, Black), Node (Node ( node_b, (node_y_key, node_y_val, Red), node_c), (node_z_key, node_z_val, Red), node_d))
  | Node (node_a, (node_x_key, node_x_val, Black), Node (node_b, (node_y_key, node_y_val, Red), Node (node_c, (node_z_key, node_z_val, Red), node_d))) ->
    Node (
      Node (
        node_a,
        (node_x_key, node_x_val, Black),
        node_b
      ),
      (node_y_key, node_y_val, Red),
      Node (
        node_c,
        (node_z_key, node_z_val, Black),
        node_d
      )
    )
  | _ -> tree
  (* | (t1, n, t2) -> "Empty-2" *)

let test1 = balance test_tree_left_red_left_red
= correct_tree
let test2 = balance test_tree_left_red_right_red
= correct_tree
let test3 = balance test_tree_right_red_left_red
= correct_tree
let test4 = balance test_tree_right_red_right_red
= correct_tree

(* 目的: 赤黒木とキーと値を受け取ったら、それを挿入した赤黒木を返す関数。キーが存在したらうわがく *)
(* insert : rb_tree_t -> rb_tree_t *)
let rec insert tree key value = match tree with
    Empty -> Node (Empty, (key, value, Red), Empty)
  | Node (t1, (n_key, n_value, n_color), t2) ->
    let new_tree = if key = n_key then Node (t1, (n_key, value, n_color), t2)
    else if value < n_value then Node (insert t1  key value, (n_key, n_value, n_color), t2)
    else Node (t1, (n_key, n_value, n_color), insert t2  key value) in
      if n_color = Black then balance new_tree
      else new_tree

let test21 = insert test_tree_left_red_left_red "fourth" 25
= Node (
  Node (
    Node (
      Empty,
      ("fourth", 25, Red),
      Empty
    ),
    ("node_x", 50, Black), Empty),
  ("node_y", 100, Red),
  Node (
    Empty,
    ("node_z", 150, Black),
    Empty
  )
)

let test22 = insert test_tree_left_red_left_red "fourth" 200
= Node (
  Node (
    Empty,
    ("node_x", 50, Black),
    Empty
  ),
  ("node_y", 100, Red),
  Node (
    Empty,
    ("node_z", 150,
    Black
  ),
  Node (
    Empty,
    ("fourth", 200, Red),
    Empty)
  )
)

(* 空の赤黒木 *)
let empty = Empty

(* search : rb_tree_t key *)
(* 見つからなかったら例外 Not_found を起こす *)
let rec search tree key = match tree with
    Empty -> raise Not_found
  | Node (t1, (n_key, n_value, _), t2) ->
    if key = n_key then n_value
    else if key < n_key then search t1 key
    else search t2 key

let test31 = search test_tree_left_red_left_red "node_x"
