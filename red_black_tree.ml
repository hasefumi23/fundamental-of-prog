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
      ("first", 1, Red),
      Empty
    ),
    ("first", 1, Red),
    Empty
  ),
  ("first", 0, Black),
  Empty
)

let test_tree_left_red_right_red =
Node(
  Node (
    Empty,
    ("first", 1, Red),
    Node (
      Empty,
      ("first", 1, Red),
      Empty
    )
  ),
  ("first", 0, Black),
  Empty
)

let test_tree_right_red_left_red =
Node(
  Empty,
  ("first", 0, Black),
  Node (
    Node (
      Empty,
      ("first", 1, Red),
      Empty
    ),
    ("first", 1, Red),
    Empty
  )
)

let test_tree_right_red_right_red =
Node(
  Empty,
  ("first", 0, Black),
  Node (
    Empty,
    ("first", 0, Red),
    Node (
      Empty,
      ("first", 0, Red),
      Empty
    )
  )
)

let correct_tree = 
Node(
  Node (
    Empty,
    ("first", 0, Black),
    Empty
  ),
  ("first", 0, Red),
  Node (
    Empty,
    ("first", 0, Black),
    Empty
  )
)

(* 目的: 受け取った木が赤黒木の定義を満たしていないようであれば、満たすように処理を実施する。満たしていればそのまま返す *)
(* balance rb_tree_t : rb_tree_t *)
let balance tree = match tree with
    Empty -> Empty
  (* | Node (Node (Node ( _, (_, _, Red), _), (_, _, Red), _), (_, _, Black), _)
  | Node (Node (_, (_, _, Red), Node ( _, (_, _, Red), _)), (_, _, Black), _)
  | Node (_, (_, _, Black), Node (Node ( _, (_, _, Red), _), (_, _, Red), _)) *)
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
  | _ -> Empty
  (* | (t1, n, t2) -> "Empty-2" *)

let test1 = balance test_tree_left_red_left_red
let test2 = balance test_tree_left_red_right_red
let test3 = balance test_tree_right_red_left_red
let test4 = balance test_tree_right_red_right_red
= correct_tree
