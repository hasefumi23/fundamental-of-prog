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
    ("first", 1, Red),
    Node (
      Empty,
      ("first", 1, Red),
      Empty
    )
  )
)

let correct_tree = 
Node(
  Node (
    Empty,
    ("first", 1, Red),
    Empty
  ),
  ("first", 0, Black),
  Node (
    Empty,
    ("first", 1, Red),
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
  | Node (_, (_, _, Black), Node (_, (_, _, Red), Node ( _, (_, _, Red), _))) -> Empty
  | _ -> Empty
  (* | (t1, n, t2) -> "Empty-2" *)

let test1 = balance test_tree_left_red_left_red
let test2 = balance test_tree_left_red_right_red
let test3 = balance test_tree_right_red_left_red
let test4 = balance test_tree_right_red_right_red = correct_tree
