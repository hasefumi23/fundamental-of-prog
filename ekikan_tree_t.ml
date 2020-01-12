type tmp_ekikan_tree_t = {
  namae : string; (* 駅名(漢字) *)
  eki_list : (string * float) list; (* 「上記の駅に直接つながっている駅名(漢字)とその駅までの距離の組」のリスト *)
}

type ekikan_tree_t = Empty
                   | Node of ekikan_tree_t * (string * (string * float) list) * ekikan_tree_t

(* テスト *)
let test1 = Empty
let test2 = Node (Empty, ("a", [("b", 10.)]), Empty)

(* 「駅名」と「駅名と距離の組のリスト」を受け取ったら、その駅までの距離を返す *)
(* assoc : string (string * int) list *)
let rec assoc ekimei list = match list with
    [] -> infinity
  | (eki, kyori) :: rest -> if ekimei = eki then kyori else assoc ekimei rest

(* テスト *)
let test1 = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8
let test2 = assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] = infinity

type ekikan_t = {
  kiten  : string; (* 起点 *)
  shuten : string; (* 終点 *)
  (* keiyu  : string; 経由線名 *)
  kyori  : float;  (* 距離 *)
  (* jikan  : int;    時間 *)
}

let test_tree1
= Node (
  Node (
    Empty,
    ("新大塚", [("茗荷谷", 1.2)]),
    Empty
  ),
  ("茗荷谷", [("新大塚", 1.2)]),
  Empty
)

(* ekikan_tree_t 型の木と ekikan_t 型の駅間を受け取ったら、その情報を挿入した木を返す *)
(* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let rec ins_ekikan tree comp_ekimei into_ekimei kyori = match tree with
    Empty -> Node (Empty, (comp_ekimei, [(into_ekimei, kyori)]), Empty)
  | Node (t1, eki_k, t2) -> match eki_k with
    (name, ekikan_list) ->
      if comp_ekimei = name then Node (t1, (name, (into_ekimei, kyori) :: ekikan_list), t2)
      else if comp_ekimei < name then Node ((ins_ekikan t1 comp_ekimei into_ekimei kyori), eki_k, t2)
      else Node (t1, eki_k, (ins_ekikan t2 comp_ekimei into_ekimei kyori))

let test10 = ins_ekikan Empty "新大塚" "茗荷谷" 1.2
= Node (
  Empty,
  ("新大塚", [("茗荷谷", 1.2)]),
  Empty
)
let test11 = ins_ekikan test_tree1 "新大塚" "渋谷" 1.3
= Node (
  Node (
    Empty,
    ("新大塚", [("渋谷", 1.3); ("茗荷谷", 1.2)]),
    Empty
  ),
  ("茗荷谷", [("新大塚", 1.2)]),
  Empty
)
let test12 = ins_ekikan test_tree1 "霞が関" "渋谷" 2.3
= Node (
  Node (
    Empty,
    ("新大塚", [("茗荷谷", 1.2)]),
    Empty
  ),
  ("茗荷谷", [("新大塚", 1.2)]),
  Node (
    Empty,
    ("霞が関", [("渋谷", 2.3)]),
    Empty
  )
)

let insert_ekikan ekikan tree = match ekikan with
  {kiten = k; shuten = s; kyori = kyo} ->
    if k > s then let first_tree = ins_ekikan tree k s kyo in
      ins_ekikan first_tree s k kyo
    else let first_tree = ins_ekikan tree s k kyo in
      ins_ekikan first_tree k s kyo

(* テスト *)
let test20 = insert_ekikan {kiten = "新大塚"; shuten = "茗荷谷"; kyori = 1.2} Empty
= Node (
  Node (
    Empty,
    ("新大塚", [("茗荷谷", 1.2)]),
    Empty
  ),
  ("茗荷谷", [("新大塚", 1.2)]),
  Empty
)

(* 目的: ekikan_tree_t 型の木と ekikan_t list 型の駅間のリストを受け取ったら、リストの中に含まれる駅間を全て挿入した木を返す *)
(* inserts_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
(* let rec inserts_ekikan ekikan_list tree = match ekikan_list with
    [] -> tree
  | first :: rest -> let first_tree = insert_ekikan first tree in
    inserts_ekikan rest first_tree *)

let inserts_ekikan tree ekikan_list = List.fold_right insert_ekikan ekikan_list tree

(* テスト *)
let test30 = inserts_ekikan Empty [
  {kiten = "新大塚"; shuten = "茗荷谷"; kyori = 1.2};
  {kiten = "新大塚"; shuten = "渋谷"; kyori = 2.2}
]
= Node (
  Node (
    Empty,
    ("新大塚", [("茗荷谷", 1.2); ("渋谷", 2.2)]),
    Empty
  ),
  ("渋谷", [("新大塚", 2.2)]),
  Node (
    Empty,
    ("茗荷谷", [("新大塚", 1.2)]),
    Empty
  )
)
