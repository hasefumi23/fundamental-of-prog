(* 目的: 2つのリストを受け取り長さが同じであることを判定する *)
(* merge : int list -> int list -> boolean *)
let rec equal_length lst1 lst2 = match (lst1, lst2) with
    ([], []) -> true
  | ([], first2 :: rest2) -> false (* merge lst1 rest2 *)
  | (first1 :: rest1, []) -> false (* merge rest1 lst2 *)
  | (first1 :: rest1, first2 :: rest2) -> equal_length rest1 rest2

(* テスト *)
let test1 = equal_length [] [] = true
let test2 = equal_length [] [1; 2] = false
let test3 = equal_length [1; 2] [] = false
let test4 = equal_length [1; 3] [2; 4] = true
