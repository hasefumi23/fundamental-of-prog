(* 目的: 受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let rec sum lst = match lst with
    [] -> 0
  | first :: rest -> first + sum rest (* sum rest *)

(* テスト *)
let test1 = sum [] = 0
let test2 = sum [2] = 2
let test3 = sum [1; 3] = 4
