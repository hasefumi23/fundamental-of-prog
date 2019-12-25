(* 目的: 受け取ったリスト lst の各要素の和を求める *)
(* length : int list -> int *)
let rec length lst = match lst with
    [] -> 0
  | _ :: rest -> 1 + length rest (* length rest *)

(* テスト *)
let test1 = length [] = 0
let test2 = length [1] = 1
let test3 = length [1; 2; 0; 5] = 4
