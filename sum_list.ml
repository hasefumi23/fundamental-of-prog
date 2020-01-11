(* 目的: 与えられたlst の先頭から各要素を加算した値のリストを返す *)
(* sum_list : int list -> int list *)
let sum_list lst = let rec hojo rest_lst total = match rest_lst with
    [] -> []
  | first :: rest -> let tmp_total = first + total in
     tmp_total :: (hojo rest tmp_total) in hojo lst 0

(* テスト *)
let test1 = sum_list [3; 2; 1; 4]
= [3; 5; 6; 10]
