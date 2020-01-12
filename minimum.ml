(* 目的: 受け取った lst の中の最小値を返す *)
(* minimum int list -> int *)
(* let rec minimum lst = match lst with
    [] -> max_int
  | first :: rest ->
  let min_rest = minimum rest in
  if first <= min_rest
    then first
    else min_rest *)

let rec new_minimum f rest = match rest with
    [] -> f
  | first :: rest ->
  let min_rest = new_minimum first rest in
  if first <= min_rest
  then first
  else min_rest

(* テスト *)
(* let test1 = new_minimum [3] = 3
let test2 = new_minimum [1; 2] = 1
let test3 = new_minimum [3; 2] = 2
let test4 = new_minimum [3; 2; 6; 4; 1; 8] = 1 *)

let test1 = new_minimum 3 [3] = 3
let test2 = new_minimum 1 [1; 2] = 1
let test3 = new_minimum 3 [3; 2] = 2
let test4 = new_minimum 3 [3; 2; 6; 4; 1; 8] = 1
