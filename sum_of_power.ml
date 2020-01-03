(* 目的: 0 から受け取った自然数 n までの 2 乗の和を求める *)
(* sum_of_power : int -> int *)
let rec sum_of_power n =
  if n = 0 then 0
  else (n * n) + sum_of_power (n - 1)

(* テスト *)
let test1 = sum_of_power 0 = 0
let test2 = sum_of_power 1 = 1
let test3 = sum_of_power 2 = 5
let test4 = sum_of_power 3 = 14
