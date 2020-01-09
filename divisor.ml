(* 目的: n から 1 までのリストを作る *)
(* enumrate : int -> int list *)
let rec enumrate n = if n = 0 then [] else n :: enumrate (n - 1)
let test1 = enumrate 5
let test2 = enumrate 10
let test3 = enumrate 20

(* 目的: n の約数のリストを返す *)
let divisor n = List.filter (fun x -> n mod x = 0) (enumrate n)
let test4 = divisor 5
let test5 = divisor 15
let test6 = divisor 25

(* 目的: m 以下の完全数のリストを返す *)
(* perfect : int -> int list *)
let perfect m = List.filter (fun n -> List.fold_right (+) (divisor n) 0 - n = n) (enumrate m)
let test7 = perfect 100

(* 目的: 1 から受け取った自然数 n までの合計を求める *)
(* one_to_n : int -> int *)
let one_to_n n = List.fold_right (+) (enumrate n) 0
let test8 = one_to_n 10

(* 目的: 1 から受け取った自然数までの合計を求める *)
(* fac : int -> int *)
let fac n = List.fold_right ( * ) (enumrate n) 1
let test9 = fac 4
