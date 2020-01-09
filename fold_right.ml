(* 目的: init から初めて lst の要素を右から順に f を施しこむ *)
(* fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_right f lst init = match lst with
    [] -> init
  | first :: rest -> f first (fold_right f rest init)

(* 目的: first と rest_result を加える *)
(* add_int : int -> int -> int *)
let add_int first rest_result = first + rest_result

(* 目的: 受け取った lst の各要素の和を求める *)
(* sum : int list -> int *)
let sum lst = fold_right add_int lst 0
let test_sum = sum [1; 2; 3]

(* 目的: first は無視して rest_result に 1 を加える *)
(* add_one : int -> int -> int *)
let add_one first rest_result = 1 + rest_result

(* 目的: 受け取ったリストlstの長さを求める *)
(* lenght : 'a list -> int *)
let length lst = fold_right add_one lst 0
let test_length = length [1; 2; 3; 4; 5]

(* 目的: 受け取ったリストlstの長さを求める *)
(* lenght : 'a list -> int *)
let length lst =
  let add_one _ rest_result = 1 + rest_result
  in fold_right add_one lst 0
let test_length = length [1; 2; 3; 4; 5]

(* 目的: 受け取った lst の各要素の和を求める *)
(* sum : int list -> int *)
let sum lst =
  (* 目的: first と rest_result を加える *)
  (* add_int : int -> int -> int *)
  let add_int first rest_result = first + rest_result
  in fold_right add_int lst 0

let test_sum = sum [1; 2; 3]

(* 目的: 受け取った lst の各要素の和を求める *)
(* sum : int list -> int *)
let sum lst = fold_right (fun first rest_result -> first + rest_result) lst 0
