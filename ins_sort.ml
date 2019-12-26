(* 目的: リスト lst と 整数 n を受け取り n を lst の昇順となる位置に挿入する *)
(* ins_sort : int list n -> int list *)
let rec insert lst n = match lst with
    [] -> [n]
  | first :: rest -> if first > n then (n :: first :: rest)
                                  else (first :: insert rest n)

let test1 = insert [] 1 = [1]
let test2 = insert [1; 5; 6] 3 = [1; 3; 5; 6]
let test3 = insert [1; 5; 6] 10 = [1; 5; 6; 10]
let test4 = insert [5; 10; 20] 3 = [3; 5; 10; 20]

(* 目的: 整数のリストを受け取ったら、それを昇順に整列したリストを返す *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst = match lst with
    [] -> []
  | first :: rest -> insert (ins_sort rest) first

(* テスト *)
let test1 = ins_sort [] = []
(* let test2 = ins_sort [3; 2; 1] = [1; 2; 3] *)
let test2 = ins_sort [3; 2; 1]
(* let test3 = ins_sort [5; 3; 8; 1; 7; 4] = [1; 3; 4; 5; 7; 8] *)
let test3 = ins_sort [5; 3; 8; 1; 7; 4]
