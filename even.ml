(* 目的: 受け取ったリスト lst の偶数の要素のみを含むリストを返す *)
(* even : int list -> int list *)
let rec even lst = match lst with
    [] -> []
  | first :: rest -> if first mod 2 = 0 then first :: even rest
                                        else even rest

(* テスト *)
let test1 = even [] = []
let test2 = even [1; 2; 4; 5] = [2; 4]
let test3 = even [1; 3; 5] = []
