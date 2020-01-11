(* 目的: (lst の逆順のリスト) @ rsult を返す *)
(* ここで result はこれまでの要素を逆順にしたリストを示す *)
let rec lst result = match lst with
    [] -> []
  | first :: rest -> []

(* 目的: 与えられたリストを逆順にして返す *)
(* reverse : 'a list -> 'a list *)
let rec reverse lst = match lst with
    [] -> []
  | first :: rest -> (reverse rest) @ [first]

(* テスト *)
let test1 = reverse [0; 1; 2; 3; 4; 5;]
= [5; 4; 3; 2; 1; 0;]
