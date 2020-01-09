(* 目的: 2 以上 n 以下の自然数を受け取ったらその素数のリストを返す *)
(* sieve : int list -> int list *)
let rec sieve lst = match lst with
    [] -> []
  | first :: rest -> first :: sieve (List.filter (fun n -> (n mod first) != 0) rest)

(* テスト *)
let test1 = sieve [] = []
let test2 = sieve [2; 3; 4] = [2; 3]
let test3 = sieve [2; 3; 4; 5; 6; 7] = [2; 3; 5; 7]

(* 目的: 自然数 n 以下の素数をすべて求めるエラトステネスのふるいを実装する *)
(* prime : int -> int list *)
let prime n =
  if n < 3 then [n]
  else let rec make_num x = if x < 3 then [x] else x :: make_num (x - 1)
    in sieve (List.rev (make_num n))

(* テスト *)
let test4 = prime 3 = [2; 3]
let test5 = prime 20 = [2; 3; 5; 7; 11; 13; 17; 19]
