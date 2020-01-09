(* 目的: ユークリッドの互除法を実装する *)
(* gcd : int -> int -> int *)
let rec gcd m n =
  if n = 0
  then m
  else gcd n (m mod n)

(* テスト *)
let test1 = gcd 10 0 = 10
let test2 = gcd 10 5 = 5
let test3 = gcd 12 8 = 4
