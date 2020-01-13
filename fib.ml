(* 目的: フィボナッチ数を再帰回数とともに求める *)
(* fib : int -> int -> (int * int) *)
let rec fib n c =
  let c0 = c + 1 in
  if n < 2
  then (n, c0)
  else let (r1, c1) = fib (n - 1) c0 in
  let (r2, c2) = fib (n - 2) c1 in
  (r1 + r2, c2)

let test1 = fib 8 0

let count = ref 0
let rec fib2 n =
  (
    count := !count + 1;
    if n < 2 then n else fib2 (n - 1) + fib2 (n - 2)
  )
