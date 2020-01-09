let add3 x = x + 3
(* let twice f =
  let g x = f (f x)
  in g

let test1 = twice add3 10
let add6 = twice add3
let test2 = add6 10 *)

let alpha a b = a
let beta a b = b

let compose f g =
  let h x = f (g x)
  in h
let test3 = (compose add3 add3) 4 = 10

fun x -> x + 1;;
(fun x -> x + 1) 5;;
let add1 = fun x -> x + 1;;

(fun x -> x * x - 1) 10;;

let sum lst = List.fold_right (+) lst 0
sum [1; 2; 3; 10]
