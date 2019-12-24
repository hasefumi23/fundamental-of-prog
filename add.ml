(* 目的: ふたつの整数の組 pair を受け取りその要素の和を返す *)
(* add : int * int -> int *)
let add pair = match pair with (a, b) -> a + b

(* テスト *)
let test1 = add (0, 0) = 0
let test2 = add (3, 5) = 8
let test3 = add (3, -5) = -2

(* 目的: x 座標と y 座標の組で表された平面座標を受け取り、x 軸について対照な点を返す *)
(* taisho_x : int * int -> int * int *)
let taisho_x xy = match xy with (x, y) -> (x, -y)

(* テスト *)
let test1 = taisho_x (1, 5) = (1, -5)
let test2 = taisho_x (0, -3) = (0, 3)

type gakusei_t = {
  name : string;
  tensu :int;
  seiseki : string;
} ;;
