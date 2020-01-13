(* 座標を表すモジュール *)
module Zahyo = struct
  let x = 3.0
  let y = 4.0
  let kyori (a, b) =
    sqrt ((x -. a) *. (x -. a) +. (y -.b) *. (y -. b))
end
