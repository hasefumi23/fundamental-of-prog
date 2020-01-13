(* 0 がみつかったことを示す例外 *)
exception Zero

(* 目的: lst 中の整数を全て掛け合わせる *)
(* times : int list -> int *)
let times lst =
  let rec hojo lst = match lst with
      [] -> 1
    | first :: rest ->
      if first = 0 then raise Zero
      else first * hojo rest
    in try hojo lst with Zero -> 0
