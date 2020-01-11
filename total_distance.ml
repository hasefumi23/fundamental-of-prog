(* 目的: 先頭からリスト中の各点までの距離の合計を計算する *)
(* total_distance : distance_t list -> distance_t list *)
type distance_t = {
  kyori : float;
  total : float;
}

(* 目的: 先頭からリスト中の各点までの距離の合計を計算する *)
(* ここで total0 はこれまでの距離の合計 *)
let rec total_distance lst =
  let rec hojo lst total0 = match lst with
      [] -> []
    | {kyori = k; total = t} :: rest ->
        {kyori = k; total = total0 +. k} :: hojo rest (total0 +.  k) in
          hojo lst 0.0
