(* 学生ひとり分のデータ(名前、点数、成績)を表す型 *)
type gakusei_t = {
  namae : string;   (* 名前 *)
  tensuu : int;     (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* 目的: 学生のデータ gakusei_t を受け取り成績のついたデータを返す *)
(* hyouka : gakusei_t -> gakusei_t *)
let hyouka gakusei = match gakusei with
  {namae = n; tensuu = t; seiseki = s} ->
    {namae = n;
     tensuu = t;
     seiseki = if t >= 80 then "A"
               else if t >= 70 then "B"
               else if t >= 60 then "C" else "D"
    }

(* テスト *)
let test1 = hyouka {namae="asai"; tensuu=90; seiseki=""}
                 = {namae="asai"; tensuu=90; seiseki="A"}
let test2 = hyouka {namae="asai"; tensuu=70; seiseki=""}
                 = {namae="asai"; tensuu=70; seiseki="B"}
