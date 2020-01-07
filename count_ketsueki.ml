type person_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  ketsueki : string; (* 成績 *)
}

(* 目的: person_t 型のリストを受け取ったら、その中から指定された血液型の人の数を返す *)
(* count_ketsueki : person_t list -> int *)
let rec count_ketsueki lst kata = match lst with
    [] -> 0
  | {namae = n; tensuu = t; ketsueki = s} :: rest ->
      if s = kata then 1 + (count_ketsueki rest kata)
      else (count_ketsueki rest kata)
  (* 0 count_ketsueki rest kata *)

(* テスト *)
let test1 = count_ketsueki [] "A" = 0
let test2 = count_ketsueki [
  {namae = "a"; tensuu = 10; ketsueki = "O"};
  {namae = "b"; tensuu = 20; ketsueki = "O"};
  {namae = "c"; tensuu = 30; ketsueki = "A"};
] "A" = 1
let test3 = count_ketsueki [
  {namae = "a"; tensuu = 10; ketsueki = "O"};
  {namae = "b"; tensuu = 20; ketsueki = "O"};
  {namae = "c"; tensuu = 30; ketsueki = "A"};
] "O" = 2
