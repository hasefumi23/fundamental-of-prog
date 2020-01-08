type person_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  ketsueki : string; (* 成績 *)
}

(* 目的: person_t 型のリストを受け取ったら、その中に出てくる人の名前のリストを返す関数 *)
(* person_namae : person_t list -> string list *)
let p_namae person = match person with
    {namae = n; tensuu = _; ketsueki = _} -> n
let person_namae person_list = List.map p_namae person_list

(* テスト *)
let test1 = person_namae [] = []
let test2 = person_namae [
  {namae = "tanaka"; tensuu = 50; ketsueki = "A"};
  {namae = "yamada"; tensuu = 70; ketsueki = "A"};
] = ["tanaka"; "yamada"]
