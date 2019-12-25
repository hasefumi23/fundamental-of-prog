(* 目的: 受け取った文字列の lst を結合して返す *)
(* concat : string list -> string *)
let rec concat lst = match lst with
    [] -> ""
  | first :: rest -> first ^ concat rest

(* テスト *)
let test1 = concat [] = ""
let test2 = concat ["a"; "b"; "c"] = "abc"
