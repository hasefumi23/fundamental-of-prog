(* 八百屋においてある野菜と値段のリストの例 *)
let global_yaoya_list = [("トマト", 300); ("たまねぎ", 200); ("にんじん", 150); ("ほうれん草", 200)]

(* 目的: item の値段を調べる *)
(* price : string -> (string * int) list -> int *)
let rec price item yaoya_list = match yaoya_list with
    [] -> None
  | (yasai, nedan) :: rest ->
    if item = yasai then Some (nedan) else price item rest

(* 目的: yasai_list を買った時の値段の合計を調べる *)
(* total_price : string list -> (string * int) list -> int *)
let rec total_price yasai_list yaoya_list = match yasai_list with
    [] -> 0
  | first :: rest ->
    match price first yaoya_list with
        None -> total_price rest yaoya_list
      | Some (p) -> p + total_price rest yaoya_list

(* 目的: 野菜のリストのうち八百屋においていない野菜の数を返す *)
(* count_urikire_yasai : string list -> (string * int) list -> int *)
let rec count_urikire_yasai yasai_list yaoya_list = match yasai_list with
    [] -> 0
  | first :: rest ->
    match price first yaoya_list with
        None -> 1 + count_urikire_yasai rest yaoya_list
      | Some (_) -> 0 + count_urikire_yasai rest yaoya_list

let test1 = count_urikire_yasai ["レタス"; "大根"] global_yaoya_list

(* 売り切れを示す例外 *)
exception Urikire

(* 目的: item の値段を調べる *)
(* みつからないときには Urikire という例外を発生する *)
(* price : string -> (string * int) list -> int *)
let rec price item yaoya_list = match yaoya_list with
    [] -> raise Urikire
  | (yasai, nedan) :: rest ->
    if item = yasai then nedan else price item rest

(* 目的: yasai_list を買った時の値段の合計を調べる *)
(* total_price : string list -> (string * int) list -> int *)
let total_price yasai_list yaoya_list =
  let rec hojo yasai_list = match yasai_list with
      [] -> 0
    | first :: rest -> price first yaoya_list + hojo rest
      in try hojo yasai_list
    with Urikire -> 0
