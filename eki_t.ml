type eki_t = {
  namae  : string; (* 名前 *)
  saitan_kyori : float; (* 最短距離 *)
  temae_list  : string list; (* 駅名(漢字の文字列)のリスト *)
}

type ekimei_t = {
  kanji   : string; (* 駅名 *)
  kana    : string; (* 読み *)
  romaji  : string; (* ローマ字 *)
  shozoku : string; (* 所属線名 *)
}

(* 目的: ekimei_t 型のリストを受け取ったら、その駅名を使って eki_t 型のリストを作る *)
(* make_eki_list : ekimei_t list -> eki_t list *)
let rec make_eki_list make_ekimei_list = match make_ekimei_list with
    [] -> []
  | {kanji = k; kana = ka; romaji = r; shozoku = s} :: rest ->
      {namae = k; saitan_kyori = infinity; temae_list = []} :: make_eki_list rest

let make_eki_list eki_list =
  let convert {kanji = k; kana = ka; romaji = r; shozoku = s} = {namae = k; saitan_kyori = infinity; temae_list = []} in
    List.map convert eki_list

(* テスト *)
let test1 = make_eki_list [] = []
let test2 = make_eki_list [{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"};] = [{namae="代々木上原"; saitan_kyori=infinity; temae_list=[]}; {namae="代々木公園"; saitan_kyori=infinity; temae_list=[]}]

(* 目的: eki_t 型のリストと起点(漢字の文字列)を受け取ったら、起点のみいい感じになっている eki_t 型のリストを返す *)
(* shokika : eki_t list string -> eki_t list *)
let rec shokika eki_list kiten = match eki_list with
    [] -> []
  | first :: rest -> match first with
      {namae = n; saitan_kyori = s; temae_list = t} ->
      if n = kiten then
        {namae = n; saitan_kyori = 0.; temae_list = n :: []} :: shokika rest kiten
      else first :: shokika rest kiten

(* テスト *)
let test3 = shokika [] "代々木上原" = []
let test4 = shokika [{namae = "代々木上原"; saitan_kyori = infinity; temae_list = []}; {namae = "代々木公園"; saitan_kyori = infinity; temae_list = []}] "代々木上原" = [{namae = "代々木上原"; saitan_kyori = 0.; temae_list = ["代々木上原"]}; {namae = "代々木公園"; saitan_kyori = infinity; temae_list = []}]

(* 目的: ekimei_t 型のリストを受け取ったら、それをひらがなの順に整列し、さらに駅の重複を取り除いた ekimei_t 型のリストを返す *)
(* seiretsu : ekimei_t list -> ekimei_t list *)
let rec insert lst n = match lst with
    [] -> [n]
  | first :: rest -> match first with
      {kanji = k; kana = ka; romaji = r; shozoku = s} -> match n with
      {kanji = nk; kana = nka; romaji = nr; shozoku = ns} ->
      if ka = nka then (first :: rest)
      else if ka > nka then (n :: first :: rest)
      else (first :: insert rest n)

let rec seiretsu ekimei_lst = match ekimei_lst with
    [] -> []
  | first :: rest -> insert (seiretsu rest) first

let test5 = seiretsu [] = []
let test6 = seiretsu [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
  {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"};
  {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"};
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"};
] = [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"};
  {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"};
  {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"};
]

let make_eki_list eki_list =
  let convert {kanji = k; kana = ka; romaji = r; shozoku = s} = {namae = k; saitan_kyori = infinity; temae_list = []} in
    List.map convert eki_list

(* テスト *)
let test11 = make_eki_list [] = []
let test12 = make_eki_list [{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"};] = [{namae="代々木上原"; saitan_kyori=infinity; temae_list=[]}; {namae="代々木公園"; saitan_kyori=infinity; temae_list=[]}]

let shokika eki_list kiten = let shoki eki = match eki with
  {namae = n; saitan_kyori = _; temae_list = _} ->
  if n = kiten then {namae = n; saitan_kyori = 0.; temae_list = n :: []}
  else eki in List.map shoki eki_list

(* テスト *)
let test13 = shokika [] "代々木上原" = []
let test14 = shokika [
  {namae = "代々木上原"; saitan_kyori = infinity; temae_list = []};
  {namae = "代々木公園"; saitan_kyori = infinity; temae_list = []}] "代々木上原"
= [
  {namae = "代々木上原"; saitan_kyori = 0.; temae_list = ["代々木上原"]};
  {namae = "代々木公園"; saitan_kyori = infinity; temae_list = []}
]

(* make_initial_eki_list : ekimei_t list string *)
let make_initial_eki_list ekimei_list kiten = let shoki ekimei = match ekimei with
  {kanji = k; kana = _; romaji = _; shozoku = _} ->
    if k = kiten then {namae = k; saitan_kyori = 0.; temae_list = k :: []}
    else {namae = k; saitan_kyori = infinity; temae_list = []} in
      List.map shoki ekimei_list
