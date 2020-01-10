type eki_t = {
  namae  : string; (* 名前 *)
  saitan_kyori : float; (* 最短距離 *)
  temae_list  : string list; (* 駅名(漢字の文字列)のリスト *)
}

type ekikan_t = {
  kiten  : string; (* 起点 *)
  shuten : string; (* 終点 *)
  keiyu  : string; (* 経由線名 *)
  kyori  : float;  (* 距離 *)
  jikan  : int;    (* 時間 *)
}

type ekimei_t = {
  kanji   : string; (* 駅名 *)
  kana    : string; (* 読み *)
  romaji  : string; (* ローマ字 *)
  shozoku : string; (* 所属線名 *)
}

let global_ekikan_list = [
  {kiten="代々木上原"; shuten="代々木公園"; keiyu="千代田線"; kyori=1.0; jikan=2};
  {kiten="代々木公園"; shuten="明治神宮前"; keiyu="千代田線"; kyori=1.2; jikan=2};
  {kiten="明治神宮前"; shuten="表参道"; keiyu="千代田線"; kyori=0.9; jikan=2};
  {kiten="表参道"; shuten="乃木坂"; keiyu="千代田線"; kyori=1.4; jikan=3};
  {kiten="乃木坂"; shuten="赤坂"; keiyu="千代田線"; kyori=1.1; jikan=2};
  {kiten="赤坂"; shuten="国会議事堂前"; keiyu="千代田線"; kyori=0.8; jikan=1};
]

let global_ekimei_list = [
  {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"};
  {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"};
  {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"};
  {kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="千代田線"};
  {kanji="乃木坂"; kana="のぎざか"; romaji="nogizaka"; shozoku="千代田線"};
  {kanji="赤坂"; kana="あかさか"; romaji="akasaka"; shozoku="千代田線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; romaji="kokkaigijidoumae"; shozoku="千代田線"};
]

(* 目的: eki_t list 形のリストを受け取ったら「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組を返す *)
(* saitan_wo_bunri : eki_t list -> eki_t list *)
let saitan_wo_bunri lst =
  let rec minimum eki_list = match eki_list with
      [] -> {namae = ""; saitan_kyori = infinity; temae_list = []}
    | first :: rest -> match first with
    {namae = _; saitan_kyori = s; temae_list = _} ->
      let min_of_rest = minimum rest in
      match min_of_rest with {namae = _; saitan_kyori = min_s; temae_list = _} ->
        if s < min_s then first
        else min_of_rest in
          let min_eki = minimum lst in match min_eki with {namae = min_n; saitan_kyori = min_s; temae_list = min_t} ->
            let rest_eki_list = List.filter (fun {namae = com_n; saitan_kyori = con_s; temae_list = con_t} -> min_n != com_n || min_s != con_s || min_t != con_t) lst in
              (min_eki, rest_eki_list)

let saitan_wo_bunri lst = match lst with
    [] -> ({namae = ""; saitan_kyori = infinity; temae_list = []}, [])
  | first :: rest -> let comp_eki first_eki rest_result = match first_eki with
    {namae = _; saitan_kyori = f_s; temae_list = _} -> match rest_result with
      (rest_eki, rest_list) -> match rest_eki with
      {namae = _; saitan_kyori = r_s; temae_list = _} ->
        if f_s < r_s then (first_eki, rest_eki :: rest_list)
        else (rest_eki, first_eki :: rest_list) in
          List.fold_right comp_eki rest (first, [])

(* テスト *)
let test1 = saitan_wo_bunri [
  {namae = "代々木公園"; saitan_kyori = 10.; temae_list = []};
  {namae = "明治神宮前"; saitan_kyori = 12.2; temae_list = []};
  {namae = "代々木上原"; saitan_kyori = 1.5; temae_list = ["代々木上原"]};
] = ({namae = "代々木上原"; saitan_kyori = 1.5; temae_list = ["代々木上原"]}, [
  {namae = "明治神宮前"; saitan_kyori = 12.2; temae_list = []};
  {namae = "代々木公園"; saitan_kyori = 10.; temae_list = []};
])
