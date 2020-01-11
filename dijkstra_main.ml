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
]

let rec get_ekikan_kyori eki1 eki2 ekikan_list = match ekikan_list with
    [] -> infinity
  | {kiten = k; shuten = s; keiyu = _; kyori = kyo; jikan = _} :: rest ->
    if eki1 = k && eki2 = s || eki1 = s && eki2 = k then kyo
    else get_ekikan_kyori eki1 eki2 rest

(* 目的: 直前に確定した駅 p (eki_t 型) と未確定の駅 q (eki_t 型) のリストを受け取って eki_t 型のリストを返す *)
(* FIXME: ここ直前に確定した駅の距離を足す必要がある気がする *)
let koushin eki eki_list g_ekikan_list = let koushin1 p q = match p with
  {namae = p_n; saitan_kyori = p_kyori; temae_list = p_t} -> match q with
  {namae = q_n; saitan_kyori = q_k; temae_list = q_t} ->
    let q_kyori = get_ekikan_kyori p_n q_n g_ekikan_list in
      let total_kyori = q_kyori +. p_kyori in
      if q_kyori = infinity || total_kyori >= q_k then q
      else {namae = q_n; saitan_kyori = total_kyori; temae_list = q_n :: p_t} in
        List.map (koushin1 eki) eki_list

let saitan_wo_bunri lst = match lst with
    [] -> ({namae = ""; saitan_kyori = infinity; temae_list = []}, [])
  | first :: rest -> let comp_eki first_eki rest_result = match first_eki with
    {namae = _; saitan_kyori = f_s; temae_list = _} -> match rest_result with
      (rest_eki, rest_list) -> match rest_eki with
      {namae = _; saitan_kyori = r_s; temae_list = _} ->
        if f_s < r_s then (first_eki, rest_eki :: rest_list)
        else (rest_eki, first_eki :: rest_list) in
          List.fold_right comp_eki rest (first, [])

(* 目的: ダイクストラのアルゴリズムを実装する *)
(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
let dijkstra_main eki_list ekikan_list =
  let rec dijkstra_sub kakutei_list mikakutei_list = if mikakutei_list = [] then kakutei_list else match kakutei_list with
      [] -> []
    | first :: _ -> let updated_list = koushin first mikakutei_list ekikan_list in
      let bunri_pair = saitan_wo_bunri updated_list in
        match bunri_pair with (burni_eki, bunri_eki_list) ->
          dijkstra_sub (burni_eki :: kakutei_list) bunri_eki_list in match eki_list with
            [] -> []
          | first :: rest -> List.rev (dijkstra_sub [first] rest)

let test1 = dijkstra_main [
  {namae = "代々木上原"; saitan_kyori = 0.; temae_list = ["代々木上原"]};
  {namae = "代々木公園"; saitan_kyori = infinity; temae_list = []};
  {namae = "明治神宮前"; saitan_kyori = infinity; temae_list = []};
  {namae = "表参道"; saitan_kyori = infinity; temae_list = []}]
  global_ekikan_list
= [
  {namae = "代々木上原"; saitan_kyori = 0.; temae_list = ["代々木上原"]};
  {namae = "代々木公園"; saitan_kyori = 1.0; temae_list = ["代々木公園"; "代々木上原"]};
  {namae = "明治神宮前"; saitan_kyori = 2.2; temae_list = ["明治神宮前"; "代々木公園"; "代々木上原"]};
  {namae = "表参道"; saitan_kyori = 3.1; temae_list = ["表参道"; "明治神宮前"; "代々木公園"; "代々木上原"]}
]
