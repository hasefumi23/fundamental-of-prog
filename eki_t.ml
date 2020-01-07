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
let rec make_eki_list ekimei_list = match ekimei_list with
    [] -> []
  | {kanji = k; kana = ka; romaji = r; shozoku = s} :: rest ->
      {namae = k; saitan_kyori = infinity; temae_list = []} :: make_eki_list rest

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
