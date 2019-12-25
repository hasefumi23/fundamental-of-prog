(* 目的: 駅の情報を表す型 *)
type ekimei_t = {
  kanji : string; (* 漢字の駅名 *)
  kana : string; (* ひらがなの駅名 *)
  romaji : string; (* ローマ字の駅名 *)
  shozoku : string; (* 所属する路線名 *)
}

(* 駅の情報 ekimei_t を受け取って情報を表示する *)
(* hyouji ekimei_t -> string *)
let hyouji ekimei = match ekimei with
  {kanji = kanji_a;
   kana = kana_a;
   romaji = romaji_a;
   shozoku = shozoku_a;
  } -> shozoku_a ^ "、" ^ kanji_a ^ "(" ^ kana_a ^ ")"

(* テスト *)
let test1 = hyouji {kanji = "佐賀"; kana = "さが"; romaji = "saga"; shozoku = "佐賀線"} = "佐賀線、佐賀(さが)"
let test2 = hyouji {kanji = "福岡"; kana = "ふくおか"; romaji = "fukuoka"; shozoku = "九州線"} = "九州線、福岡(ふくおか)"

(* 駅間 *)
type ekikan_t = {
  kiten : string; (* 起点 *)
  shuten : string; (* 終点 *)
  keiyu : string; (* 経由 *)
  kyori : float; (* 距離 *)
  jikan : int; (* 時間 *)
}

(* let lis = "sunday" :: "monday" :: "tuesday" :: [] *)
(* match [1; 2; 3] with 
  [] -> 0
| first :: rest -> first ;; *)
