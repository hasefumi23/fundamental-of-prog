(* --- START: dijkstra を tree を使った実装に書き直すためのコード --- *)

type tmp_ekikan_tree_t = {
  namae : string; (* 駅名(漢字) *)
  eki_list : (string * float) list; (* 「上記の駅に直接つながっている駅名(漢字)とその駅までの距離の組」のリスト *)
}

type ekikan_tree_t = Empty
                   | Node of ekikan_tree_t * (string * (string * float) list) * ekikan_tree_t

(* テスト *)
let test1 = Empty
let test2 = Node (Empty, ("a", [("b", 10.)]), Empty)

(* 「駅名」と「駅名と距離の組のリスト」を受け取ったら、その駅までの距離を返す *)
(* assoc : string (string * int) list *)
let rec assoc ekimei list = match list with
    [] -> infinity
  | (eki, kyori) :: rest -> if ekimei = eki then kyori else assoc ekimei rest

(* テスト *)
let test1 = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8
let test2 = assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] = infinity

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

let test_tree1
= Node (
  Node (
    Empty,
    ("新大塚", [("茗荷谷", 1.2)]),
    Empty
  ),
  ("茗荷谷", [("新大塚", 1.2)]),
  Empty
)

(* ekikan_tree_t 型の木と ekikan_t 型の駅間を受け取ったら、その情報を挿入した木を返す *)
(* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let rec ins_ekikan tree comp_ekimei into_ekimei kyori = match tree with
    Empty -> Node (Empty, (comp_ekimei, [(into_ekimei, kyori)]), Empty)
  | Node (t1, eki_k, t2) -> match eki_k with
    (name, ekikan_list) ->
      if comp_ekimei = name then Node (t1, (name, (into_ekimei, kyori) :: ekikan_list), t2)
      else if comp_ekimei < name then Node ((ins_ekikan t1 comp_ekimei into_ekimei kyori), eki_k, t2)
      else Node (t1, eki_k, (ins_ekikan t2 comp_ekimei into_ekimei kyori))

let test10 = ins_ekikan Empty "新大塚" "茗荷谷" 1.2
= Node (
  Empty,
  ("新大塚", [("茗荷谷", 1.2)]),
  Empty
)
let test11 = ins_ekikan test_tree1 "新大塚" "渋谷" 1.3
= Node (
  Node (
    Empty,
    ("新大塚", [("渋谷", 1.3); ("茗荷谷", 1.2)]),
    Empty
  ),
  ("茗荷谷", [("新大塚", 1.2)]),
  Empty
)
let test12 = ins_ekikan test_tree1 "霞が関" "渋谷" 2.3
= Node (
  Node (
    Empty,
    ("新大塚", [("茗荷谷", 1.2)]),
    Empty
  ),
  ("茗荷谷", [("新大塚", 1.2)]),
  Node (
    Empty,
    ("霞が関", [("渋谷", 2.3)]),
    Empty
  )
)

let insert_ekikan ekikan tree = match ekikan with
  {kiten = k; shuten = s; kyori = kyo} ->
    if k > s then let first_tree = ins_ekikan tree k s kyo in
      ins_ekikan first_tree s k kyo
    else let first_tree = ins_ekikan tree s k kyo in
      ins_ekikan first_tree k s kyo

(* テスト *)
let test20 = insert_ekikan {kiten = "新大塚"; shuten = "茗荷谷"; kyori = 1.2; keiyu = "keiyu"; jikan = 1} Empty
= Node (
  Node (
    Empty,
    ("新大塚", [("茗荷谷", 1.2)]),
    Empty
  ),
  ("茗荷谷", [("新大塚", 1.2)]),
  Empty
)

(* 目的: ekikan_tree_t 型の木と ekikan_t list 型の駅間のリストを受け取ったら、リストの中に含まれる駅間を全て挿入した木を返す *)
(* inserts_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
(* let rec inserts_ekikan ekikan_list tree = match ekikan_list with
    [] -> tree
  | first :: rest -> let first_tree = insert_ekikan first tree in
    inserts_ekikan rest first_tree *)

let inserts_ekikan tree ekikan_list = List.fold_right insert_ekikan ekikan_list tree

(* テスト *)
let test30 = inserts_ekikan Empty [
  {kiten = "新大塚"; shuten = "茗荷谷"; kyori = 1.2; keiyu = "keiyu"; jikan = 1};
  {kiten = "新大塚"; shuten = "渋谷"; kyori = 2.2; keiyu = "keiyu"; jikan = 1}
]
= Node (
  Node (
    Empty,
    ("新大塚", [("茗荷谷", 1.2); ("渋谷", 2.2)]),
    Empty
  ),
  ("渋谷", [("新大塚", 2.2)]),
  Node (
    Empty,
    ("茗荷谷", [("新大塚", 1.2)]),
    Empty
  )
)

let global_ekimei_list = [
  {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"};
  {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"};
  {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"};
  {kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="千代田線"};
  {kanji="乃木坂"; kana="のぎざか"; romaji="nogizaka"; shozoku="千代田線"};
  {kanji="赤坂"; kana="あかさか"; romaji="akasaka"; shozoku="千代田線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; romaji="kokkaigijidoumae"; shozoku="千代田線"};
  {kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="千代田線"};
  {kanji="日比谷"; kana="ひびや"; romaji="hibiya"; shozoku="千代田線"};
  {kanji="二重橋前"; kana="にじゅうばしまえ"; romaji="nijuubasimae"; shozoku="千代田線"};
  {kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="千代田線"};
  {kanji="新御茶ノ水"; kana="しんおちゃのみず"; romaji="shin-ochanomizu"; shozoku="千代田線"};
  {kanji="湯島"; kana="ゆしま"; romaji="yushima"; shozoku="千代田線"};
  {kanji="根津"; kana="ねづ"; romaji="nedu"; shozoku="千代田線"};
  {kanji="千駄木"; kana="せんだぎ"; romaji="sendagi"; shozoku="千代田線"};
  {kanji="西日暮里"; kana="にしにっぽり"; romaji="nishinippori"; shozoku="千代田線"};
  {kanji="町屋"; kana="まちや"; romaji="machiya"; shozoku="千代田線"};
  {kanji="北千住"; kana="きたせんじゅ"; romaji="kitasenjyu"; shozoku="千代田線"};
  {kanji="綾瀬"; kana="あやせ"; romaji="ayase"; shozoku="千代田線"};
  {kanji="北綾瀬"; kana="きたあやせ"; romaji="kitaayase"; shozoku="千代田線"};
  {kanji="浅草"; kana="あさくさ"; romaji="asakusa"; shozoku="銀座線"};
  {kanji="田原町"; kana="たわらまち"; romaji="tawaramachi"; shozoku="銀座線"};
  {kanji="稲荷町"; kana="いなりちょう"; romaji="inaricho"; shozoku="銀座線"};
  {kanji="上野"; kana="うえの"; romaji="ueno"; shozoku="銀座線"};
  {kanji="上野広小路"; kana="うえのひろこうじ"; romaji="uenohirokoji"; shozoku="銀座線"};
  {kanji="末広町"; kana="すえひろちょう"; romaji="suehirocho"; shozoku="銀座線"};
  {kanji="神田"; kana="かんだ"; romaji="kanda"; shozoku="銀座線"};
  {kanji="三越前"; kana="みつこしまえ"; romaji="mitsukoshimae"; shozoku="銀座線"};
  {kanji="日本橋"; kana="にほんばし"; romaji="nihonbashi"; shozoku="銀座線"};
  {kanji="京橋"; kana="きょうばし"; romaji="kyobashi"; shozoku="銀座線"};
  {kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="銀座線"};
  {kanji="新橋"; kana="しんばし"; romaji="shinbashi"; shozoku="銀座線"};
  {kanji="虎ノ門"; kana="とらのもん"; romaji="toranomon"; shozoku="銀座線"};
  {kanji="溜池山王"; kana="ためいけさんのう"; romaji="tameikesannou"; shozoku="銀座線"};
  {kanji="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; shozoku="銀座線"};
  {kanji="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyamaicchome"; shozoku="銀座線"};
  {kanji="外苑前"; kana="がいえんまえ"; romaji="gaienmae"; shozoku="銀座線"};
  {kanji="表参道"; kana="おもてさんどう"; romaji="omotesando"; shozoku="銀座線"};
  {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="銀座線"};
  {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="半蔵門線"};
  {kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="半蔵門線"};
  {kanji="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyama-itchome"; shozoku="半蔵門線"};
  {kanji="永田町"; kana="ながたちょう"; romaji="nagatacho"; shozoku="半蔵門線"};
  {kanji="半蔵門"; kana="はんぞうもん"; romaji="hanzomon"; shozoku="半蔵門線"};
  {kanji="九段下"; kana="くだんした"; romaji="kudanshita"; shozoku="半蔵門線"};
  {kanji="神保町"; kana="じんぼうちょう"; romaji="jinbocho"; shozoku="半蔵門線"};
  {kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="半蔵門線"};
  {kanji="三越前"; kana="みつこしまえ"; romaji="mitsukoshimae"; shozoku="半蔵門線"};
  {kanji="水天宮前"; kana="すいてんぐうまえ"; romaji="suitengumae"; shozoku="半蔵門線"};
  {kanji="清澄白河"; kana="きよすみしらかわ"; romaji="kiyosumi-shirakawa"; shozoku="半蔵門線"};
  {kanji="住吉"; kana="すみよし"; romaji="sumiyoshi"; shozoku="半蔵門線"};
  {kanji="錦糸町"; kana="きんしちょう"; romaji="kinshicho"; shozoku="半蔵門線"};
  {kanji="押上"; kana="おしあげ"; romaji="oshiage"; shozoku="半蔵門線"};
  {kanji="中目黒"; kana="なかめぐろ"; romaji="nakameguro"; shozoku="日比谷線"};
  {kanji="恵比寿"; kana="えびす"; romaji="ebisu"; shozoku="日比谷線"};
  {kanji="広尾"; kana="ひろお"; romaji="hiro"; shozoku="日比谷線"};
  {kanji="六本木"; kana="ろっぽんぎ"; romaji="roppongi"; shozoku="日比谷線"};
  {kanji="神谷町"; kana="かみやちょう"; romaji="kamiyacho"; shozoku="日比谷線"};
  {kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="日比谷線"};
  {kanji="日比谷"; kana="ひびや"; romaji="hibiya"; shozoku="日比谷線"};
  {kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="日比谷線"};
  {kanji="東銀座"; kana="ひがしぎんざ"; romaji="higashiginza"; shozoku="日比谷線"};
  {kanji="築地"; kana="つきじ"; romaji="tsukiji"; shozoku="日比谷線"};
  {kanji="八丁堀"; kana="はっちょうぼり"; romaji="hacchobori"; shozoku="日比谷線"};
  {kanji="茅場町"; kana="かやばちょう"; romaji="kayabacho"; shozoku="日比谷線"};
  {kanji="人形町"; kana="にんぎょうちょう"; romaji="ningyomachi"; shozoku="日比谷線"};
  {kanji="小伝馬町"; kana="こでんまちょう"; romaji="kodemmacho"; shozoku="日比谷線"};
  {kanji="秋葉原"; kana="あきはばら"; romaji="akihabara"; shozoku="日比谷線"};
  {kanji="仲御徒町"; kana="なかおかちまち"; romaji="nakaokachimachi"; shozoku="日比谷線"};
  {kanji="上野"; kana="うえの"; romaji="ueno"; shozoku="日比谷線"};
  {kanji="入谷"; kana="いりや"; romaji="iriya"; shozoku="日比谷線"};
  {kanji="三ノ輪"; kana="みのわ"; romaji="minowa"; shozoku="日比谷線"};
  {kanji="南千住"; kana="みなみせんじゅ"; romaji="minamisenju"; shozoku="日比谷線"};
  {kanji="北千住"; kana="きたせんじゅ"; romaji="kitasenju"; shozoku="日比谷線"};
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
  {kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="丸ノ内線"};
  {kanji="本郷三丁目"; kana="ほんごうさんちょうめ"; romaji="hongosanchome"; shozoku="丸ノ内線"};
  {kanji="御茶ノ水"; kana="おちゃのみず"; romaji="ochanomizu"; shozoku="丸ノ内線"};
  {kanji="淡路町"; kana="あわじちょう"; romaji="awajicho"; shozoku="丸ノ内線"};
  {kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="丸ノ内線"};
  {kanji="東京"; kana="とうきょう"; romaji="tokyo"; shozoku="丸ノ内線"};
  {kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="丸ノ内線"};
  {kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="丸ノ内線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; romaji="kokkaigijidomae"; shozoku="丸ノ内線"};
  {kanji="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; shozoku="丸ノ内線"};
  {kanji="四ツ谷"; kana="よつや"; romaji="yotsuya"; shozoku="丸ノ内線"};
  {kanji="四谷三丁目"; kana="よつやさんちょうめ"; romaji="yotsuyasanchome"; shozoku="丸ノ内線"};
  {kanji="新宿御苑前"; kana="しんじゅくぎょえんまえ"; romaji="shinjuku-gyoemmae"; shozoku="丸ノ内線"};
  {kanji="新宿三丁目"; kana="しんじゅくさんちょうめ"; romaji="shinjuku-sanchome"; shozoku="丸ノ内線"};
  {kanji="新宿"; kana="しんじゅく"; romaji="shinjuku"; shozoku="丸ノ内線"};
  {kanji="西新宿"; kana="にししんじゅく"; romaji="nishi-shinjuku"; shozoku="丸ノ内線"};
  {kanji="中野坂上"; kana="なかのさかうえ"; romaji="nakano-sakaue"; shozoku="丸ノ内線"};
  {kanji="新中野"; kana="しんなかの"; romaji="shin-nakano"; shozoku="丸ノ内線"};
  {kanji="東高円寺"; kana="ひがしこうえんじ"; romaji="higashi-koenji"; shozoku="丸ノ内線"};
  {kanji="新高円寺"; kana="しんこうえんじ"; romaji="shin-koenji"; shozoku="丸ノ内線"};
  {kanji="南阿佐ヶ谷"; kana="みなみあさがや"; romaji="minami-asagaya"; shozoku="丸ノ内線"};
  {kanji="荻窪"; kana="おぎくぼ"; romaji="ogikubo"; shozoku="丸ノ内線"};
  {kanji="中野新橋"; kana="なかのしんばし"; romaji="nakano-shimbashi"; shozoku="丸ノ内線"};
  {kanji="中野富士見町"; kana="なかのふじみちょう"; romaji="nakano-fujimicho"; shozoku="丸ノ内線"};
  {kanji="方南町"; kana="ほうなんちょう"; romaji="honancho"; shozoku="丸ノ内線"};
  {kanji="四ツ谷"; kana="よつや"; romaji="yotsuya"; shozoku="南北線"};
  {kanji="永田町"; kana="ながたちょう"; romaji="nagatacho"; shozoku="南北線"};
  {kanji="溜池山王"; kana="ためいけさんのう"; romaji="tameikesanno"; shozoku="南北線"};
  {kanji="六本木一丁目"; kana="ろっぽんぎいっちょうめ"; romaji="roppongiitchome"; shozoku="南北線"};
  {kanji="麻布十番"; kana="あざぶじゅうばん"; romaji="azabujuban"; shozoku="南北線"};
  {kanji="白金高輪"; kana="しろかねたかなわ"; romaji="shirokanetakanawa"; shozoku="南北線"};
  {kanji="白金台"; kana="しろかねだい"; romaji="shirokanedai"; shozoku="南北線"};
  {kanji="目黒"; kana="めぐろ"; romaji="meguro"; shozoku="南北線"};
  {kanji="市ヶ谷"; kana="いちがや"; romaji="ichigaya"; shozoku="南北線"};
  {kanji="飯田橋"; kana="いいだばし"; romaji="idabashi"; shozoku="南北線"};
  {kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="南北線"};
  {kanji="東大前"; kana="とうだいまえ"; romaji="todaimae"; shozoku="南北線"};
  {kanji="本駒込"; kana="ほんこまごめ"; romaji="honkomagome"; shozoku="南北線"};
  {kanji="駒込"; kana="こまごめ"; romaji="komagome"; shozoku="南北線"};
  {kanji="西ヶ原"; kana="にしがはら"; romaji="nishigahara"; shozoku="南北線"};
  {kanji="王子"; kana="おうじ"; romaji="oji"; shozoku="南北線"};
  {kanji="王子神谷"; kana="おうじかみや"; romaji="ojikamiya"; shozoku="南北線"};
  {kanji="志茂"; kana="しも"; romaji="shimo"; shozoku="南北線"};
  {kanji="赤羽岩淵"; kana="あかばねいわぶち"; romaji="akabaneiwabuchi"; shozoku="南北線"};
  {kanji="西船橋"; kana="にしふなばし"; romaji="nishi-funabashi"; shozoku="東西線"};
  {kanji="原木中山"; kana="ばらきなかやま"; romaji="baraki-nakayama"; shozoku="東西線"};
  {kanji="妙典"; kana="みょうでん"; romaji="myoden"; shozoku="東西線"};
  {kanji="行徳"; kana="ぎょうとく"; romaji="gyotoku"; shozoku="東西線"};
  {kanji="南行徳"; kana="みなみぎょうとく"; romaji="minami-gyotoku"; shozoku="東西線"};
  {kanji="浦安"; kana="うらやす"; romaji="urayasu"; shozoku="東西線"};
  {kanji="葛西"; kana="かさい"; romaji="kasai"; shozoku="東西線"};
  {kanji="西葛西"; kana="にしかさい"; romaji="nishi-kasai"; shozoku="東西線"};
  {kanji="南砂町"; kana="みなみすなまち"; romaji="minami-sunamachi"; shozoku="東西線"};
  {kanji="東陽町"; kana="とうようちょう"; romaji="touyoucho"; shozoku="東西線"};
  {kanji="木場"; kana="きば"; romaji="kiba"; shozoku="東西線"};
  {kanji="門前仲町"; kana="もんぜんなかちょう"; romaji="monzen-nakacho"; shozoku="東西線"};
  {kanji="茅場町"; kana="かやばちょう"; romaji="kayabacho"; shozoku="東西線"};
  {kanji="日本橋"; kana="にほんばし"; romaji="nihonbashi"; shozoku="東西線"};
  {kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="東西線"};
  {kanji="竹橋"; kana="たけばし"; romaji="takebashi"; shozoku="東西線"};
  {kanji="九段下"; kana="くだんした"; romaji="kudanshita"; shozoku="東西線"};
  {kanji="飯田橋"; kana="いいだばし"; romaji="iidabashi"; shozoku="東西線"};
  {kanji="神楽坂"; kana="かぐらざか"; romaji="kagurazaka"; shozoku="東西線"};
  {kanji="早稲田"; kana="わせだ"; romaji="waseda"; shozoku="東西線"};
  {kanji="高田馬場"; kana="たかだのばば"; romaji="takadanobaba"; shozoku="東西線"};
  {kanji="落合"; kana="おちあい"; romaji="ochiai"; shozoku="東西線"};
  {kanji="中野"; kana="なかの"; romaji="nakano"; shozoku="東西線"};
  {romaji="shinkiba"; kana="しんきば"; kanji="新木場"; shozoku="有楽町線"};
  {romaji="tatsumi"; kana="たつみ"; kanji="辰巳"; shozoku="有楽町線"};
  {romaji="toyosu"; kana="とよす"; kanji="豊洲"; shozoku="有楽町線"};
  {romaji="tsukishima"; kana="つきしま"; kanji="月島"; shozoku="有楽町線"};
  {romaji="shintomityou"; kana="しんとみちょう"; kanji="新富町"; shozoku="有楽町線"};
  {romaji="ginzaittyoume"; kana="ぎんざいっちょうめ"; kanji="銀座一丁目"; shozoku="有楽町線"};
  {romaji="yuurakutyou"; kana="ゆうらくちょう"; kanji="有楽町"; shozoku="有楽町線"};
  {romaji="sakuradamon"; kana="さくらだもん"; kanji="桜田門"; shozoku="有楽町線"};
  {romaji="nagatacho"; kana="ながたちょう"; kanji="永田町"; shozoku="有楽町線"};
  {romaji="koujimachi"; kana="こうじまち"; kanji="麹町"; shozoku="有楽町線"};
  {romaji="ichigaya"; kana="いちがや"; kanji="市ヶ谷"; shozoku="有楽町線"};
  {romaji="iidabashi"; kana="いいだばし"; kanji="飯田橋"; shozoku="有楽町線"};
  {kanji="江戸川橋"; kana="えどがわばし"; romaji="edogawabasi"; shozoku="有楽町線"};
  {kanji="護国寺"; kana="ごこくじ"; romaji="gokokuji"; shozoku="有楽町線"};
  {kanji="東池袋"; kana="ひがしいけぶくろ"; romaji="higasiikebukuro"; shozoku="有楽町線"};
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"};
  {kanji="要町"; kana="かなめちょう"; romaji="kanametyou"; shozoku="有楽町線"};
  {kanji="千川"; kana="せんかわ"; romaji="senkawa"; shozoku="有楽町線"};
  {kanji="小竹向原"; kana="こたけむかいはら"; romaji="kotakemukaihara"; shozoku="有楽町線"};
  {kanji="氷川台"; kana="ひかわだい"; romaji="hikawadai"; shozoku="有楽町線"};
  {kanji="平和台"; kana="へいわだい"; romaji="heiwadai"; shozoku="有楽町線"};
  {kanji="営団赤塚"; kana="えいだんあかつか"; romaji="eidanakakuka"; shozoku="有楽町線"};
  {kanji="営団成増"; kana="えいだんなります"; romaji="eidannarimasu"; shozoku="有楽町線"};
  {kanji="和光市"; kana="わこうし"; romaji="wakousi"; shozoku="有楽町線"};
]

let global_ekikan_list = [
  {kiten="代々木上原"; shuten="代々木公園"; keiyu="千代田線"; kyori=1.0; jikan=2};
  {kiten="代々木公園"; shuten="明治神宮前"; keiyu="千代田線"; kyori=1.2; jikan=2};
  {kiten="明治神宮前"; shuten="表参道"; keiyu="千代田線"; kyori=0.9; jikan=2};
  {kiten="表参道"; shuten="乃木坂"; keiyu="千代田線"; kyori=1.4; jikan=3};
  {kiten="乃木坂"; shuten="赤坂"; keiyu="千代田線"; kyori=1.1; jikan=2};
  {kiten="赤坂"; shuten="国会議事堂前"; keiyu="千代田線"; kyori=0.8; jikan=1};
  {kiten="国会議事堂前"; shuten="霞ヶ関"; keiyu="千代田線"; kyori=0.7; jikan=1};
  {kiten="霞ヶ関"; shuten="日比谷"; keiyu="千代田線"; kyori=1.2; jikan=2};
  {kiten="日比谷"; shuten="二重橋前"; keiyu="千代田線"; kyori=0.7; jikan=1};
  {kiten="二重橋前"; shuten="大手町"; keiyu="千代田線"; kyori=0.7; jikan=1};
  {kiten="大手町"; shuten="新御茶ノ水"; keiyu="千代田線"; kyori=1.3; jikan=2};
  {kiten="新御茶ノ水"; shuten="湯島"; keiyu="千代田線"; kyori=1.2; jikan=2};
  {kiten="湯島"; shuten="根津"; keiyu="千代田線"; kyori=1.2; jikan=2};
  {kiten="根津"; shuten="千駄木"; keiyu="千代田線"; kyori=1.0; jikan=2};
  {kiten="千駄木"; shuten="西日暮里"; keiyu="千代田線"; kyori=0.9; jikan=1};
  {kiten="西日暮里"; shuten="町屋"; keiyu="千代田線"; kyori=1.7; jikan=2};
  {kiten="町屋"; shuten="北千住"; keiyu="千代田線"; kyori=2.6; jikan=3};
  {kiten="北千住"; shuten="綾瀬"; keiyu="千代田線"; kyori=2.5; jikan=3};
  {kiten="綾瀬"; shuten="北綾瀬"; keiyu="千代田線"; kyori=2.1; jikan=4};
  {kiten="浅草"; shuten="田原町"; keiyu="銀座線"; kyori=0.8; jikan=2};
  {kiten="田原町"; shuten="稲荷町"; keiyu="銀座線"; kyori=0.7; jikan=1};
  {kiten="稲荷町"; shuten="上野"; keiyu="銀座線"; kyori=0.7; jikan=2};
  {kiten="上野"; shuten="上野広小路"; keiyu="銀座線"; kyori=0.5; jikan=2};
  {kiten="上野広小路"; shuten="末広町"; keiyu="銀座線"; kyori=0.6; jikan=1};
  {kiten="末広町"; shuten="神田"; keiyu="銀座線"; kyori=1.1; jikan=2};
  {kiten="神田"; shuten="三越前"; keiyu="銀座線"; kyori=0.7; jikan=1};
  {kiten="三越前"; shuten="日本橋"; keiyu="銀座線"; kyori=0.6; jikan=2};
  {kiten="日本橋"; shuten="京橋"; keiyu="銀座線"; kyori=0.7; jikan=2};
  {kiten="京橋"; shuten="銀座"; keiyu="銀座線"; kyori=0.7; jikan=1};
  {kiten="銀座"; shuten="新橋"; keiyu="銀座線"; kyori=0.9; jikan=2};
  {kiten="新橋"; shuten="虎ノ門"; keiyu="銀座線"; kyori=0.8; jikan=2};
  {kiten="虎ノ門"; shuten="溜池山王"; keiyu="銀座線"; kyori=0.6; jikan=2};
  {kiten="溜池山王"; shuten="赤坂見附"; keiyu="銀座線"; kyori=0.9; jikan=2};
  {kiten="赤坂見附"; shuten="青山一丁目"; keiyu="銀座線"; kyori=1.3; jikan=2};
  {kiten="青山一丁目"; shuten="外苑前"; keiyu="銀座線"; kyori=0.7; jikan=2};
  {kiten="外苑前"; shuten="表参道"; keiyu="銀座線"; kyori=0.7; jikan=1};
  {kiten="表参道"; shuten="渋谷"; keiyu="銀座線"; kyori=1.3; jikan=1};
  {kiten="渋谷"; shuten="表参道"; keiyu="半蔵門線"; kyori=1.3; jikan=2};
  {kiten="表参道"; shuten="青山一丁目"; keiyu="半蔵門線"; kyori=1.4; jikan=2};
  {kiten="青山一丁目"; shuten="永田町"; keiyu="半蔵門線"; kyori=1.3; jikan=2};
  {kiten="永田町"; shuten="半蔵門"; keiyu="半蔵門線"; kyori=1.0; jikan=2};
  {kiten="半蔵門"; shuten="九段下"; keiyu="半蔵門線"; kyori=1.6; jikan=2};
  {kiten="九段下"; shuten="神保町"; keiyu="半蔵門線"; kyori=0.4; jikan=1};
  {kiten="神保町"; shuten="大手町"; keiyu="半蔵門線"; kyori=1.7; jikan=3};
  {kiten="大手町"; shuten="三越前"; keiyu="半蔵門線"; kyori=0.7; jikan=1};
  {kiten="三越前"; shuten="水天宮前"; keiyu="半蔵門線"; kyori=1.3; jikan=2};
  {kiten="水天宮前"; shuten="清澄白河"; keiyu="半蔵門線"; kyori=1.7; jikan=3};
  {kiten="清澄白河"; shuten="住吉"; keiyu="半蔵門線"; kyori=1.9; jikan=3};
  {kiten="住吉"; shuten="錦糸町"; keiyu="半蔵門線"; kyori=1.; jikan=2};
  {kiten="錦糸町"; shuten="押上"; keiyu="半蔵門線"; kyori=1.4; jikan=2};
  {kiten="中目黒"; shuten="恵比寿"; keiyu="日比谷線"; kyori=1.; jikan=2};
  {kiten="恵比寿"; shuten="広尾"; keiyu="日比谷線"; kyori=1.5; jikan=3};
  {kiten="広尾"; shuten="六本木"; keiyu="日比谷線"; kyori=1.7; jikan=3};
  {kiten="六本木"; shuten="神谷町"; keiyu="日比谷線"; kyori=1.5; jikan=3};
  {kiten="神谷町"; shuten="霞ヶ関"; keiyu="日比谷線"; kyori=1.3; jikan=2};
  {kiten="霞ヶ関"; shuten="日比谷"; keiyu="日比谷線"; kyori=1.2; jikan=2};
  {kiten="日比谷"; shuten="銀座"; keiyu="日比谷線"; kyori=0.4; jikan=1};
  {kiten="銀座"; shuten="東銀座"; keiyu="日比谷線"; kyori=0.4; jikan=1};
  {kiten="東銀座"; shuten="築地"; keiyu="日比谷線"; kyori=0.6; jikan=2};
  {kiten="築地"; shuten="八丁堀"; keiyu="日比谷線"; kyori=1.; jikan=2};
  {kiten="八丁堀"; shuten="茅場町"; keiyu="日比谷線"; kyori=0.5; jikan=1};
  {kiten="茅場町"; shuten="人形町"; keiyu="日比谷線"; kyori=0.9; jikan=2};
  {kiten="人形町"; shuten="小伝馬町"; keiyu="日比谷線"; kyori=0.6; jikan=1};
  {kiten="小伝馬町"; shuten="秋葉原"; keiyu="日比谷線"; kyori=0.9; jikan=2};
  {kiten="秋葉原"; shuten="仲御徒町"; keiyu="日比谷線"; kyori=1.; jikan=1};
  {kiten="仲御徒町"; shuten="上野"; keiyu="日比谷線"; kyori=0.5; jikan=1};
  {kiten="上野"; shuten="入谷"; keiyu="日比谷線"; kyori=1.2; jikan=2};
  {kiten="入谷"; shuten="三ノ輪"; keiyu="日比谷線"; kyori=1.2; jikan=2};
  {kiten="三ノ輪"; shuten="南千住"; keiyu="日比谷線"; kyori=0.8; jikan=2};
  {kiten="南千住"; shuten="北千住"; keiyu="日比谷線"; kyori=1.8; jikan=3};
  {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3};
  {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2};
  {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2};
  {kiten="後楽園"; shuten="本郷三丁目"; keiyu="丸ノ内線"; kyori=0.8; jikan=1};
  {kiten="本郷三丁目"; shuten="御茶ノ水"; keiyu="丸ノ内線"; kyori=0.8; jikan=1};
  {kiten="御茶ノ水"; shuten="淡路町"; keiyu="丸ノ内線"; kyori=0.8; jikan=1};
  {kiten="淡路町"; shuten="大手町"; keiyu="丸ノ内線"; kyori=0.9; jikan=2};
  {kiten="大手町"; shuten="東京"; keiyu="丸ノ内線"; kyori=0.6; jikan=1};
  {kiten="東京"; shuten="銀座"; keiyu="丸ノ内線"; kyori=1.1; jikan=2};
  {kiten="銀座"; shuten="霞ヶ関"; keiyu="丸ノ内線"; kyori=1.0; jikan=2};
  {kiten="霞ヶ関"; shuten="国会議事堂前"; keiyu="丸ノ内線"; kyori=0.7; jikan=1};
  {kiten="国会議事堂前"; shuten="赤坂見附"; keiyu="丸ノ内線"; kyori=0.9; jikan=2};
  {kiten="赤坂見附"; shuten="四ツ谷"; keiyu="丸ノ内線"; kyori=1.3; jikan=2};
  {kiten="四ツ谷"; shuten="四谷三丁目"; keiyu="丸ノ内線"; kyori=1.0; jikan=2};
  {kiten="四谷三丁目"; shuten="新宿御苑前"; keiyu="丸ノ内線"; kyori=0.9; jikan=1};
  {kiten="新宿御苑前"; shuten="新宿三丁目"; keiyu="丸ノ内線"; kyori=0.7; jikan=1};
  {kiten="新宿三丁目"; shuten="新宿"; keiyu="丸ノ内線"; kyori=0.3; jikan=1};
  {kiten="新宿"; shuten="西新宿"; keiyu="丸ノ内線"; kyori=0.8; jikan=1};
  {kiten="西新宿"; shuten="中野坂上"; keiyu="丸ノ内線"; kyori=1.1; jikan=2};
  {kiten="中野坂上"; shuten="新中野"; keiyu="丸ノ内線"; kyori=1.1; jikan=2};
  {kiten="新中野"; shuten="東高円寺"; keiyu="丸ノ内線"; kyori=1.0; jikan=1};
  {kiten="東高円寺"; shuten="新高円寺"; keiyu="丸ノ内線"; kyori=0.9; jikan=1};
  {kiten="新高円寺"; shuten="南阿佐ヶ谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2};
  {kiten="南阿佐ヶ谷"; shuten="荻窪"; keiyu="丸ノ内線"; kyori=1.5; jikan=2};
  {kiten="中野坂上"; shuten="中野新橋"; keiyu="丸ノ内線"; kyori=1.3; jikan=2};
  {kiten="中野新橋"; shuten="中野富士見町"; keiyu="丸ノ内線"; kyori=0.6; jikan=1};
  {kiten="中野富士見町"; shuten="方南町"; keiyu="丸ノ内線"; kyori=1.3; jikan=2};
  {kiten="市ヶ谷"; shuten="四ツ谷"; keiyu="南北線"; kyori=1.0; jikan=2};
  {kiten="四ツ谷"; shuten="永田町"; keiyu="南北線"; kyori=1.3; jikan=3};
  {kiten="永田町"; shuten="溜池山王"; keiyu="南北線"; kyori=0.9; jikan=1};
  {kiten="溜池山王"; shuten="六本木一丁目"; keiyu="南北線"; kyori=0.9; jikan=2};
  {kiten="六本木一丁目"; shuten="麻布十番"; keiyu="南北線"; kyori=1.2; jikan=2};
  {kiten="麻布十番"; shuten="白金高輪"; keiyu="南北線"; kyori=1.3; jikan=2};
  {kiten="白金高輪"; shuten="白金台"; keiyu="南北線"; kyori=1.0; jikan=2};
  {kiten="白金台"; shuten="目黒"; keiyu="南北線"; kyori=1.3; jikan=2};
  {kiten="市ヶ谷"; shuten="飯田橋"; keiyu="南北線"; kyori=1.1 ; jikan=2};
  {kiten="飯田橋"; shuten="後楽園"; keiyu="南北線"; kyori=1.4 ; jikan=2};
  {kiten="後楽園"; shuten="東大前"; keiyu="南北線"; kyori=1.3 ; jikan=3};
  {kiten="東大前"; shuten="本駒込"; keiyu="南北線"; kyori=0.9 ; jikan=2};
  {kiten="本駒込"; shuten="駒込"; keiyu="南北線"; kyori=1.4; jikan=2};
  {kiten="駒込"; shuten="西ヶ原"; keiyu="南北線"; kyori=1.4; jikan=2};
  {kiten="西ヶ原"; shuten="王子"; keiyu="南北線"; kyori=1.0; jikan=2};
  {kiten="王子"; shuten="王子神谷"; keiyu="南北線"; kyori=1.2; jikan=2};
  {kiten="王子神谷"; shuten="志茂"; keiyu="南北線"; kyori=1.6; jikan=3};
  {kiten="志茂"; shuten="赤羽岩淵"; keiyu="南北線"; kyori=1.1; jikan=2};
  {kiten="西船橋" ; shuten="原木中山"; keiyu="東西線"; kyori=1.9; jikan=3};
  {kiten="原木中山"; shuten="妙典"; keiyu="東西線"; kyori=2.1 ; jikan=2};
  {kiten="妙典"; shuten="行徳"; keiyu="東西線"; kyori=1.3 ; jikan=2};
  {kiten="行徳"; shuten="南行徳"; keiyu="東西線"; kyori=1.5 ; jikan=2};
  {kiten="南行徳"; shuten="浦安" ; keiyu="東西線"; kyori=1.2 ; jikan=2};
  {kiten="浦安" ; shuten="葛西"; keiyu="東西線"; kyori=1.9 ; jikan=2};
  {kiten="葛西"; shuten="西葛西"; keiyu="東西線"; kyori=1.2 ; jikan=2};
  {kiten="西葛西"; shuten="南砂町"; keiyu="東西線"; kyori=2.7 ; jikan=2};
  {kiten="南砂町"; shuten="東陽町"; keiyu="東西線"; kyori=1.2 ; jikan=2};
  {kiten="東陽町"; shuten="木場" ; keiyu="東西線"; kyori=0.9 ; jikan=1};
  {kiten="木場"; shuten="門前仲町"; keiyu="東西線"; kyori=1.1 ; jikan=1};
  {kiten="門前仲町"; shuten="茅場町"; keiyu="東西線"; kyori=1.8 ; jikan=2};
  {kiten="茅場町"; shuten="日本橋"; keiyu="東西線"; kyori=0.5 ; jikan=1};
  {kiten="日本橋"; shuten="大手町"; keiyu="東西線"; kyori=0.8 ; jikan=1};
  {kiten="大手町"; shuten="竹橋"; keiyu="東西線"; kyori=1.0; jikan=2};
  {kiten="竹橋"; shuten="九段下"; keiyu="東西線"; kyori=1.0; jikan=1};
  {kiten="九段下"; shuten="飯田橋"; keiyu="東西線"; kyori=0.7; jikan=1};
  {kiten="飯田橋"; shuten="神楽坂"; keiyu="東西線"; kyori=1.2; jikan=2};
  {kiten="神楽坂"; shuten="早稲田"; keiyu="東西線"; kyori=1.2; jikan=2};
  {kiten="早稲田"; shuten="高田馬場"; keiyu="東西線"; kyori=1.7; jikan=3};
  {kiten="高田馬場"; shuten="落合"; keiyu="東西線"; kyori=1.9; jikan=3};
  {kiten="落合"; shuten="中野"; keiyu="東西線"; kyori=2.0; jikan=3};
  {kiten="新木場"; shuten="辰巳"; keiyu="有楽町線"; kyori=1.5; jikan=2};
  {kiten="辰巳"; shuten="豊洲"; keiyu="有楽町線"; kyori=1.7; jikan=2};
  {kiten="豊洲"; shuten="月島"; keiyu="有楽町線"; kyori=1.4; jikan=2};
  {kiten="月島"; shuten="新富町"; keiyu="有楽町線"; kyori=1.3; jikan=2};
  {kiten="新富町"; shuten="銀座一丁目"; keiyu="有楽町線"; kyori=0.7; jikan=1};
  {kiten="銀座一丁目"; shuten="有楽町"; keiyu="有楽町線"; kyori=0.5; jikan=1};
  {kiten="有楽町"; shuten="桜田門"; keiyu="有楽町線"; kyori=1.0; jikan=1};
  {kiten="桜田門"; shuten="永田町"; keiyu="有楽町線"; kyori=0.9; jikan=2};
  {kiten="永田町"; shuten="麹町"; keiyu="有楽町線"; kyori=0.9; jikan=1};
  {kiten="麹町"; shuten="市ヶ谷"; keiyu="有楽町線"; kyori=0.9; jikan=1};
  {kiten="市ヶ谷"; shuten="飯田橋"; keiyu="有楽町線"; kyori=1.1; jikan=2};
  {kiten="飯田橋"; shuten="江戸川橋"; keiyu="有楽町線"; kyori=1.6; jikan=3};
  {kiten="江戸川橋"; shuten="護国寺"; keiyu="有楽町線"; kyori=1.3; jikan=2};
  {kiten="護国寺"; shuten="東池袋"; keiyu="有楽町線"; kyori=1.1; jikan=2};
  {kiten="東池袋"; shuten="池袋"; keiyu="有楽町線"; kyori=2.0; jikan=2};
  {kiten="池袋"; shuten="要町"; keiyu="有楽町線"; kyori=1.2; jikan=2};
  {kiten="要町"; shuten="千川"; keiyu="有楽町線"; kyori=1.0; jikan=1};
  {kiten="千川"; shuten="小竹向原"; keiyu="有楽町線"; kyori=1.0; jikan=2};
  {kiten="小竹向原"; shuten="氷川台"; keiyu="有楽町線"; kyori=1.5; jikan=2};
  {kiten="氷川台"; shuten="平和台"; keiyu="有楽町線"; kyori=1.4; jikan=2};
  {kiten="平和台"; shuten="営団赤塚"; keiyu="有楽町線"; kyori=1.8; jikan=2};
  {kiten="営団赤塚"; shuten="営団成増"; keiyu="有楽町線"; kyori=1.5; jikan=2};
  {kiten="営団成増"; shuten="和光市"; keiyu="有楽町線"; kyori=2.1; jikan=3};
]

let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list

let rec search eki_tree ekimei1 = match eki_tree with
  Empty -> []
| Node (t1, n, t2) -> match n with (n_ekimei, eki_list) ->
  if ekimei1 = n_ekimei then eki_list
  else if ekimei1 < n_ekimei then search t1 ekimei1
  else search t2 ekimei1

(* テスト *)
let test50 = search global_ekikan_tree "六本木"
= [("広尾", 1.7); ("神谷町", 1.5)]

let new_get_ekikan_kyori ekimei1 ekimei2 ekikan_tree =
  let eki_list = search ekikan_tree ekimei1 in
    let ekikan = List.find (fun (ekimei, _) -> ekimei = ekimei2) eki_list in
      match ekikan with (_, kyori) -> kyori

(* テスト *)
(* let test1 = new_get_ekikan_kyori "佐賀市" "佐賀市" global_ekikan_tree = infinity *)
let test102 = new_get_ekikan_kyori "平和台" "営団赤塚" global_ekikan_tree = 1.8
let test103 = new_get_ekikan_kyori "営団成増" "和光市" global_ekikan_tree = 2.1
let test104 = new_get_ekikan_kyori "六本木" "神谷町" global_ekikan_tree = 1.5

(* --- END: dijkstra を tree を使った実装に書き直すためのコード --- *)

let rec get_ekikan_kyori eki1 eki2 ekikan_list = match ekikan_list with
    [] -> infinity
  | {kiten = k; shuten = s; keiyu = _; kyori = kyo; jikan = _} :: rest ->
    if eki1 = k && eki2 = s || eki1 = s && eki2 = k then kyo
    else get_ekikan_kyori eki1 eki2 rest

(* 目的: 直前に確定した駅 p (eki_t 型) と未確定の駅 q (eki_t 型) のリストを受け取って eki_t 型のリストを返す *)
(* FIXME: ここ直前に確定した駅の距離を足す必要がある気がする *)
let old_koushin eki eki_list g_ekikan_list = let koushin1 p q = match p with
  {namae = p_n; saitan_kyori = p_kyori; temae_list = p_t} -> match q with
  {namae = q_n; saitan_kyori = q_k; temae_list = q_t} ->
    let q_kyori = get_ekikan_kyori p_n q_n g_ekikan_list in
      let total_kyori = q_kyori +. p_kyori in
      if q_kyori = infinity || total_kyori >= q_k then q
      else {namae = q_n; saitan_kyori = total_kyori; temae_list = q_n :: p_t} in
        List.map (koushin1 eki) eki_list

let koushin eki eki_list g_ekikan_tree = let koushin1 p q = match p with
  {namae = p_n; saitan_kyori = p_kyori; temae_list = p_t} -> match q with
  {namae = q_n; saitan_kyori = q_k; _} ->
    let q_kyori = try new_get_ekikan_kyori p_n q_n g_ekikan_tree with Not_found -> q_k in
      let total_kyori = q_kyori +. p_kyori in
      if total_kyori >= q_k then q
      else {namae = q_n; saitan_kyori = total_kyori; temae_list = q_n :: p_t} in
        List.map (koushin1 eki) eki_list

(* let old_saitan_wo_bunri lst = match lst with
    [] -> ({namae = ""; saitan_kyori = infinity; temae_list = []}, [])
  | first :: rest -> let comp_eki first_eki rest_result = match first_eki with
    {namae = _; saitan_kyori = f_s; temae_list = _} -> match rest_result with
      (rest_eki, rest_list) -> match rest_eki with
      {namae = _; saitan_kyori = r_s; temae_list = _} ->
        if f_s < r_s then (first_eki, rest_eki :: rest_list)
        else (rest_eki, first_eki :: rest_list) in
          List.fold_right comp_eki rest (first, []) *)

let new_saitan_wo_bunri first rest = let comp_eki first_eki rest_result = match first_eki with
  {saitan_kyori = f_s; _} -> match rest_result with
    (rest_eki, rest_list) -> match rest_eki with
    {saitan_kyori = r_s; _} ->
      if f_s < r_s then (first_eki, rest_eki :: rest_list)
      else (rest_eki, first_eki :: rest_list) in
        List.fold_right comp_eki rest (first, [])

(* 目的: ダイクストラのアルゴリズムを実装する *)
(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main eki_list ekikan_list = match eki_list with
    [] -> []
  | first :: rest -> let bunri_pair = new_saitan_wo_bunri first rest in
    match bunri_pair with (bunri_eki, bunri_eki_list) ->
      let updated_list = koushin bunri_eki bunri_eki_list ekikan_list in
        bunri_eki :: dijkstra_main updated_list ekikan_list

let test201 = dijkstra_main [
  {namae = "代々木上原"; saitan_kyori = 0.; temae_list = ["代々木上原"]};
  {namae = "代々木公園"; saitan_kyori = infinity; temae_list = []};
  {namae = "明治神宮前"; saitan_kyori = infinity; temae_list = []};
  {namae = "表参道"; saitan_kyori = infinity; temae_list = []}]
  global_ekikan_tree
= [
  {namae = "代々木上原"; saitan_kyori = 0.; temae_list = ["代々木上原"]};
  {namae = "代々木公園"; saitan_kyori = 1.0; temae_list = ["代々木公園"; "代々木上原"]};
  {namae = "明治神宮前"; saitan_kyori = 2.2; temae_list = ["明治神宮前"; "代々木公園"; "代々木上原"]};
  {namae = "表参道"; saitan_kyori = 3.1; temae_list = ["表参道"; "明治神宮前"; "代々木公園"; "代々木上原"]}
]

(* --- ダイクストラの完全実装 ---

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

(* 目的: 昇順に並んでいるリスト lst1 と lst2 をマージする *)
(* romaji_to_kanji : string -> int list -> string *)
let rec romaji_to_kanji romaji ekimei_list = match ekimei_list with
    [] -> ""
  | first :: rest -> match first with
  {kanji = k; kana = _; romaji = r; shozoku = _} ->
  if romaji = r then k
  else romaji_to_kanji romaji rest

(* make_initial_eki_list : ekimei_t list string *)
let make_initial_eki_list ekimei_list kiten = let shoki ekimei = match ekimei with
  {kanji = k; kana = _; romaji = _; shozoku = _} ->
    if k = kiten then {namae = k; saitan_kyori = 0.; temae_list = k :: []}
    else {namae = k; saitan_kyori = infinity; temae_list = []} in
      List.map shoki ekimei_list

let dijkstra shiten shuten =
  let g_ekimei_list = seiretsu global_ekimei_list in
  let shiten_kanji = romaji_to_kanji shiten g_ekimei_list in
  let shuten_kanji = romaji_to_kanji shuten g_ekimei_list in
  let init_eki_list = make_initial_eki_list g_ekimei_list shiten_kanji in
  let after_dijkstra = dijkstra_main init_eki_list global_ekikan_list in
  List.find (fun {namae = n; saitan_kyori = _; temae_list = _} -> shuten_kanji = n) after_dijkstra

let test1 = dijkstra "yoyogiuehara" "ueno"
let test2 = dijkstra "akasaka" "ueno" *)
