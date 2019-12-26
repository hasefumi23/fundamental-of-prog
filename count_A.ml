(* 学生一人分のデータ(名前、点数、成績)を表す型 *)
type gakusei_t = {
  namae: string;
  tensuu: int;
  seiseki: string;
}

(* gakusei_t list は
    - [] からリスト、あるいは
    - first :: rest 最初の要素が first で残りのリストが rest
                    (first は gakusei_t 型、 rest が自己参照のケース)という形
*)

(* gakusei_t list 型のデータの例 *)
let lst1 = []
let lst2 = [{namae = "asai"; tensuu = 70; seiseki = "B"}]
let lst3 = [{namae = "asai"; tensuu = 70; seiseki = "B"};
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}]

(* 目的: 学生リスト lst のうち成績が A の人の数を返す *)
(* count_A : gakusei_t list -> int *)
let rec count_A lst = match lst with
    [] -> 0
  | {namae = _; tensuu = _; seiseki = s} ::rest
         -> if s = "A" then 1 + count_A rest else count_A rest

(* テスト *)
let test1 = count_A lst1 = 0
let test2 = count_A lst2 = 0
let test3 = count_A lst3 = 1
