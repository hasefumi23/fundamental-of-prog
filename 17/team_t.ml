type team_t = Red | White
let team_string team = match team with
    Red -> "red"
  | White -> "white"

type nengou_t = Meiji of int
              | Taisho of int
              | Showa of int
              | Heisei of int
              | Reiwa of int

let test1 = Showa (42)

let to_seireki nengou = match nengou with
    Meiji (n) -> n + 1867
  | Taisho (n) -> n + 1911
  | Showa (n) -> n + 1925
  | Heisei (n) -> n + 1988
  | Reiwa (n) -> n + 2019

let test2 = to_seireki (Meiji (20))
let test3 = to_seireki (Taisho (20))
let test4 = to_seireki (Showa (20))
let test5 = to_seireki (Heisei (20))
let test6 = to_seireki (Reiwa (20))
