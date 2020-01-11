(* 目的: fold_left を実装する *)
(* fold_left : f init lst -> 'a *)
let rec fold_left f init lst = match lst with
    [] -> init
  | first :: rest -> fold_left f first (f init rest)
