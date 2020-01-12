type tree_t = Empty
            | Leaf of int
            | Node of tree_t * int * tree_t

let test1 = Empty
let test2 = Leaf (2)
let test3 = Leaf (100)
let test4 = Node (Empty, 100, Leaf 3)
