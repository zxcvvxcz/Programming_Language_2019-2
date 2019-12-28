type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h = match h with
    | EMPTY -> -1
    | NODE(r, _, _, _) -> r
let findMin h = match h with
    | EMPTY -> raise EmptyHeap
    | NODE(_, x, _, _) -> x
let shake (x, lh, rh) = if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh)

let rec merge: heap * heap -> heap = fun (h1, h2) -> match h1 with
    | EMPTY -> h2
    | NODE (r1, v1, lh1, rh1) -> ( match h2 with
        | EMPTY -> h1
        | NODE (r2, v2, lh2, rh2) -> (
            if v1 < v2 then (* h1 is root*)
                shake (v1, lh1, merge (rh1, h2))
            else    (*h2 is root*) 
                shake (v2, lh2, merge (rh2, h1))
        )
    )
    
let insert(x, h) = merge(h, NODE(0, x, EMPTY, EMPTY))
let deleteMin h = match h with
    | EMPTY -> raise EmptyHeap
    | NODE(_, x, lh, rh) -> merge (lh, rh)

(*
let my_heap0 = EMPTY
let my_heap1 = insert(1, my_heap0)
let rank1 = rank (my_heap1)
let min1 = findMin (my_heap1)
let my_heap2 = insert(2, my_heap1)
let rank2 = rank (my_heap2)
let min2 = findMin (my_heap2)
let my_heap3 = insert(3, my_heap2)
let rank3 = rank (my_heap3)
let min3 = findMin (my_heap3)
let my_heap4 = insert(4, my_heap3)
let rank4 = rank (my_heap4)
let min4 = findMin (my_heap4)
let my_heap5 = insert(5, my_heap4)
let rank5 = rank (my_heap5)
let min5 = findMin (my_heap5)
let my_heap6 = insert(6, my_heap5)
let rank6 = rank (my_heap6)
let min6 = findMin (my_heap6)
let my_heap7 = insert(7, my_heap6)
let rank7 = rank (my_heap7)
let min7 = findMin (my_heap7)
let my_heap8 = insert(8, my_heap7)
let rank8 = rank (my_heap8)
let min8 = findMin (my_heap8)
let my_heap9 = deleteMin (my_heap8)
let rank9 = rank (my_heap8)
let min9 = findMin (my_heap8)
let my_heap10 = deleteMin (my_heap9)
let rank10 = rank (my_heap9)
let min10 = findMin (my_heap9)
*)