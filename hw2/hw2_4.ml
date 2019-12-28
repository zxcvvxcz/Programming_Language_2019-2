module type Queue =
sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
end

module IntListQ =
struct
    type element = int list
    type queue = (int list) list * (int list) list
    exception EMPTY_Q
    let emptyQ : queue = ([], [])
    let enQ : queue * element -> queue = fun (que, el) -> 
        ([el] @ (fst que), (snd que))
    let deQ : queue -> element * queue = fun que -> if List.length (snd que) > 0 
        then (List.hd (snd que), ((fst que), List.tl (snd que)))
        else (
            if List.length (fst que) = 0 then raise EMPTY_Q
            else (List.hd (List.rev (fst que)), ([], List.tl (List.rev (fst que))))
        )
end
(*
module ValidIntListQ = (IntListQ : Queue)

let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1;2;3])
let yourQ = IntListQ.enQ(yourQ, [4;5])
let yourQ = IntListQ.enQ(yourQ, [6;])
let yourQ = IntListQ.enQ(yourQ, [9;7;8;6])
let yourQ = IntListQ.enQ(yourQ, [4;5])
let (x1,restQ1) = IntListQ.deQ yourQ
let (x2,restQ2) = IntListQ.deQ restQ1
let (x3,restQ3) = IntListQ.deQ restQ2
let (x4,restQ4) = IntListQ.deQ restQ3
let (x5,restQ5) = IntListQ.deQ restQ4
let yourQ1 = IntListQ.enQ(restQ1, [4;5])
let yourQ = IntListQ.enQ(yourQ, [1;2;3])
let hisQ = IntListQ.enQ(myQ, [2])

*)