
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun n ->
    match n with
    | NIL -> 0
    | ZERO c -> 2 * (crazy2val c)
    | ONE c -> 1 + 2 * (crazy2val c)
    | MONE c -> -1 + 2 * (crazy2val c)

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (z, z') ->
    match z with
    | NIL -> z'
    | ZERO c -> (match z' with
        | NIL -> z
        | ZERO c' -> ZERO (crazy2add (c,c'))
        | ONE c' -> ONE (crazy2add (c,c'))
        | MONE c' -> MONE (crazy2add (c,c'))
    )
    | ONE c -> (match z' with
        | NIL -> z
        | ZERO c' -> ONE (crazy2add (c, c'))
        | ONE c' -> ZERO (crazy2add (c, crazy2add (ONE NIL, c')))
        | MONE c' -> ZERO (crazy2add (c, c'))
    )
    | MONE c -> (match z' with
        | NIL -> z
        | ZERO c' -> MONE (crazy2add (c, c'))
        | ONE c' -> ZERO (crazy2add (c, c'))
        | MONE c' -> ZERO (crazy2add (c, crazy2add (MONE NIL, c')))
    ) 
(*
let _ = print_endline (string_of_int (crazy2val (crazy2add (ONE(ONE(ONE NIL)), ONE(ONE(ONE(ONE NIL)))))))
let _ = print_endline (string_of_int (crazy2val (crazy2add (ONE(ONE(MONE NIL)), ONE(ONE(MONE(ONE NIL)))))))
*)