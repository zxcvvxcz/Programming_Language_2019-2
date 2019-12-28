type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun n ->
    match n with
    | NIL -> 0
    | ZERO c -> 2 * (crazy2val c)
    | ONE c -> 1 + 2 * (crazy2val c)
    | MONE c -> -1 + 2 * (crazy2val c)
(*
let _ = print_endline (string_of_int (crazy2val (NIL)))
let _ = print_endline (string_of_int (crazy2val (ZERO(ONE(MONE NIL))))) (*0+-*)
let _ = print_endline (string_of_int (crazy2val (ONE(MONE(ONE(MONE NIL)))))) (*+-+-*)
let _ = print_endline (string_of_int (crazy2val (MONE(MONE(ONE(ZERO NIL))))))
let _ = print_endline (string_of_int (crazy2val (MONE(MONE(ONE(NIL))))))
let _ = print_endline (string_of_int (crazy2val (ZERO(ZERO(ONE(NIL))))))
let _ = print_endline (string_of_int (crazy2val (ZERO(ZERO(MONE(NIL))))))
let _ = print_endline (string_of_int (crazy2val (ZERO(ZERO(ZERO(NIL))))))
*)