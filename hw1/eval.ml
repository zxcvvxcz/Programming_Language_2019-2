type formula = TRUE | FALSE
            | NOT of formula 
            | ANDALSO of formula * formula
            | ORELSE of formula * formula
            | IMPLY of formula * formula
            | LESS of expr * expr 
and expr = NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr

let rec calc : expr -> int = fun e ->
    match e with
    |NUM i -> i
    |PLUS (e1, e2) -> calc (e1) + calc (e2)
    |MINUS (e1, e2) -> calc (e1) - calc (e2)

let rec eval : formula -> bool = fun l ->
    match l with
    | TRUE -> true
    | FALSE -> false
    | NOT l1 -> not (eval (l1))
    | ANDALSO (l1, l2) -> (eval l1) && (eval l2)
    | ORELSE (l1, l2) -> (eval l1) || (eval l2)
    | IMPLY (l1, l2) -> (if (eval l1) && (not (eval l2)) then false else true)
    | LESS (e1, e2) -> (calc (e1) <= calc (e2))
(*
let _ = print_endline (string_of_bool (eval FALSE));;
let _ = print_endline (string_of_bool (eval (NOT TRUE)));;
let _ = print_endline (string_of_bool (eval(ANDALSO (TRUE, TRUE))));;
let _ = print_endline (string_of_bool (eval(ANDALSO (FALSE, TRUE))));;
let _ = print_endline (string_of_bool (eval(ANDALSO (FALSE, FALSE))));;
let _ = print_endline (string_of_bool (eval(ORELSE (FALSE, TRUE))));;
let _ = print_endline (string_of_bool (eval(ORELSE (TRUE, TRUE))));;
let _ = print_endline (string_of_bool (eval(ORELSE (FALSE, FALSE))));;
let _ = print_endline (string_of_bool (eval(IMPLY (TRUE, TRUE))));;
let _ = print_endline (string_of_bool (eval(IMPLY (FALSE, TRUE))));;
let _ = print_endline (string_of_bool (eval(IMPLY (TRUE, FALSE))));;
let _ = print_endline (string_of_bool (eval(IMPLY (FALSE, FALSE))));;
let _ = print_endline (string_of_bool (eval(LESS (NUM 2, NUM 3))));;
let _ = print_endline (string_of_bool (eval(LESS (NUM 4, NUM 3))));;
let _ = print_endline (string_of_bool (eval(LESS (PLUS (NUM 4, NUM 2), MINUS (NUM 5, NUM 3)))));;

*)

let _ = eval (ORELSE (LESS (PLUS (MINUS (NUM 3, NUM 2), NUM 9), NUM 10), FALSE))