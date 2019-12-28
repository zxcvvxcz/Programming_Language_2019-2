type expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
    | MULT of expr * expr
    | DIVIDE of expr * expr
    | MAX of expr list

let rec eval : expr -> int = fun ex ->
    match ex with
    | NUM i -> i
    | PLUS (e1, e2) -> (eval e1) + (eval e2)
    | MINUS (e1, e2) -> (eval e1) - (eval e2)
    | MULT (e1, e2) -> (eval e1) * (eval e2)
    | DIVIDE (e1, e2) -> (eval e1) / (eval e2)
    | MAX el -> 
        if List.length el = 0 then 0
        else max_num el
        
and max_num : expr list -> int = fun il ->
        match il with
        | [] -> min_int
        | hd :: [] -> eval hd
        | hd :: tl -> 
            if (eval hd) > (max_num tl) then (eval hd)
            else (max_num tl)
    

let _ = print_endline (string_of_int (eval(NUM 1)))
let _ = print_endline (string_of_int (eval(PLUS (NUM 3, NUM 4))))
let _ = print_endline (string_of_int (eval(MINUS (NUM 3, NUM 4))))
let _ = print_endline (string_of_int (eval(MULT (NUM 3, NUM 4))))
let _ = print_endline (string_of_int (eval(DIVIDE (NUM 3, NUM 4))))

let _ = print_endline (string_of_int (eval(MAX [NUM (-3); NUM (-4); NUM (-1); PLUS (NUM (-1), NUM (-5))])))
let _ = print_endline (string_of_int (eval(DIVIDE (NUM 13, NUM (-7)))))
let _ = print_endline (string_of_int (eval(MAX [])));;