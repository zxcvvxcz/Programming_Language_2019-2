type ae = CONST of int
    | VAR of string
    | POWER of string * int
    | TIMES of ae list
    | SUM of ae list

exception InvalidArgument

let rec diff : ae * string -> ae = fun (ex, str) ->
    match ex with
        | CONST i -> CONST 0
        | VAR s -> if (compare s str = 0) then CONST 1 
            else CONST 0
        | POWER (s, i) -> (if (compare s str = 0 ) then 
                (if i = 0 then CONST 0
                else if i = 1 then CONST 1
                else if i = 2 then TIMES([CONST 2; VAR s])
                else TIMES([CONST i; POWER (s, i - 1)])
                )
            else CONST 0)
        | SUM el -> (match el with
            | [] -> raise InvalidArgument
            | hd :: tl -> if (List.length tl > 0)
                then SUM (diff (hd, str) :: diff (SUM tl, str) :: [])
                else diff (hd, str))
        | TIMES el -> (match el with
            | [] -> raise InvalidArgument
            | hd :: tl -> if hd = CONST 0 then CONST 0
                else if (List.length tl > 0) 
                then SUM (TIMES (diff (hd, str) :: tl) :: (TIMES (hd :: diff ((TIMES tl), str) :: [])) :: [])
                else diff (hd, str))
(*
let d0 = diff (TIMES [SUM [(TIMES [CONST 2; VAR "X"]); CONST 1]; SUM [(TIMES [CONST 2; POWER ("X", 1)]); CONST 1]], "X");;
let d1 = diff (CONST 4, "4")
let d2 = diff (VAR "4", "4")
let d3 = diff (VAR "4", "X")
let d4 = diff (POWER ("X", 0), "X")
let d5 = diff (POWER ("X", 1), "X")
let d6 = diff (POWER ("X", 2), "X")
let d7 = diff (POWER ("X", 3), "X")
let d8 = diff (POWER ("X", 4), "X")
let d9 = diff (POWER ("X", 4), "Y")
let d10 = diff (POWER ("XY", 4), "X")
let d11 = diff (SUM [POWER ("X", 1);POWER ("X", 4)], "X")
let d12 = diff (TIMES [POWER ("X", 1);POWER ("X", 4)], "X")
let d13 = diff (TIMES [SUM [POWER ("X", 1);POWER ("X", 4)];POWER ("X", 4)], "X")
*)