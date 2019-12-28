type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
    | Poland | Portugal | Italy | Germany | Norway | Sweden | England
    | Argentina
type tourna = LEAF of team
    | NODE of tourna * tourna

let team_to_string : team -> string = fun t ->
    match t with
    | Korea -> "Korea"   | France -> "France" | Usa -> "Usa"   | Brazil -> "Brazil" | Japan -> "Japan"
    | Nigeria -> "Nigeria"  | Cameroon -> "Cameroon"    | Poland -> "Poland"    | Portugal -> "Portugal"
    | Italy -> "Italy"  | Germany -> "Germany"  | Norway -> "Norway"    | Sweden -> "Sweden"
    | England -> "England"  | Argentina -> "Argentina"
let rec parenize : tourna -> string = fun t ->
    match t with
    |LEAF l -> team_to_string l
    |NODE (t1, t2) -> "(" ^ (parenize t1) ^ " " ^ (parenize t2) ^ ")"
(*
let _ = print_endline (parenize(NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)))
let _ = print_endline (parenize(NODE(NODE(LEAF Korea, NODE(LEAF Portugal, LEAF Germany)), LEAF Brazil)))
*)