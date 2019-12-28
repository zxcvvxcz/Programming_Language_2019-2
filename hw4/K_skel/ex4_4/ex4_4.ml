type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure
| Branch of map * map
| Guide of string * map

exception IMPOSSIBLE

let getNode = fun n ->
match n with
| Bar -> Bar, Bar
| Node (k1, k2) -> k1, k2

let rec suggest : map -> key -> (string * key) list -> key * (string * key) list = fun map key sklist ->
match map with
| End t ->
    let (sl, tl) = List.split sklist in
   ( match t with
    | StarBox -> 
    if (List.mem "star" sl) then Bar, sklist else Bar, ("star", Bar) :: sklist
    | NameBox s -> 
    if (List.mem s sl) then (List.assoc s sklist), sklist else key, ((s, key) :: sklist)
    )
| Branch (m1, m2) ->
    if m1 = m2 then raise IMPOSSIBLE
    else (
        let a, sklist_a = suggest m2 key sklist in
        let c, sklist_b = key_check m1 (Node (key, a)) a sklist_a in (* c = (a,b) *)
        (snd (getNode c)), sklist_b
    )
| Guide (s, m) ->
    let sklist' = List.remove_assoc s sklist in
    let sklist'' = (s, fst (getNode key)) :: sklist' in
    Node (fst (getNode key), fst (suggest m key sklist'')), snd (suggest m key sklist'')
and key_check = fun map k1 k2 sklist ->
    let k', sklist' = suggest map k1 sklist in
    match k' with
    | Node (n1, n2) ->
    if n1 = k2 then (k', sklist') else (key_check map (Node (k1,k1)) k2 sklist)
    | Bar -> (key_check (map) (Node (Bar, Bar)) (k2) (sklist))
let rec list_last = fun klist res_list->
    match klist with
    | [] -> res_list
    | hd :: tl ->
        if (List.mem hd res_list) then list_last tl res_list else list_last tl (hd :: res_list)
let getReady = fun map ->
    let key, sklist = suggest map Bar [] in
    let sl, kl = List.split sklist in
    (list_last kl [])


(* Testcases *)

(* Print key type *)
let rec printK : key -> unit = fun k ->
    match k with
    | Bar -> print_string("-")
    | Node (k1, k2) ->
        print_string("(");
        printK(k1);
        print_string(",");
        printK(k2);
        print_string(")")
(* Print key list *)
let rec printKL : key list -> unit = fun kl ->
    match kl with
    | h::t ->
        printK(h);
        print_string("  ");
        printKL(t)
    | [] -> print_endline("")

let _ = printKL(getReady(End(NameBox "x")))
let _ = printKL(getReady(Guide("x", End(NameBox "x"))))
let _ = printKL(getReady(Branch(Guide("x", End(NameBox "x")), End(StarBox))))
let _ = printKL(getReady(Branch(Guide("x", End(NameBox "x")), Branch(Guide("y", End(NameBox "y")), End(StarBox)))))
let _ = printKL(getReady(Branch(Guide("x", End(NameBox "x")),Guide("y", End(NameBox "y")))))
let _ = printKL(getReady(Branch(End(NameBox "x"), End(StarBox))))
let _ = printKL(getReady(Branch(Branch(End(NameBox "x"), End(NameBox "y")), End(NameBox "y"))))
let _ = printKL(getReady(Guide("x", Guide("y", Branch(End(NameBox "x"), End(NameBox "y"))))))
let _ = printKL(getReady(Branch(Branch(End(NameBox "x"), End(StarBox)), Branch(End(NameBox "y"), End(NameBox "x")))))
(*let _ = printKL(getReady(Branch(Guide("x", Branch(End(NameBox "x"), End(NameBox "x"))), End(StarBox))))*)
(*let _ = printKL(getReady(Branch(Guide("x", Branch(End(NameBox "x"), End(NameBox "y"))), End(NameBox "y"))))*)
(*let _ = printKL(getReady(Branch(End(NameBox "x"), End(NameBox "x"))))*)
(*let _ = printKL(getReady(Branch(End(StarBox), End(StarBox))))*)
(*let _ = printKL(getReady(Branch(End(NameBox "x"), Guide("x", End(NameBox "x")))))*)
