(*
 * SNU 4190.310 Programming Languages 
 * Homework "Continuation Passing Style" Skeleton
 *)

open M0

let count = ref 0

let new_name () = 
  let _ = count := !count + 1 in
  "x_" ^ (string_of_int !count)

let rec alpha_conv exp subst = 
  match exp with
  | Num n -> Num n
  | Var x -> (try Var (List.assoc x subst) with Not_found -> Var x)
  | Fn (x, e) ->
    let x' = new_name () in
    let subst' = (x, x') :: subst in
    Fn (x', alpha_conv e subst')
  | App (e1, e2) -> App (alpha_conv e1 subst, alpha_conv e2 subst)
  | Rec (f, x, e) -> 
    let x' = new_name () in
    let f' = new_name () in
    let subst' = (f, f') :: (x, x') :: subst in
    Rec (f', x', alpha_conv e subst')
  | Ifz (e1, e2, e3) -> 
    Ifz (alpha_conv e1 subst, alpha_conv e2 subst, alpha_conv e3 subst)
  | Add (e1, e2) -> Add (alpha_conv e1 subst, alpha_conv e2 subst)
  | Pair (e1, e2) -> Pair (alpha_conv e1 subst, alpha_conv e2 subst)
  | Fst e -> Fst (alpha_conv e subst)
  | Snd e -> Snd (alpha_conv e subst)

(* TODO : Complete this function *)
let rec cps' exp = 
  let k = new_name () in
  match exp with
  (* Constant expressions *)
  | Num n -> Fn (k, App(Var k, Num n) (* Fill in here *) )
  | Var x -> Fn (k, App(Var k, Var x)(* Fill in here *) )
  | Fn (x, e) -> 
    let k1 = new_name() in
    Fn (k, App(Var k, Fn(k1, Fn(x, App(cps' e, Var k1))))(* Fill in here *) )
  | Rec (f, x, e) -> 
    let k1 = new_name() in
    Fn (k, App(Var k, Rec(f, k1, Fn(x, App(cps' e, Var k1))))(* Fill in here *) )
  (* Non constant expressions *)
  | App (e1, e2) -> 
    let v1 = new_name() in
    let v2 = new_name() in
    Fn (k, App(cps' e1, Fn(v1, App(cps' e2, Fn(v2, App(App(Var v1, Var k), Var v2)))))(* Fill in here *) )
  | Ifz (e1, e2, e3) -> 
    let k1 = new_name() in
    let k2 = new_name() in
    let k3 = new_name() in
    Fn (k, App(cps' e1, Fn(k1, Ifz(Var k1, 
    App(cps' e2, Fn(k2, App(Var k, Var k2))), App(cps' e3, Fn(k3, App(Var k, Var k3))))))(* Fill in here *) )
  | Add (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k, 
        App (cps' e1, 
            Fn (v1, 
                App (cps' e2, 
                    Fn (v2, 
                        App (Var k, Add (Var v1, Var v2))
                        )
                    )
                )
            )
        )
  | Pair (e1, e2) -> 
    let v = new_name() in
    let w = new_name() in
    Fn (k, App(cps' e1, Fn(v, App(cps' e2, Fn(w, App(Var k, Pair(Var v, Var w))))))(* Fill in here *) )
  | Fst e ->  
    let x = new_name() in
    Fn (k, App(cps' e, Fn (x, App(Var k, Fst(Var x))))(* Fill in here *) )
  | Snd e ->  
    let x = new_name() in
    Fn (k, App(cps' e, Fn (x, App(Var k, Snd(Var x))))(* Fill in here *) )

let cps exp = cps' (alpha_conv exp [])

