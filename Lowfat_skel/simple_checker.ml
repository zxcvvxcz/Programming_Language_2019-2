(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)
  | TEq of var
  | TWrite of var
let rec conv2m : typ -> M.types = fun t ->
  match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (conv2m t1, conv2m t2)
  | TLoc t' -> M.TyLoc (conv2m t')
  | TFun (t1, t2) -> raise (M.TypeError "TFun is not a result type")
  | TVar v -> raise (M.TypeError ("TVar" ^ v ^ " is not a result type"))
  | _ -> raise (M.TypeError "Not a result type")

(* Definitions related to substitution *)

type typ_env = (M.id * typ) list
type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | TEq x' | TWrite x' | TVar x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_typ : subst -> typ -> typ = fun subs ty -> (subs ty)
let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, ty) -> (x, subst_typ subs ty)) tyenv

type tyEqn = (typ * typ) list
  
let rec v_algorithm : typ_env * M.exp * typ -> tyEqn = fun (env, exp, tau) -> match exp with
  | M.CONST c -> (match c with
    | M.S s-> (tau, TString) :: []
    | M.N n-> (tau, TInt) :: []
    | M.B b-> (tau, TBool) :: [])
  | M.VAR x -> if(List.exists (fun (m_id, _) -> m_id = x) env) then
    let (_, tau') = List.find (fun elem -> (fst elem) = x) env in
    (tau, tau') :: []
    else raise (M.TypeError (x ^ " is not declared"))
  | M.FN (x, e) ->
    let alpha1 = TVar (new_var ()) in
    let alpha2 = TVar (new_var ()) in
    (tau , TFun (alpha1, alpha2)) :: v_algorithm (((x, alpha1) :: env), e, alpha2)
  | M.APP (e1, e2) -> 
    let alpha = TVar (new_var ()) in
    v_algorithm (env, e1, TFun (alpha, tau)) @ v_algorithm (env, e2, alpha)
  | M.LET (dec, e) -> (match dec with
    | M.REC (id1, id2, e_rec) -> 
      let a1 = TVar (new_var ()) in
      let a2 = TVar (new_var ()) in
      let a3 = TVar (new_var ()) in
      (a3, TFun (a1, a2)) :: 
      v_algorithm ((id1, a3) :: (id2, a1) :: env, e_rec, a1) @ v_algorithm ((id1, a3) :: (id2, a1) ::env, e, tau)
    | M.VAL (x, e_val) -> 
      let alpha = TVar (new_var ()) in
      v_algorithm (env, e_val, alpha) @ v_algorithm ((x, alpha) :: env, e, tau))
  | M.IF (e1, e2, e3) ->
    v_algorithm (env, e1, TBool) @ v_algorithm (env, e2, tau) @ v_algorithm (env, e3, tau)
  | M.BOP (b, e1, e2) ->( match b with
    | M.ADD 
    | M.SUB -> (tau, TInt) :: v_algorithm (env, e1, TInt) @ v_algorithm (env, e2, TInt)
    | M.AND
    | M.OR -> (tau, TBool) :: v_algorithm (env, e1, TBool) @ v_algorithm (env, e2, TBool)
    | M.EQ -> 
      let a_eq = new_var () in
      (tau , TBool) :: v_algorithm (env, e1, TEq a_eq) @ v_algorithm (env, e2, TEq a_eq)
)
  | M.READ -> (tau, TInt) :: []
  | M.WRITE e ->
    let a_write = new_var () in
    (tau, TWrite a_write) :: v_algorithm (env, e, tau)
  | M.MALLOC e ->      (*   malloc e *)
    let alpha = TVar (new_var ()) in
    (tau, TLoc alpha) :: v_algorithm (env, e, alpha)
  | M.ASSIGN (e1, e2) ->   (*   e := e   *)
    v_algorithm (env, e1, TLoc tau) @ v_algorithm (env, e2, tau)
  | M.BANG e ->          (*   !e       *)
    v_algorithm (env, e, TLoc tau)
  | M.SEQ (e1, e2) ->     (*   e ; e    *)
    let alpha = TVar (new_var ()) in
    v_algorithm (env, e1, alpha) @ v_algorithm (env, e2, tau)
  | M.PAIR (e1, e2) ->   (*   (e, e)   *)
    let alpha1 = TVar (new_var ()) in
    let alpha2 = TVar (new_var ()) in
    (tau, TPair (alpha1, alpha2)) :: v_algorithm (env, e1, alpha1) @ v_algorithm (env, e2, alpha2)
  | M.FST e ->         (*   e.1      *)
    let a1 = TVar (new_var ()) in
    let a2 = TVar (new_var ()) in
    (a1, TPair (tau, a2)) :: v_algorithm (env, e, a1)
  | M.SND e ->            (*   e.2      *)
    let a1 = TVar (new_var ()) in
    let a2 = TVar (new_var ()) in
    (a1, TPair (a2, tau)) :: v_algorithm (env, e, a1)

let rec unify = fun (tau, tau') -> 
  let rec exists = fun alpha tau -> match tau with
    | TPair (t1, t2)
    | TFun (t1, t2) -> (exists alpha t1) || (exists alpha t2)
    | TVar tv
    | TEq tv
    | TWrite tv -> (alpha = tv)
    | TLoc tl -> exists alpha tl
    | _ -> false
  in
  match (tau, tau') with
  | (TInt, TInt) | (TString, TString) | (TBool, TBool) -> empty_subst
  | (TLoc tl1, TLoc tl2) -> unify (tl1, tl2) 
  | ((TVar alpha), t) | (t, TVar alpha) -> make_subst alpha t
  | ((TEq alpha), t) | (t, TEq alpha) -> if (exists alpha t) then raise (M.TypeError "already in tau")
    else (match t with
    | TInt | TBool | TString | TLoc _ | TEq _ | TWrite _ -> make_subst alpha t
    | _ -> raise (M.TypeError "Unify fail: equal"))
  | ((TWrite alpha), t) | (t, TWrite alpha) -> if (exists alpha t) then raise (M.TypeError "already in tau")
    else (match t with
    | TInt | TBool | TString | TWrite _ -> make_subst alpha t
    | _ -> raise (M.TypeError "Unify fail: write"))
  | (TPair (tau1, tau2), TPair (tau1', tau2'))
  | (TFun (tau1, tau2), TFun (tau1', tau2')) ->
    let s = unify (tau1, tau1') in
    let s' = unify (s tau2, s tau2') in
    s' @@ s
  | _ -> raise (M.TypeError "Unify fail: etc")
let rec unify_all = fun (eqn, s) -> match eqn with
  | [] -> raise (M.TypeError "Unifying nothing")
  | [(t, t')] -> (unify (t, t')) @@ s
  | u :: u' -> 
  let t = unify_all (u :: [], s) in
  let rec subst_eqn : subst -> tyEqn -> tyEqn = fun sub te -> match te with
    | [] -> []
    | (tau, tau') :: tl -> ((tau), (sub tau')) :: subst_eqn sub tl
  in
  unify_all ((subst_eqn t u'), t)

let rec m_algorithm : typ_env * M.exp * typ -> subst = fun (env, exp, tau) -> match exp with
  | M.CONST c -> (match c with
    | M.S s-> unify (TString, tau)
    | M.N n-> unify (TInt, tau)
    | M.B b-> unify (TBool, tau))
  | M.VAR x -> if (List.exists (fun (m_id, _) -> m_id = x) env) then
    let (_, tau') = List.find (fun elem -> (fst elem) = x) env in
    unify (tau, tau')
    else raise (M.TypeError (x ^ " is not declared"))
  | M.FN (x, e) ->
    let alpha1 = TVar (new_var ()) in
    let alpha2 = TVar (new_var ()) in
    let s = unify(TFun (alpha1, alpha2), tau) in
    let s' = m_algorithm ((x, s alpha1) :: (subst_env s env), e, s alpha2) in
    s' @@ s
  | M.APP (e1, e2) -> 
    let alpha = TVar (new_var ()) in
    let s = m_algorithm (env, e1, TFun (alpha, tau)) in
    let s' = m_algorithm (subst_env s env, e2, s alpha) in 
    s' @@ s
  | M.LET (dec, e) -> (match dec with
    | M.REC (id1, id2, e_rec) -> 
      let a1 = TVar (new_var ()) in
      let a2 = TVar (new_var ()) in
      let a3 = TVar (new_var ()) in
      let s = unify(TFun (a1, a2), a3) in
      let s' = m_algorithm ((id1, (s a3)) :: (id2, (s a1)) :: (subst_env s env), e_rec, s a2 ) in
      let s'' = m_algorithm ((id1, (s' @@ s) a3) :: (id2, (s' @@ s) a1) :: (subst_env (s' @@ s) env), e, (s' @@ s) tau) in
      s'' @@ s' @@ s
    | M.VAL (x, e_val) -> 
      let alpha = TVar (new_var ()) in
      let s = m_algorithm (env, e_val, alpha) in
      let s' = m_algorithm ((x, s alpha) :: (subst_env s env), e, s tau) in
      s' @@ s)
  | M.IF (e1, e2, e3) ->
    let s = m_algorithm (env, e1, TBool) in
    let s' = m_algorithm (subst_env s env, e2, s tau) in
    let s'' = m_algorithm (subst_env (s' @@ s) env, e3, (s' @@ s) tau) in
    s'' @@ s' @@ s
  | M.BOP (b, e1, e2) ->( match b with
    | M.ADD 
    | M.SUB -> 
      let s = unify (TInt, tau) in
      let s' = m_algorithm (subst_env s env, e1, TInt) in
      let s'' = m_algorithm (subst_env (s' @@ s) env, e2, TInt) in
      s'' @@ s' @@ s
    | M.AND
    | M.OR -> 
      let s = unify (TBool, tau) in
      let s' = m_algorithm (subst_env s env, e1, TBool) in
      let s'' = m_algorithm (subst_env (s' @@ s) env, e2, TBool) in
      s'' @@ s' @@ s
    | M.EQ -> 
      let a_eq = new_var () in
      let s = unify (TBool, tau) in
      let s' = m_algorithm (subst_env s env, e1, s (TEq a_eq)) in
      let s'' = m_algorithm (subst_env (s' @@ s) env, e2, (s' @@ s) (TEq a_eq)) in
      s'' @@ s' @@ s
)
  | M.READ -> unify (TInt, tau)
  | M.WRITE e ->
    let a_write = new_var () in
    let s = unify (TWrite a_write, tau) in
    let s' = m_algorithm (subst_env s env, e, s tau) in
    s' @@ s
  | M.MALLOC e ->      (*   malloc e *)
    let alpha = TVar (new_var ()) in
    let s = unify ((TLoc alpha), tau) in
    let s' = m_algorithm (subst_env s env, e, s alpha) in
    s' @@ s
  | M.ASSIGN (e1, e2) ->   (*   e := e   *)
    let s = m_algorithm (env, e1, TLoc tau) in
    let s' = m_algorithm (subst_env s env, e2, s tau) in
    s' @@ s
  | M.BANG e ->          (*   !e       *)
    m_algorithm (env, e, (TLoc tau))
  | M.SEQ (e1, e2) ->     (*   e ; e    *)
    let alpha = TVar (new_var ()) in
    let s = m_algorithm (env, e1, alpha) in
    let s' = m_algorithm (subst_env s env, e2, s tau) in
    s' @@ s
  | M.PAIR (e1, e2) ->   (*   (e, e)   *)
    let alpha1 = TVar (new_var ()) in
    let alpha2 = TVar (new_var ()) in
    let s = unify (TPair (alpha1, alpha2), tau) in
    let s' = m_algorithm (subst_env s env, e1, s alpha1) in
    let s'' = m_algorithm (subst_env (s' @@ s) env, e2, (s' @@ s) alpha2) in
    s'' @@ s' @@ s
  | M.FST e ->         (*   e.1      *)
    let a1 = TVar (new_var ()) in
    let a2 = TVar (new_var ()) in
    let s = unify (TPair (tau, a2), a1) in
    let s' = m_algorithm (subst_env s env, e, s a1) in
    s' @@ s
  | M.SND e ->            (*   e.2      *)
    let a1 = TVar (new_var ()) in
    let a2 = TVar (new_var ()) in
    let s = unify (TPair (a2, tau), a1) in
    let s' = m_algorithm (subst_env s env, e, s a1) in
    s' @@ s

let u_algorithm : tyEqn -> subst = fun eqn -> 
  unify_all (eqn, empty_subst)
(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp ->
  let a = TVar (new_var ()) in
  let empty_env = [] in
  (*let v = v_algorithm (empty_env, exp, a) in
  let s = u_algorithm v in
  conv2m (s a)*)
  (let s = m_algorithm (empty_env, exp, a) in
  conv2m (s a))
  
