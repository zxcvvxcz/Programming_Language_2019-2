(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton
 *)

open M

type var = string

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

type typ_scheme =
  | SimpleTyp of typ 
  | GenTyp of (var list * typ)

type typ_env = (M.id * typ_scheme) list

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

let rec conv2m : typ -> M.typ = fun t ->
  match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (conv2m t1, conv2m t2)
  | TLoc t' -> M.TyLoc (conv2m t')
  | TFun (t1, t2) -> raise (M.TypeError "TFun is not a result type")
  | TVar v -> raise (M.TypeError ("TVar" ^ v ^ " is not a result type"))
  | _ -> raise (M.TypeError "Not a result type")
(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 = 
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2
  
let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v
  | TEq v
  | TWrite v -> [v]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas 

let ftv_of_env : typ_env -> var list = fun tyenv ->
  List.fold_left 
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv 

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | TEq x_eq -> if (x = x_eq) then
      (match t with
        | TVar x' -> TEq x'
        | _ -> t)
      else t'
    | TWrite x_w -> if (x = x_w) then
      (match t with
        | TVar x' | TEq x' -> TWrite x'
        | _ -> t)
      else t'
    | TVar x_var -> if (x = x_var) then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

let typ_of_scheme = function
  | SimpleTyp s -> s
  | GenTyp (ftv, ty) -> ty

let rec expansive = function 
  | M.CONST _ 
  | M.VAR _ 
  | M.READ
  | M.FN _ -> false
  | M.APP _
  | M.MALLOC _ -> true
  | M.LET (dec, e) -> (match dec with
    | M.REC (f, x, e_rec) -> (expansive e)
    | M.VAL (x, e_val) ->  (expansive e_val) || (expansive e))
  | M.IF (e1, e2, e3) -> (expansive e1) || (expansive e2) || (expansive e3)
  | M.BOP (_, e1, e2) 
  | M.ASSIGN (e1, e2) 
  | M.SEQ (e1, e2) 
  | M.PAIR (e1, e2) -> (expansive e1) || (expansive e2)
  | M.WRITE e 
  | M.BANG e 
  | M.FST e
  | M.SND e -> expansive e

let rec unify = fun (tau, tau') -> 
  let rec exists = fun alpha ts -> match ts with
      | TPair (t1, t2)
      | TFun (t1, t2) -> (exists alpha t1) || (exists alpha t2)
      | TVar tv
      | TEq tv
      | TWrite tv -> (alpha = tv)
      | TLoc tl -> exists alpha tl
      | _ -> false
  in
  match (tau, tau') with
  | (t1, t2) when t1 = t2 -> empty_subst
  | (TLoc tl1, TLoc tl2) -> unify (tl1, tl2) 
  | ((TVar alpha), t) | (t, TVar alpha) -> if (exists alpha t) then raise (M.TypeError "already in tau") 
    else make_subst alpha t
  | ((TEq alpha), t) | (t, TEq alpha) -> if (exists alpha t) then raise (M.TypeError "already in tau")
    else (match t with
    | TInt | TBool | TString | TLoc _ | TEq _ | TWrite _ -> make_subst alpha t
    | _ -> raise (M.TypeError "Unify fail: equal"))
  | ((TWrite alpha), t) | (t, TWrite alpha) -> if (exists alpha t) then raise (M.TypeError "already in tau")
    else (match t with
    | TInt | TBool | TString | TWrite _ -> make_subst alpha t
    | TEq eq -> unify ((TWrite alpha), (TWrite eq))
    | _ -> raise (M.TypeError "Unify fail: write"))
  | (TPair (tau1, tau2), TPair (tau1', tau2'))
  | (TFun (tau1, tau2), TFun (tau1', tau2')) ->
    let s = unify (tau1, tau1') in
    let s' = unify (s tau2, s tau2') in
    s' @@ s
  | _ -> raise (M.TypeError "Unify fail: etc")

let rec m_algorithm : typ_env * M.exp * typ -> subst = fun (env, exp, tau) ->
  match exp with
  | M.CONST c -> 
    (match c with
    | M.S s-> unify (TString, tau)
    | M.N n-> unify (TInt, tau)
    | M.B b-> unify (TBool, tau))
  | M.VAR x -> 
    let alpha_scheme = try List.assoc x env with Not_found -> raise (M.TypeError (x ^ "is unbound")) in
    let ab_scheme = subst_scheme empty_subst alpha_scheme in
    (match ab_scheme with
    | SimpleTyp st -> unify (tau, st)
    | GenTyp (ftv, t) -> unify (tau, t))
  | M.FN (x, e) ->
    let beta1 = TVar (new_var ()) in
    let beta2 = TVar (new_var ()) in
    let s1 = unify(tau, TFun (beta1, beta2)) in
    let s2 = m_algorithm ((x, SimpleTyp (s1 beta1)) :: (subst_env s1 env), e, s1 beta2) in
    s2 @@ s1
  | M.APP (e1, e2) -> 
    let beta = TVar (new_var ()) in
    let s1 = m_algorithm (env, e1, TFun (beta, tau)) in
    let s2 = m_algorithm (subst_env s1 env, e2, s1 beta) in 
    s2 @@ s1
  | M.LET (dec, e) -> (match dec with
    | M.REC (f, x, e_rec) -> 
      let beta = TVar (new_var ()) in
      let s1 = m_algorithm ((f, SimpleTyp beta) :: env, (M.FN (x, e_rec)), beta) in
      (* expansive \x.e_rec always false *)
      let s2 = m_algorithm ((f, generalize (subst_env s1 env) (s1 beta)) :: subst_env s1 env, e, s1 tau) in
      s2 @@ s1
    | M.VAL (x, e_val) -> 
      let beta = TVar (new_var ()) in
      let s1 = m_algorithm (env, e_val, beta) in
      if (expansive e_val) then
        let s2 = m_algorithm ((x, SimpleTyp (s1 beta) ) :: (subst_env s1 env), e, s1 tau) in
        s2 @@ s1
      else
        let s2 = m_algorithm ((x, generalize (subst_env s1 env) (s1 beta) ) :: (subst_env s1 env), e, s1 tau) in
        s2 @@ s1)
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
      let a_eq = TEq (new_var ()) in
      let s = unify (TBool, tau) in
      let s' = m_algorithm (subst_env s env, e1, s a_eq) in
      let s'' = m_algorithm (subst_env (s' @@ s) env, e2, (s' @@ s) a_eq) in
      s'' @@ s' @@ s
)
  | M.READ -> unify (TInt, tau)
  | M.WRITE e ->
    let a_write = TWrite (new_var ()) in
    let s = unify (a_write, tau) in
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

(* TODO : Implement this function *)
let check : M.exp -> M.typ = fun exp ->
  let a = TVar (new_var ()) in
  let empty_env = [] in
  let s = m_algorithm (empty_env, exp, a) in
  conv2m (s a)
