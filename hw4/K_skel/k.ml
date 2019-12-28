(*
 * SNU 4190.310 Programming Languages 2019 Fall
 *  K- Interpreter Skeleton Code
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
    
  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | NUM i -> (Num i, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem) 
    | UNIT -> (Unit, mem)
    | VAR x -> (Mem.load mem (lookup_env_loc env x), mem)
    | ADD (e1, e2) -> 
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (Num (value_int v1 + value_int v2), mem'')
    | SUB (e1, e2) -> 
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (Num (value_int v1 - value_int v2), mem'')
    | MUL (e1, e2) -> 
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (Num (value_int v1 * value_int v2), mem'')
    | DIV (e1,e2) -> 
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (Num (value_int v1 / value_int v2), mem'')
    | EQUAL (e1,e2) -> 
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      if (v1 = v2) then (Bool true, mem'')
      else (Bool false, mem'')
    | LESS (e1,e2) -> 
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      if (value_int v1 < value_int v2) then (Bool true, mem'')
      else (Bool false, mem'')
    | NOT e -> 
      let (v, mem') = eval mem env e in
      (Bool (not (value_bool v)), mem')
    | SEQ (e1, e2) ->            (* sequence *)
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (v2, mem'')
    | IF (e1, e2, e3) ->      (* if-then-else *)
      let (v1, mem') = eval mem env e1 in
      if (value_bool v1 = true) then
        let (v2, mem'') = eval mem' env e2 in
        (v2, mem'')
      else 
        let (v2, mem'') = eval mem' env e3 in
        (v2, mem'')
    | WHILE (e1, e2) ->          (* while loop *)
      let (v, mem') = eval mem env e1 in
      if (value_bool v = true) then
        let (v1, mem1) = eval mem' env e2 in
        eval mem1 env (WHILE (e1, e2))
      else (Unit, mem')
    | LETF (x, xl, e1, e2) -> (* procedure binding *)
      let f = Proc (xl, e1, env) in
      let env' = (Env.bind env x f) in
      eval mem env' e2
    | CALLV (f, el) ->     (* call by value *)
      let (xl, e', env') = lookup_env_proc env f in
      if List.length xl != List.length el then raise (Error "Invalid")
      else
      let rec vmlist : memory -> env -> exp list -> (value list) * memory = fun mem env el ->
        match el with
        | [] -> ([], mem)
        | hd :: tl ->
        let (vn, memn) = eval mem env hd in
        (vn :: fst (vmlist memn env tl), snd (vmlist memn env tl))
      in
      let (vl, memn) = vmlist mem env el in
      let rec mem_add : memory -> value list -> memory * Loc.t list = fun mem vl ->
      match vl with
      | [] -> mem, []
      | hd :: tl ->
        let (l, mem') = Mem.alloc mem in
        let mem'' = Mem.store mem' l hd in
        (fst (mem_add mem'' tl), l :: snd (mem_add mem'' tl))
      in
      let memn', loc_list = mem_add memn vl in
      let rec env_loc_bind : env -> id list -> Loc.t list -> env = fun env xl loc_list ->
      match loc_list with
        | [] -> env
        | hd :: tl ->
        let env' = Env.bind env (List.hd xl) (Addr hd) in
        env_loc_bind env' (List.tl xl) tl
      in
      let env'' = env_loc_bind env' xl loc_list in
      let env''' = Env.bind env'' f (Proc (xl, e', env'')) in
      let (v', mem') = eval memn' env''' e' in
      (v', mem')
    | CALLR (f, yl) ->       (* call by referenece *)
      let (xl, e, env') = lookup_env_proc env f in
      if List.length xl != List.length yl then raise (Error "Invalid")
      else
      let loc_list = List.map (lookup_env_loc env) yl in
      let rec b_list = fun env xlist llist ->
      match llist with
      | [] -> env
      | hd :: tl ->
        let env_new = Env.bind env (List.hd xlist) (Addr hd) in
        b_list env_new (List.tl xlist) tl
      in
      let env'' = b_list env' xl loc_list in
      let env''' = Env.bind env'' f (Proc (xl, e, env'')) in
      let (v, mem') = eval mem env''' e in
      (v, mem')
    | RECORD xel -> ( (* record construction *)
      if xel = [] then (Unit, mem) else
      let rec eval_list : memory -> env -> (id * exp) list -> (id * Loc.t) list * memory = 
      fun mem env xel ->
      match xel with
        | [] -> ([], mem)
        | hd :: tl ->(
        let (vi, memi) = eval mem env (snd hd) in
        let (li, memi') = Mem.alloc memi in
        let memi'' = Mem.store memi' li vi in
        ((fst hd), li) :: fst (eval_list memi'' env tl), snd (eval_list memi'' env tl)) 
      in
      let rlist, memn = eval_list mem env xel in
      let rec list_search : (id * Loc.t) list -> id -> Loc.t= fun list x ->
      match list with
      | [] -> raise (Error "Unbound")
      | hd :: tl ->
        if (x = fst hd) then snd hd
        else list_search tl x
      in
    (Record (list_search rlist), memn)
    ) 
    | FIELD (e, x) ->           (* access record field *)
      let (r, mem') = eval mem env e in
      let l = (value_record r) x in
      (Mem.load mem' l, mem')
    | ASSIGNF (e1, x, e2) ->   (* assign to record field *)
      let (r, mem1) = eval mem env e1 in
      let l = (value_record r) x in
      let (v, mem2) = eval mem1 env e2 in
      (v, Mem.store mem2 l v)
    (*| _ -> failwith "Unimplemented" (* TODO : Implement rest of the cases *)*)

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
