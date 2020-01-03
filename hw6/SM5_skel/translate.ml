(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 *)

open K
open Sm5
module Translator = struct
  let name_num = ref 0 
  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    | K.VAR id -> [Sm5.PUSH (Sm5.Id id); Sm5.LOAD]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT e1 -> trans e1 @ [Sm5.NOT] 
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE e -> trans e @ [Sm5.MALLOC; Sm5.BIND "#write"; Sm5.PUSH (Sm5.Id "#write"); Sm5.STORE;
      Sm5.PUSH (Sm5.Id "#write"); Sm5.LOAD; Sm5.PUT; Sm5.PUSH (Sm5.Id "#write"); Sm5.LOAD; Sm5.UNBIND; Sm5.POP;] 
    | K.LETF (f, x, e1, e2) -> 
      [Sm5.PUSH (Sm5.Fn (x, [Sm5.BIND f] @ trans e1)); Sm5.BIND f;]
       @ trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.ASSIGN (x, e) -> trans e @ [Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.IF (e_cond, e_true, e_false) -> trans e_cond @ [Sm5.JTR (trans e_true, trans e_false)]
    | K.WHILE (e1, e2) ->
      let f_body = K.IF (e1, K.SEQ (e2, K.CALLV(while_id, K.UNIT)), K.UNIT) in 
      (trans (K.LETF("#while_f", "#while_x", f_body, K.CALLV (while_id, K.UNIT))))
    | K.FOR (id, e1, e2, e3) ->
      let body = K.IF ( K.LESS(K.VAR "#for_iter", K.ADD(K.VAR for_"#for_fin"end, K.NUM 1)), 
                        K.SEQ (K.SEQ (K.ASSIGN(id, (K.VAR "#for_iter")), e3),
                          K.SEQ (K.ASSIGN("#for_iter", K.ADD(K.VAR "#for_iter", K.NUM 1)), K.CALLR("#for_id", "#for_iter")
                          )
                        ), K.UNIT) in
      (trans (K.LETV("#for_iter", e1,
                K.LETV("#for_fin", e2,
                  K.LETF("#for_id", "#for_iter", body, K.CALLR ("#for_id", "#for_iter"))
                )
              )
            )
      )
    | K.SEQ (e1, e2) -> trans e1 @ trans e2
    | K.CALLV (f, arg_exp) -> 
      [Sm5.PUSH(Sm5.Id f);Sm5.PUSH(Sm5.Id f)] @ trans arg_exp @ [Sm5.MALLOC; Sm5.CALL;] 
    | K.CALLR (f, arg_var) -> 
    [Sm5.PUSH(Sm5.Id f);Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id arg_var); Sm5.LOAD; 
    Sm5.PUSH (Sm5.Id arg_var); Sm5.CALL]
    (*| _ -> failwith "Unimplemented"*)

end
