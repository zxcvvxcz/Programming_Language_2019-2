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
    | K.WHILE (e_cond, e_body) -> 
      let _ = name_num := !name_num + 1 in
      let while_f = String.concat "" ["#while_f"; string_of_int (!name_num)] in
      [Sm5.PUSH (Sm5.Fn ("#while_x", [Sm5.BIND while_f] @ trans e_cond @ [Sm5.JTR (
        trans e_body @ [Sm5.PUSH (Sm5.Id while_f); Sm5.PUSH (Sm5.Id while_f)] 
        @ trans e_cond @ [Sm5.MALLOC; Sm5.CALL], [Sm5.PUSH (Sm5.Val Sm5.Unit)])])); Sm5.BIND while_f; 
        Sm5.PUSH (Sm5.Id while_f); Sm5.PUSH (Sm5.Id while_f)] @ trans e_cond @ [Sm5.MALLOC; Sm5.CALL];
    | K.FOR (id, e1, e2, e_body) -> 
      let _ = name_num := !name_num + 1 in
      let for_f = String.concat "" ["#for_f"; string_of_int (!name_num)] in
      trans e2 @ trans e1 @ 
      [Sm5.MALLOC; Sm5.BIND id; Sm5.PUSH (Sm5.Id id); Sm5.STORE; Sm5.PUSH (Sm5.Id id); Sm5.LOAD; Sm5.LESS;
      Sm5.PUSH (Sm5.Fn ("#for_x", [Sm5.BIND for_f] @ [
      Sm5.JTR([Sm5.PUSH (Sm5.Val (Sm5.Unit))], (trans e_body @
      [Sm5.PUSH (Sm5.Id id); Sm5.LOAD; Sm5.PUSH (Sm5.Val (Sm5.Z 1)); Sm5.ADD; Sm5.PUSH (Sm5.Id id); Sm5.STORE]
       @ trans e2 @ [Sm5.PUSH (Sm5.Id id); Sm5.LOAD;Sm5.LESS; Sm5.PUSH (Sm5.Id for_f); Sm5.PUSH (Sm5.Id for_f); 
      Sm5.PUSH(Sm5.Id id); Sm5.LOAD; Sm5.MALLOC; Sm5.CALL]))])); Sm5.BIND for_f ; 
      Sm5.PUSH (Sm5.Id for_f); Sm5.PUSH (Sm5.Id for_f); Sm5.PUSH (Sm5.Id id); Sm5.LOAD; Sm5.MALLOC; Sm5.CALL]
    | K.SEQ (e1, e2) -> trans e1 @ trans e2
    | K.CALLV (f, arg_exp) -> 
      [Sm5.PUSH(Sm5.Id f);Sm5.PUSH(Sm5.Id f)] @ trans arg_exp @ [Sm5.MALLOC; Sm5.CALL;] 
    | K.CALLR (f, arg_var) -> 
    [Sm5.PUSH(Sm5.Id f);Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id arg_var); Sm5.LOAD; 
    Sm5.PUSH (Sm5.Id arg_var); Sm5.CALL]
    (*| _ -> failwith "Unimplemented"*)

end
