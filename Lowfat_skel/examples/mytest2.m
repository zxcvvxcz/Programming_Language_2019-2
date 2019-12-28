(* simple let val *)

let val l = (fn x=> (1 + x)) in
  (l) 2; l 1
end


(* result : int *)
(*Wrong*)