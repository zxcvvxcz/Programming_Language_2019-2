(* simple let rec *)

let rec f = fn x => 
  if (true) 
    then 1
	else if (false)
	       then 1
		   else (f (x - 1) + f (x - 2))
in
  f 10
end

(* result : int *)
