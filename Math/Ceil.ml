(* Ceil function *)

let ceil x = 
	if 2 - int_of_float x <= 0 then int_of_float(x)
	else int_of_float(x) + 1;;