(* Power using recursion *)
(* power : float -> int -> float *)

let rec power base exponent = 
	if exponent <> 0 then  base *. power base (exponent - 1) else 1.;;