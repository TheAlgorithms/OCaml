(* Project Euler - Problem 1 *)

let rec sum n = match n with
	| 1 -> 0
	| _ -> if n mod 3 = 0 || n mod 5 = 0 then n + sum (n - 1) else sum (n - 1)
	;;

sum (1000 - 1);; 

