(* Euler project - Problem 2 *)

let rec sum current previous max accu = 
	if current > max 
		then accu 
		else
			if current mod 2 = 0 
				then sum (current + previous) current max (accu + current)
				else sum (current + previous) current max accu;;
		
sum 2 1 4000000 0;;	