(* Euler Project - Problem 3 *)

let rec bigger_div n d =
	if n mod d = 0 then n/d
	else bigger_div n (d + 1);;

let is_composed n = bigger_div n 2 <> 1;;

let solution n = 
	let test = ref (bigger_div n 2) in
	while is_composed !test do
		test := bigger_div !test 2
		done;
	!test;;
	
solution 600851475143 ;;	