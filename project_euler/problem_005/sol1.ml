(* Euler Project - Problem 5 *)

let solution =
	let solved = ref false in
	let i = ref (20*19) in
	while !solved = false do
		solved := true;
		for d = 2 to 20 do
			if !i mod d <> 0 then solved := false
		done;
		if !solved = false then i := !i + (20*19)
	done;
	!i;;
	
solution;;