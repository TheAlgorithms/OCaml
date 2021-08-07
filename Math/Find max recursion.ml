(* Find max recursion *)

let rec find_max l = match l with
	|[x] -> x
	|t::q -> if t > (find_max q) then t else find_max q
	|[] -> failwith "Liste vide!";;