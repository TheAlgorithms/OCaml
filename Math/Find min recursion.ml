(* Find min recursion *)

let rec find_min l = match l with
	|[x] -> x
	|t::q -> if t < (find_min q) then t else find_min q
	|[] -> failwith "Liste vide!";;