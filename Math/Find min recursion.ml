(* Find min recursion *)

let rec find_min l = match l with
	|[x] -> x
	|t::q -> (t if t < (find_min q) else find_min q)
	|[] -> failwith "Liste vide!";;