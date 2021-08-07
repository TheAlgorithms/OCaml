(* Euclidean GCD*)

let euclidean_gcd a b =
	let t = ref a in
	let a = ref a in
	let b = ref b in
	while !b <> 0 do
		t := !a;
		a := !b;
		b := !t mod !b;
	done;
	!a ;;


(* Euclidean GCD Recursive *)

let rec euclidean_gcd_recursive a b = 
	if b == 0 then a else euclidean_gcd_recursive b (a mod b);;