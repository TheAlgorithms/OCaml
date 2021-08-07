(* Euler Project - Problem 4 *)

let is_palindromic n = 
	let str_n = string_of_int n in
	let len_n = 1 + int_of_float(log10(float_of_int n)) in
	let rec palindrom_test = function
		| i when i = len_n / 2 -> true
		| i -> str_n.[i] = str_n.[len_n - i - 1] && palindrom_test (i + 1)
	in palindrom_test 0;;

let solution = 
	let largest = ref 0 in
	for a = 100 to 999 do
		for b = 100 to 999 do
			if is_palindromic (a * b) then
				if (a * b) > !largest then largest := (a * b)
		done
	done;
	!largest
	;;
	
solution;;
