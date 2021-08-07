(* Sum of digits *)

let rec sum_of_digits_recursion n =
	let abs x = if x > 0 then x else -x in (* We need the absolute value of the number for the result to be positive) *)
	if abs(n) < 10 then abs(n) else abs(n) mod 10 + sum_of_digits_recursion (n / 10);;
