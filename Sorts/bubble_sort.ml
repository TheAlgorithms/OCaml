(* Bubble sort - recursive version with tail-recursion *)
(* See https://en.wikipedia.org/wiki/Bubble_sort *)
let bubble_sort l = 
  let n = (List.length l) - 1 in
  let rec bubble_sort_rec arr res n = 
    match arr with
    | [] -> if n > 0 then bubble_sort_rec res [] (n - 1) else res 
    | x :: [] -> if n > 0 then bubble_sort_rec (res @ [x]) [] (n - 1) else (res @ [x])
    | x1 :: x2 :: xs -> 
        begin
          if x1 <= x2 then
            (bubble_sort_rec (x2 :: xs) (res @ [x1]) n)
          else 
            (bubble_sort_rec (x1 :: xs) (res @ [x2]) n)
        end
  in bubble_sort_rec l [] (n+1);;


(* A few tests to check the function works correctly *)

assert ((bubble_sort []) = []);;
assert ((bubble_sort [1]) = [1]);;

assert ((bubble_sort [2; 1]) = [1; 2]);;

assert ((bubble_sort [-1; 4]) = [-1; 4]);;

assert ((bubble_sort [3; 2; 1]) = [1; 2; 3]);;  
assert ((bubble_sort [1; 2; 3]) = [1; 2; 3]);;
  
assert ((bubble_sort [3; 2; 1; 4]) = [1; 2; 3; 4]);;  
assert ((bubble_sort [1; 2; 3; 4]) = [1; 2; 3; 4]);;
  
assert ((bubble_sort [10; 40; 3; 2;]) = [2; 3; 10; 40]);;

assert ((bubble_sort [10; 40; 3; 2; 4050; -6; 28; 32;]) = [-6; 2; 3; 10; 28; 32; 40; 4050]);;

