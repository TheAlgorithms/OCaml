(* Insertion Sort
 * https://en.wikipedia.org/wiki/Insertion_sort
 *)
 
(* Compares h to be sorted with sorted hd and tail. Terminates when 
 * h is smaller than an element in the list or if it reaches the end.
 *)
 let rec insert lst h =
  match lst with
    | hd::tail -> if hd > h then h::hd::tail else hd::(insert tail h)
    | _ -> [h]

(*Effectively a supplementary recurser for insert. Works similarly to the for
* loop in standard insertion sort. Not as intuitive to look at, but try tracing
* it out.
*)
let rec insertion_sort (lst : int list) : int list = match lst with
  | [] -> []
  | hd::tail -> insert (insertion_sort tail) hd

 (* testing the insertion sort function *)
let main () =
  let list_to_sort =
      [13; 2; 3; 14; 17; 4; 1; 5; 16; 12; 9; 10; 15; 8; 7; 11; 18; 19; 6; 20]
  in

  print_string "Unsorted: ";
  List.iter (Printf.printf "%d ") list_to_sort;
  print_newline ();

  print_string "  Sorted: ";
  List.iter (Printf.printf "%d ") (insertion_sort list_to_sort);
  print_newline ()


(* this can be run with: ocaml Sorts/insertionsort.ml on the command line *)
let _ = main ()