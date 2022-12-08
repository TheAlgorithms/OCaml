(* Insertion Sort
 * https://en.wikipedia.org/wiki/Insertion_sort
 *)
 let rec insert s h =
  match s with
    | hd::tail -> if hd > h then h::hd::tail else hd::(insert tail h)
    | _ -> [h]


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