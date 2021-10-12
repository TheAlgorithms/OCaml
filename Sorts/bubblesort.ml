(* Bubblesort
 * https://en.wikipedia.org/wiki/Bubble_sort *)

(* a single pass of bubble sort
 * 
 * if list is already sorted, returns it unchanged
 *)
let rec bubblepass : 'a list -> 'a list = function
  | [] -> []  (* empty list is already sorted *)
  | [x] -> [x] (* Singleton list is already sorted *)
  | x1::x2::xs ->  if x1 > x2
                   then [x2] @ (bubblepass ([x1] @ xs))
                   else [x1] @ (bubblepass ([x2] @ xs))

(* simple Bubble sort implementation (re-implementation of the Haskell version) *)
let rec bubblesort : 'a list -> 'a list = function
    | [] -> []   (* empty list is always sorted *)
    | [x] -> [x] (* Singleton list always sorted *)
    | lst -> let bpassed = bubblepass lst
               in 
                 let cmp = compare bpassed lst in
                   if cmp == 0 then lst
                   else bubblesort bpassed

(* testing the bubblesort function *)
let main () =
    let list_to_sort =
        [13; 2; 3; 14; 17; 4; 1; 5; 16; 12; 9; 10; 15; 8; 7; 11; 18; 19; 6; 20]
    in
    print_string "Unsorted: ";
    List.iter (Printf.printf "%d ") list_to_sort;
    print_newline ();

    print_string "  Sorted: ";
    List.iter (Printf.printf "%d ") (bubblesort list_to_sort);
    print_newline ()


(* this can be run with: ocaml bubblesort.ml on the command line *)
let _ = main ()
