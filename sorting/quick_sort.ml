(* Quicksort
 * https://en.wikipedia.org/wiki/Quicksort *)

(* partitions list into two lists:
 * elements that are less than or equal to the pivot, and
 * elements that are greater than the pivot *)
let rec partition : 'a * 'a list * 'a list * 'a list -> 'a list * 'a list = function
    | _, [], less, more -> less, more
    | pivot, first :: rest, less, more ->
        if first <= pivot
        then partition (pivot, rest, first :: less, more)
        else partition (pivot, rest, less, first :: more)


(* for simplicity, always choosing the leftmost element as pivot *)
let rec quicksort : 'a list -> 'a list = function
    | [] -> []
    | pivot :: rest ->
        let less, more = partition (pivot, rest, [], []) in
        quicksort less @ [pivot] @ quicksort more


(* testing the quicksort function *)
let main () =
    let list_to_sort =
        [13; 2; 3; 14; 17; 4; 1; 5; 16; 12; 9; 10; 15; 8; 7; 11; 18; 19; 6; 20]
    in

    print_string "Unsorted: ";
    List.iter (Printf.printf "%d ") list_to_sort;
    print_newline ();

    print_string "  Sorted: ";
    List.iter (Printf.printf "%d ") (quicksort list_to_sort);
    print_newline ()


(* this can be run with: ocaml quicksort.ml on the command line *)
let _ = main ()
