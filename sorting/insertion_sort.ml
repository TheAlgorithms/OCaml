(* Arrays are mutable *)

let insertion_sort (arr: int array) =
  let rec insert_index ~(key: int) ~(j: int) =
    if j > 0 && arr.(j - 1) > key then (
      arr.(j) <- arr.(j - 1);
      insert_index ~key ~j:(j - 1)
    ) else j
  in
  let rec loop (i: int) =
    if i < Array.length arr then (
      let key = arr.(i) in
      arr.(insert_index ~key ~j:i) <- key;
      loop (i + 1)
    )
  in loop 1
;;

let () =
  let print_array ~(label:string) ~(arr: int array) =
    Printf.printf "%s: " label;
    Array.iter (fun x -> Printf.printf "%d " x) arr;
    Printf.printf "\n";
  in

  let tests = [
    [|45; 62; 78; 43; 12; 7; 92; 56; 31; 70|];
    [|13; 2; 3; 14; 17; 4; 1; 5; 16; 12; 9; 10; 15; 8; 7; 11; 18; 19; 20; 21; 6|]
  ] in

  List.iter (fun test -> 
    print_array ~label:"Before Sorting" ~arr:test;

    insertion_sort test;
    
    print_array ~label:"After Sorting" ~arr:test;
    Printf.printf "\n";
  ) tests
;;

(* Output:
Before sorting: 45 62 78 43 12 7 92 56 31 70 
After sorting: 7 12 31 43 45 56 62 70 78 92 

Before sorting: 13 2 3 14 17 4 1 5 16 12 9 10 15 8 7 11 18 19 20 21 6 
After sorting: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 
*)