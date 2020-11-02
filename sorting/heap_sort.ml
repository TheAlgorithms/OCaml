(* Arrays are mutable *)

let parent_of i = (i - 1) / 2;;
let left_of i = 2 * i + 1 ;;
let right_of i = 2 * i + 2 ;;

let rec max_heapify arr ~size ~i =
  let left = left_of i 
  and right = right_of i in 

  let largest = if left < size && arr.(left) > arr.(i) then left else i in 
  let largest = if right < size && arr.(right) > arr.(largest) then right else largest in 

  if largest != i then
    let temp = arr.(largest) in 
    arr.(largest) <- arr.(i); arr.(i) <- temp;
    max_heapify arr ~size ~i:largest
;;

let build_max_heap arr = 
  let size = Array.length arr in 
  for i = parent_of (size - 1) downto 0 do 
    max_heapify arr ~size ~i
  done
;;

let heap_sort arr =
  build_max_heap arr;

  let open Array in
  for last = length arr - 1 downto 1 do 
    let temp = arr.(last) in 
    arr.(last) <- arr.(0); 
    arr.(0) <- temp;
    max_heapify arr ~size:last ~i:0 
  done
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

    heap_sort test;
    
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