(*
  Bubble Sort
  https://en.wikipedia.org/wiki/Bubble_sort
*)

(*
  How does Bubble Sort work?
  ====================
  Bubble sort is a comparison-based sorting algorithm that takes 
  in a list of sortable objects, and returns that list sorted.

  It sorts by comparing the first item in an list to the next
  item. If the first item is greater than the second, then the
  two items' positions are swapped. Otherwise, the first item will 
  stay at its original index. Next, bubble sort compares the second 
  item in the list to the third item. Once again, if the second item 
  is greater tha the third item, the second item will swap positions
  with the third item. This process of checking and swaps will continue 
  until the end of the the list is reached.

  Example 
    Index 0 is compared to index 1: SWAP index 0 with index 1!
    [1; 0; 10; 50; 30] => [0; 1; 50; 30; 10]
    Index 1 is compared to index 2: NO SWAP!
    [0; 1; 50; 30; 10] => [0; 1; 50; 30; 10]
    Index 2 is compared to index 3: SWAP index 2 with index 3!
    [0; 1; 50; 30; 10] => [0; 1; 30; 50; 10]
    Index 3 is compared to index 4: SWAP index 3 with index 4!
    [0; 1; 30; 50; 10] => [0; 1; 30; 10; 50]

  At this point, we know that the greatest valued object is at the last
  index of the list. This is because every value has been compared to 
  the value at the next increasing index, with the larger value moving forward. 
  
  However, we do not know if the rest of the list is in order. In our
  example, for instance, index 2 holds the value 30, while index 3
  holds the value 10. This is still out of order! So, we will start again 
  at the first object in the list, and repeat the swapping process until 
  we reach the next unsorted value. In the case above, that is the last
  element of the list: index 4.

  This cycle of swaps will repeat until there is only one element that 
  isn't flagged as sorted. At that point, there is only one position it
  could be in. So the whole list should be considered sorted.
*)

(*
  Bubble Sort's Time Complexity
  =============================
  Worst-case: O(n^2)
    This occurs when every element is out of position.

  Best-case: O(n)
    This occurs when the list is already sorted. 

  Average-case: O(n^2)
*)

(* A recursive implementaiton *)
let rec bubble_sort list =
  let sorted_list = match list with
  | head_1 :: head_2 :: tail ->
      if head_1 > head_2 then
        head_2 :: bubble_sort(head_1 :: tail)
      else 
        head_1 :: bubble_sort(head_2 :: tail)
  | tail -> tail
  in
  if list = sorted_list then
    list
  else 
    bubble_sort sorted_list

(* Testing the bubble_sort function *)
let main () =
  let list_to_sort =
      [13; 2; 3; 14; 17; 4; 1; 5; 16; 12; 9; 10; 15; 8; 7; 11; 18; 19; 6; 20]
  in

  print_string "Unsorted: ";
  List.iter (Printf.printf "%d ") list_to_sort;
  print_newline ();

  print_string "  Sorted: ";
  List.iter (Printf.printf "%d ") (bubble_sort list_to_sort);
  print_newline ()


(* Run with: ocaml bubble_sort.ml on the command line *)
let () = main ()