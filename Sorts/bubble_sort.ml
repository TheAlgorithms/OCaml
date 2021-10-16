let rec bubble_sort lst =
  let aux =
  match lst with
  | h :: h' :: tl when h > h' ->
    h' :: bubble_sort (h :: tl)
  | h :: tl ->
    h :: bubble_sort tl
  | tl ->
    tl
  in
    if lst = aux then lst
    else bubble_sort aux

(* testing the bubble sort *)
let _ = 
  let pp_list list = 
    list 
    |> List.map string_of_int
    |> String.concat ", "
  in
    let list = [13; 2; 3; 14; 17; 4; 1; 5; 16; 12; 9; 10; 15; 8; 7; 11; 18; 19; 6; 20]
    in 
      Printf.printf "unsorted: [ %s ]\n" (pp_list list),
      Printf.printf "sorted: [ %s ]\n" (list |> bubble_sort |> pp_list)