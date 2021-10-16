let split list =
  let rec aux l acc acc' =
    match l with
    | [] -> (acc, acc')
    | [x] -> (x :: acc, acc')
    | h :: h' :: t -> aux t (h :: acc) (h' :: acc')
  in
    aux list [] []

let rec merge l l' =
  match (l, l') with
  | (x, []) -> x
  | ([], x) -> x
  | (x :: tx, y :: ty) -> 
    if x < y then
      x :: merge tx l'
    else
      y :: merge l ty

let rec merge_sort list =
  match list with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let (l, l') = split list in
    merge (merge_sort l) (merge_sort l')

(* testing the merge sort *)
let _ = 
  let pp_list list = 
    list 
    |> List.map string_of_int
    |> String.concat ", "
  in
    let list = [13; 2; 3; 14; 17; 4; 1; 5; 16; 12; 9; 10; 15; 8; 7; 11; 18; 19; 6; 20]
    in 
      Printf.printf "unsorted: [ %s ]\n" (pp_list list),
      Printf.printf "sorted: [ %s ]\n" (list |> merge_sort |> pp_list)