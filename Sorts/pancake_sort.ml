let rec sorted = function
  | [] -> (true)
  | h :: h' :: _ when h > h' -> (false)
  | _ :: t -> sorted t 

let flip l =
  let rec aux acc g p s = function
    | h :: t when h > g -> aux (h :: acc) h acc t t  
    | h :: t -> aux (h :: acc) g p s t
    | [] -> (g, (p @ s))
  in 
    aux [] min_int [] l l

let pancake_sort l =
  let rec aux l s =
    let g, l = flip l in 
    let s = g :: s 
    and l = List.rev l in
    if sorted l then (l @ s)
    else aux l s
  in 
    aux l []

(* testing the pancake sort *)
let _ = 
  let pp_list list = 
    list 
    |> List.map string_of_int
    |> String.concat ", "
  in
    let list = [13; 2; 3; 14; 17; 4; 1; 5; 16; 12; 9; 10; 15; 8; 7; 11; 18; 19; 6; 20]
    in 
      Printf.printf "unsorted: [ %s ]\n" (pp_list list),
      Printf.printf "sorted: [ %s ]\n" (list |> pancake_sort |> pp_list)
      