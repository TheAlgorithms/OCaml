(* Linear search implementation in OCaml.
   Since lists in OCaml are true linked lists, linear search is the only option.
   

   You can load this file into the REPL with `#use "linear_search.ml"`
 *)

(** Finds the position of an item in a linked list.

    @param needle Item to search for.
    @param haystack Linked list to search.

 *)
let linear_search needle haystack =
  let rec aux needle haystack pos =
    match haystack with
    | [] -> None
    | h :: hs ->
      if h = needle then Some pos
      else aux needle hs (pos + 1)
  in aux needle haystack 0

(* Examples:

  linear_search 0 [1;2;3] (* Returns None *)
  linear_search 0 [1;2;3;0] (* Returns (Some 3) *)
  linear_search ~start_pos:1 0 [1;2;3;0] (* Returns (Some 4) *)
 *)

(** Finds the position of an item in an array.

    @param needle Item to search for.
    @param haystack Array to search.
 *)
let linear_search_array needle haystack =
  (* This function is tail-recursive -- it will not run out of stack space *)
  let rec aux needle haystack pos =
    if haystack.(pos) = needle then pos
    else aux needle haystack (pos + 1)
  in
  try Some (aux needle haystack 0)
  with Invalid_argument _ ->
    (* Index out of bounds condition will cause an Invalid_argument exception *)
    None

(* Examples:

  linear_search_array 0 [| 1; 2; 3; 0 |]
 *)
