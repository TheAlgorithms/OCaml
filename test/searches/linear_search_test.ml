open Searches.Linear_search

let _ = assert(linear_search_array 0 [| 1; 2; 3; 0 |] = Some 3)
