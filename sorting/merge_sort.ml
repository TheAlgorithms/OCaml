(* Arrays are mutable *)

let merge (arr: ' array) ~(p: int) ~(q: int) ~(r: int) =
  let n1 = q - p + 1 
  and n2 = r - q in

  let a = Array.init n1 (fun i -> arr.(p + i))
  and b = Array.init n2 (fun i -> arr.(q + 1 + i)) in

  let rec loop (i: int) (j: int) (k: int) = 
    let select_i () = arr.(k) <- a.(i); loop (i + 1) j (k + 1)
    and select_j () = arr.(k) <- b.(j); loop i (j + 1) (k + 1) in

    if i < n1 && j < n2 then (
      if a.(i) < b.(j) then select_i () 
      else select_j ()
    ) else (
      if i < n1 then select_i () 
      else if j < n2 then select_j ()
    )
  in loop 0 0 p;
;;

let rec merge_sort arr ~p ~r =
  if p < r then (
    let q = (p + r) / 2 in 
    merge_sort arr ~p ~r:q;
    merge_sort arr ~p:(q + 1) ~r;
    merge arr ~p ~q ~r
  )
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

    merge_sort test ~p:0 ~r:(Array.length test - 1);
    
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