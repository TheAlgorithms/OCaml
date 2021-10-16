let heap_sort arr =
  let swap i j =
    let t = arr.(i) in 
    arr.(i) <- arr.(j); 
    arr.(j) <- t
  in

  let sift k l =
    let rec check root child =
      if 2 * root + 1 < l then
        let ch =
          if child < l - 1 && arr.(child) < arr.(child + 1) then child + 1 else child in
        if arr.(root) < arr.(ch) then (swap root ch; check ch (2 * ch + 1)) in
    check k (2 * k + 1)
  in

  let len = Array.length arr in

  for start = (len / 2) - 1 downto 0 do
    sift start len
  done;

  for term = len - 1 downto 1 do
    swap term 0;
    sift 0 term;
  done
;;


(* testing the heap sort *)
let _ = 
  let pp_arr arr = 
    arr 
    |> Array.map string_of_int
    |> Array.fold_left (fun acc s -> acc ^ s ^ ", ") ""
  in
    let arr = [| 13; 2; 3; 14; 17; 4; 1; 5; 16; 12; 9; 10; 15; 8; 7; 11; 18; 19; 6; 20 |]
    in 
      Printf.printf "unsorted: [ %s ]\n" (pp_arr arr),
      heap_sort arr,
      Printf.printf "sorted: [ %s ]\n" (pp_arr arr);;