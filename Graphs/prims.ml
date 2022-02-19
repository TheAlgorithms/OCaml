type graph = (int * float * int) list

(* Implementation of Prims algorithm to find the minimal spanning tree of a graph in Ocaml *)
(* This was implemented for an uni assignment *)
(* Minimal spanning tree: https://en.wikipedia.org/wiki/Minimum_spanning_tree *)
(* Prims Algorithm: https://en.wikipedia.org/wiki/Prim%27s_algorithm *)


(*Some test Graphs to check for testing*)
let graphA = [ (1,1.,2); (3,1.,4); (1,10.,3); (2,10.,4)]
let graphB = [ (7, 0.5, 1); (1, 1.5, 6); (1, 2.4, 2); (2, 3.5, 3); (6, 1.2 , 3); (4, 2.4, 2); (2,1.2,5); (3, 5.4, 5)]
let graphC = [ (1,1.0, 2); (2,0.9,3); (4,1.2,3); (1,2.4, 4); (1,3.4,3)]
let graphD = [(0,4.,1); (0,8.,7); (7,11.,1); (1,8.,2); (7,1.,6); (7,7.,8); (6,6.,8); (8,2.,2); (2,4.,5); (6,2.,5); (2,7.,3); (3,9.,4); (3,14.,5); (5,10.,4)]

let rec getpossibleedges visitednodes graph =
  match graph with
    [] -> []
  | (n1,w,n2) :: xs -> if List.mem n1 visitednodes || List.mem n2 visitednodes
    then (n1,w,n2) :: getpossibleedges visitednodes xs
        else getpossibleedges visitednodes xs

let rec prims minst visitednodes notvisited =
  let possibleedges =
    List.sort(fun (_,f1,_) (_,f2,_) -> compare (f1) (f2)) (getpossibleedges visitednodes notvisited) in
  let rec choosenode possible = match possible with
      [] -> []
    | (a,w,b) :: xs ->
      if List.mem a visitednodes && List.mem b visitednodes then choosenode xs else [(a,w,b)] in
  let chosen = choosenode possibleedges in
  match chosen with
    [] -> minst
  | [(a,w,b)] ->
    let tree = minst @ [(a,w,b)] in
    let visited = a :: b :: visitednodes in
    prims tree visited notvisited
  | _ -> failwith "Something went wrong"

let mst edges = match edges with
    [] -> []
  | (n1,w,n2) :: xs -> prims [] [n1] edges
