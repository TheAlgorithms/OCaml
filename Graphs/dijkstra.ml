(*
 #Algorithm
    #1) Create a set sptSet (shortest path tree set) that keeps track of vertices included in the shortest-path tree,
    # i.e., whose minimum distance from the source is calculated and finalized.
    # Initially, this set is empty.
    #2) Assign a distance value to all vertices in the input graph. Initialize all distance values as INFINITE.
    # Assign distance value as 0 for the source vertex so that it is picked first.
    #3) While sptSet doesn’t include all vertices
        #….a) Pick a vertex u which is not there in sptSet and has a minimum distance value.
        #….b) Include u to sptSet.
        #….c) Update distance value of all adjacent vertices of u.
        # To update the distance values,iterate through all adjacent vertices.
        # For every adjacent vertex v, if the sum of distance value of u (from source) and weight of edge u-v,
        # is less than the distance value of v, then update the distance value of v.
  *)

(* Implementation of Dijkstras shortest path alorithm returning the shortest distances between given node and all other nodes in the given graph *)
(* author: https://github.com/maharajamihir *)

type graph = (int * int) list

let graphA = [ (1,2); (3,4); (1,3); (2,4)]
let graphB = [ (7, 1); (1, 6); (1, 2); (2, 3); (6 , 3); (4, 2); (2,5); (3, 5)]
let graphC = [ (1,2); (2,3); (4,3); (1,4); (1,3)]
let graphD = [(0,1); (0,7); (7,1); (1,2); (7,6); (7,8); (6,8); (8,2); (2,5); (6,5); (2,3); (3,4); (3,5); (5,4)]


let rec get_neighbors g node =
  match g with
    [] -> []
  | (a,b) :: ns ->
    if a = node then
      b :: get_neighbors ns node
    else if b = node then
      a :: get_neighbors ns node
    else get_neighbors ns node

let dijkstra edges node =
  (*step one*)
  let rec extract_nodes es =
    match es with
      [] -> []
    | (a,b) :: ess ->
      if (List.filter(fun (x,y) -> x=a || x=b || y=a || y=b) ess) = []
      then
        extract_nodes ess
      else
        a :: b :: extract_nodes ess
  in
  let all_nodes = List.sort (Int.compare) (extract_nodes edges) in
  if List.mem node all_nodes = false then failwith "node not in graph" else
  (* step two *)
  let rec init_distances nodes n =
    match nodes with
      [] -> []
    | x :: xs ->
      if
        n = x
      then
        (n, 0) :: init_distances xs n
      else
        (x, Int.max_int) :: init_distances xs n in
  let dists = init_distances all_nodes node in
  (* step three *)
  let rec get_distances sptSet queue distances =
    let sorted = List.sort Int.compare sptSet in
    if List.equal (fun a b -> a=b) all_nodes sorted then distances else
    (* 3a *)
    match queue with
        [] -> List.sort_uniq (fun (a,_) (b,_)-> Int.compare a b) distances
    | cur :: xs ->
      if List.mem cur sptSet
      then get_distances sptSet xs distances
    (* 3b *)
      else let set = cur :: sptSet in
    (*3c*)
        let neighbors = get_neighbors edges cur in
        let rec update_distances ns dists q =
          match ns with
            [] -> dists, q
          | neighbor :: nss ->
            if (List.assoc cur dists) + 1 < (List.assoc neighbor dists) then
              let new_dists = (neighbor, (List.assoc cur dists)+1) :: List.filter(fun (a,_) -> a<>neighbor) dists in
              update_distances nss new_dists (neighbor :: q)
            else
              update_distances nss dists q in
        let ds,q = update_distances neighbors distances queue in
        get_distances set q ds in
  get_distances [] [node] dists
