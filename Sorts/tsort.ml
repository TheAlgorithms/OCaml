(* Kahn's algorithm for topological sorting
   See https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm for details.

   The idea:
     0. Create an empty list of sorted nodes (L).
     1. Find nodes that have no incoming edges (dependencies)
        and add them to the initial set S.
        Let's call them isolated nodes.
     2. Identify all nodes that only depend on isolated nodes from the set S.
        Remove those dependencies.
        Move nodes from the set S to the list L.
     3. Nodes that used to depend on original isolated nodes are now themselves isolated.
        Repeat the process until the graph is empty.

     If the graph is not empty at the end of the process, it means there's a cycle
     or a dependency on a non-existent node.
  *)

(* Helper functions *)
let rec remove_from_list x ys =
  match ys with
  | [] -> []
  | y :: ys' ->
    if y = x then ys'
    else y :: (remove_from_list x ys')

let hashtbl_keys h = h |> Hashtbl.to_seq_keys |> List.of_seq

let hashtbl_of_list kvs =
  let h = Hashtbl.create 1024 in
  let () = List.to_seq kvs |> Hashtbl.add_seq h in
  h

(* Actual algorithm *)
let find_isolated_nodes hash =
  let aux id deps acc =
    match deps with
    | [] -> id :: acc
    | _  -> acc
  in Hashtbl.fold aux hash []

let remove_nodes nodes hash =
  List.iter (Hashtbl.remove hash) nodes

let remove_dependency hash dep =
  let aux dep hash id =
    let deps = Hashtbl.find hash id in
    let deps =
      if List.exists ((=) dep) deps then
        remove_from_list dep deps
      else deps
    in
    begin
      Hashtbl.remove hash id;
      Hashtbl.add hash id deps
    end
  in
  let ids = hashtbl_keys hash in
  List.iter (aux dep hash) ids

let tsort nodes =
  let rec sorting_loop deps hash acc =
    match deps with
    | [] -> acc
    | dep :: deps ->
      let () = remove_dependency hash dep in
      let isolated_nodes = find_isolated_nodes hash in
      let () = remove_nodes isolated_nodes hash in
      sorting_loop (List.append deps isolated_nodes) hash (List.append acc isolated_nodes)
  in
  let nodes_hash = hashtbl_of_list nodes in
  let base_nodes = find_isolated_nodes nodes_hash in 
  let () = remove_nodes base_nodes nodes_hash in
  let sorted_node_ids = sorting_loop base_nodes nodes_hash [] in
  let sorted_node_ids = List.append base_nodes sorted_node_ids in
  let remaining_ids = hashtbl_keys nodes_hash in
  match remaining_ids with
  | [] -> Ok sorted_node_ids
  | _  -> Error "Graph contains a cycle or dependencies on non-existend nodes"

(* Example:
     tsort [("foundation", []); ("walls", ["foundation"]); ("roof", ["walls"])];;
*)
   
