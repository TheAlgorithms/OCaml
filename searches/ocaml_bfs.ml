(**Breadth first search, OCaml
 this can be done in pure functional or imperatively, as demonstrated below**)
type graph = (int * int) list


(**imperative ocaml IMPERFECT - working on filtering for nodes to add to queue properly**)           
let bfs graph start1 =
  (**empty visited list**)
  let visited = ref [] in
  (**empty queue**)
  let queue = ref [] in
  (**push first elt to queue**)
    queue := [start1];
  while !queue <> [] do
    let v = List.hd !queue in
    (** mark head of queue as visited**)
    (**HERE there should be a list.mem check for no duplicates but its broken so I havent included it**)
    visited := v :: !visited;
    (**update queue to not have head "popping"**)
    queue := List.tl !queue;
    (**add to visited list connected nodes that arent already in visited list**)
    (** something is wrong here, filtering for snd, second in tuple of edges, but not first works  for some cases but has some duplicates
yet mapping first also creates large amount of duplicates; not proper bfs anyway. Left in there to not miss edges but more to be done**)
    List.map (fun w -> visited := w :: !visited) (List.map snd graph |> List.filter (fun w -> w <> v));
  done;
  List.rev !visited;;

(**  (((List.map snd graph)@(List.map fst graph)) **)

(**recursive functional**)
let bfs2 graph start =
  let rec bfs_helper queue visited result =
    match queue with
   (**elts added in reverse order as above, rev them**) | [] -> List.rev result
    | hd::tl ->
       (** head of queue visited, next queue elt!**) if List.mem hd visited then bfs_helper tl visited result
      else
        (**filter for connected edges, one vertex = curr**)
        let neighbors = List.filter (fun (x,y) -> x = hd || y = hd) graph in
        (**nodes in these neighbor edges should be in next stack frame
         so we add them to this new list; curr will be added many times as part of this map but
         we do a List.mem in recursion anyway**)
        let new_nodes = List.map (fun (x,y) -> if x = hd then y else x) neighbors in
        (** update the queue with the new adjacent nodes to curr**)
        let new_queue = tl @ new_nodes in
        (**recursive call**)
        bfs_helper new_queue (hd::visited) (hd::result)
  in bfs_helper [start] [] []




