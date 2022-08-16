(*
    Traverse a Binary Tree
    Pure OCaml implementation of the traversal algorithms
*)

(*
    The following functions traverse a binary tree in preorder, inorder and postorder.
*)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

(* Examples *)

let char_tree =
  Node
    ( 'a',
      Node ('b', Node ('d', Leaf, Leaf), Node ('e', Leaf, Leaf)),
      Node ('c', Leaf, Node ('f', Node ('g', Leaf, Leaf), Leaf)) )

let int_tree = Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))

let string_tree = Node ("a", Node ("b", Leaf, Leaf), Node ("c", Leaf, Leaf))

(* Binary Tree Traversals *)

let rec pre_order f (node : 'a tree) : unit =
  match node with
  | Leaf -> ()
  | Node (value, left, right) ->
      f value;
      pre_order f left;
      pre_order f right

let rec in_order f (node : 'a tree) : unit =
  match node with
  | Leaf -> ()
  | Node (value, left, right) ->
      in_order f left;
      f value;
      in_order f right

let rec post_order f (node : 'a tree) : unit =
  match node with
  | Leaf -> ()
  | Node (value, left, right) ->
      post_order f left;
      post_order f right;
      f value

(* Printer *)
let printer x = print_string (x ^ ", ")

(* Test *)

let () =
  print_endline "Pre Order Traversal:";
  pre_order (fun x -> printer (String.make 1 x)) char_tree;
  print_string "\n";
  pre_order (fun x -> printer (string_of_int x)) int_tree;
  print_string "\n";
  pre_order (fun x -> printer x) string_tree;
  print_string "\n\n";
  print_endline "In Order Traversal:";
  in_order (fun x -> printer (String.make 1 x)) char_tree;
  print_string "\n";
  in_order (fun x -> printer (string_of_int x)) int_tree;
  print_string "\n";
  in_order (fun x -> printer x) string_tree;
  print_string "\n\n";
  print_endline "Post Order Traversal:";
  post_order (fun x -> printer (String.make 1 x)) char_tree;
  print_string "\n";
  post_order (fun x -> printer (string_of_int x)) int_tree;
  print_string "\n";
  post_order (fun x -> printer x) string_tree;
  print_string "\n"