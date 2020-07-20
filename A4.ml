exception NotImplemented

(* The type of ternary trees. *)
type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree * 'a tree

(* An example ternary tree. *)
let t = Node (
    1,
    Node (2, Empty, Empty, Empty),
    Node (3, Empty, Empty, Empty),
    Node (4, Empty, Empty, Empty)
  )

(* A function for constructing leaves of a tree.
   You may find this useful for writing cleaner test cases.
*)
let leaf v = Node (v, Empty, Empty, Empty)

(* Here are some functions that you may find useful for writing your tests.
   They have been modified so that the grader will print, e.g.,
   "(fun x -> x)", instead of just "<fun>".
*)
let identity = fun x -> x
let add1 = fun x -> x + 1

(* The type of linked lists. *)
type 'a llist =
  | Nil
  | Cons of (float * 'a) * 'a lcell * 'a lcell
and 'a lcell = ('a llist) ref

(* Constructing singleton linked lists, i.e. a linked list with just one
   element in it.
*)
let singleton (init: float * 'a): 'a lcell ref =
  let l = ref (Cons (init, ref Nil, ref Nil)) in
  let front = ref l in
  front

(* Some sample linked lists. *)
let empty_list = ref (ref Nil)
let one_element_list = singleton (3.0, "a")
let two_element_list =
  let second = ref Nil in
  let first = ref (Cons ((2.3, "b"), ref Nil, second)) in
  second := Cons ((3.3, "a"), first, ref Nil);
  ref first

(* Converting linked lists to regular OCaml lists. *)
let rec list_of_lcell lcell =
  match !lcell with
  | Nil -> []
  | Cons (d, _, next) -> d :: list_of_lcell next

(* Comparing floating-point numbers. You might find this helpful when writing
   predicates for remove.
*)
let close x y = abs_float (x -. y) < 0.0001
  
(* TODO: Write some test cases for map_tree. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let map_tree_tests: (((int -> int) * int tree) * int tree) list = [
  (* Remember: your test cases should have this form:
     ((f, t), output)
     The following test case asserts that:
       map_tree identity t
     should have the output
       t
  *)
  ((identity, t), t);
]

(* TODO: Implement map_tree. *)
let rec map_tree f t =
  raise NotImplemented

(* TODO: Implement delete_data. *)
let delete_data t =
  raise NotImplemented

(* TODO: Write some test cases for fold_tree. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let fold_tree_tests:
  (((int * int * int * int -> int) * int * int tree) * int) list =
  [
  (* Remember: your test cases should have this form:
     ((f, e, t), output))
     Where:
     - f is a function of type int * int * int * int -> int
     - e has type int
     - t is a tree of type int tree
     - output has type int.
    *)
    (((fun _ -> 0), 3, t), 0)
  ]

(* TODO: Implement fold_tree. *)
let rec fold_tree f e t =
  raise NotImplemented

(* TODO: Write some test cases for size. *)
let size_tests: (int tree * int) list = [
]

(* TODO: Implement size. *)
let size t =
  raise NotImplemented

(* TODO: Write some test cases for reflect. *)
let reflect_tests: (int tree * int tree) list = [
]

(* TODO: Implement reflect. *)
let reflect t =
  raise NotImplemented

(* TODO: Write some test cases for postorder. *)
let postorder_tests: (int tree * int list) list = [
]

(* TODO: Implement postorder. *)
let postorder t =
  raise NotImplemented

(* TODO: Implement add_head. *)
let add_head x head =
  raise NotImplemented

(* TODO: Implement remove. *)
let remove p head =
  let rec remove' ll =
    raise NotImplemented
  in
  remove' !head
