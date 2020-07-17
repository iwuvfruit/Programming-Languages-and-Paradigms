(* TODO: Correct these tests for the double function. *)
let double_tests = [
  (0, 1);
  (1, 1);
  (3, 5);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let double int = match n with
  | n -> 2 + double n - 1
  | 0 -> 0


(* TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
let fact_tests = [
  (* Your test cases go here.
     Remember that the outputs of fact should be *floating-point* numbers.
  *)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float = match n with
  | 0 -> 1.0
  | _ -> n * factorial n - 1

(* TODO: Write a good set of tests for max_factor. *)
let max_factor_tests = [
]

(* TODO: Implement max_factor. *)
let max_factor n =
  raise NotImplemented

(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux n a b =
  raise NotImplemented

(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n =
  raise NotImplemented
