exception NotImplemented

(* The type of binary numbers. *)
type bnum =
  | E
  | Zero of bnum
  | One of bnum

(* The type of propositions. *)
type prop =
  | Atom of string
  | Neg of prop
  | Conj of prop * prop
  | Disj of prop * prop
  | Impl of prop * prop
  
(* TODO: Write a good set of tests for psum. *)
let psum_tests = [
]

(* TODO: Implement psum. *)
let rec psum l =
  raise NotImplemented

(* TODO: Write a good set of tests for intToBin. *)
let intToBin_tests = [
]

(* TODO: Implement intToBin. *)
let rec intToBin n =
  raise NotImplemented

(* TODO: Write a good set of tests for binToInt. *)
let binToInt_tests = [
]

(* TODO: Implement binToInt. *)
let rec binToInt b =
  raise NotImplemented

(* TODO: Write a good set of tests for nnf. *)
let nnf_tests = [
]

(* TODO: Implement nnf. *)
let rec nnf p =
  raise NotImplemented
