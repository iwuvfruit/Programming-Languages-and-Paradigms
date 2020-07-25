exception NotImplemented
exception Fail

(* Interface for our representations of metrics. *)
module type METRIC =
sig
  type t

  val unit: t
  val plus: t -> t -> t
  val prod: float -> t -> t
  val toString: t -> string
  val toFloat: t -> float
  val fromFloat: float -> t
end

(* A simple representation of the Hour unit. Note that the type t is abstract.
   Ideally we would use an implementation of the METRIC type to write this
   module.
   The Hour module is hard-coded here for grading purposes.
*)
module Hour:
  sig
    type t
    val toFloat: t -> float
    val fromFloat: float -> t
  end
= struct
  type t = float

  let toFloat x = x
  let fromFloat x = x
end

(* Interface for a module computing speeds using a fixed metric. *)
module type SPEED =
sig
  type s
  type distance

  val speed: distance -> Hour.t -> s
  val average: s list -> s
  val toFloat: s -> float
  val speed_as_float: distance -> float -> float
  val average_as_float: float list -> float
end

(* The type of graphs. *)
type 'a graph = {
  nodes: 'a list;
  edges: ('a * 'a) list
}
  
(* TODO: Implement the functions in this module.
   Do not change the type signature!
*)
module Float: (METRIC with type t = float) =
struct
  type t = float

  (* TODO: Update this value. *)
  let unit = min_float

  (* TODO: Implement the following functions. *)
  let plus x y = raise NotImplemented
  let prod n x = raise NotImplemented
  let toString x = raise NotImplemented
  let toFloat x = raise NotImplemented
  let fromFloat x = raise NotImplemented
end

(* TODO: Use the module Float to create different representations for:
   Meter, KM, Feet, and Miles.
   Uncomment the following and replace the "???" with your solution.
*)
(*
module Meter = ???
module KM = ???
module Feet = ???
module Miles = ???
*)

(* TODO: Implement the functor Speed. *)
module Speed (M: METRIC): (SPEED with type distance = M.t) =
struct
  type s = float
  type distance = M.t

  (* TODO: Implement the following functions. *)
  let speed (m: distance) (h: Hour.t) =
    raise NotImplemented

  let average s =
    raise NotImplemented

  let toFloat s =
    raise NotImplemented

  (* You should not modify this code: it is here for testing purposes. *)
  let speed_as_float m h = toFloat (speed m (Hour.fromFloat h))
  let average_as_float s = toFloat (average s)
end

(* TODO: Use the functor Speed to create modules for computing miles per hour
   and kilometers per hour. Uncomment the following and replace the "???"
   with your solution.
*)
(*
module MilesPerHour = ???
module KMPerHour = ???
*)

(* Do not remove this line from your code: we need it for testing. *)
module TestModule = Speed (Float)

(* TODO: Write some tests for neighbours. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * string list) list = [
]

(* TODO: Implement neighbours. *)
let neighbours g vertex =
  raise NotImplemented

(* TODO: Implement find_path. *)
let find_path g a b =
  let rec aux_node node visited =
    raise NotImplemented

  and aux_list nodes visited =
    raise NotImplemented
  in
  raise NotImplemented

(* TODO: Implement find_path'. *)
let find_path' g a b =
  let rec aux_node node visited fc sc =
    raise NotImplemented

  and aux_list nodes visited fc sc =
    raise NotImplemented
  in
  raise NotImplemented
