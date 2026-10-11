(* Interfaces used by the direct numerical interpretations. Observation
   scoring and normalization are kept outside the model language because they
   belong to inference rather than model construction. *)

module type MODEL_EVALUATOR = sig
  include Model_language.MODEL

  val cardinal : 'a typ -> int
  val index : 'a typ -> 'a -> int
  val value_of_index : 'a typ -> int -> 'a
  val support : 'a dist -> 'a list
  val row : ('a, 'b) cdist -> 'a -> 'b dist
end

module type PROBABILITY_EVALUATOR = sig
  include MODEL_EVALUATOR with type weight = float

  val to_table : 'a dist -> ('a * float) list
  (* Multiply d(x) by k(x,y) without normalization. *)
  val score_observation : ('a, 'b) cdist -> 'b -> 'a dist -> 'a dist
  (* Return the normalized distribution and its original total mass. *)
  val normalize_with_mass : 'a dist -> 'a dist * float
end

let check_float_weight ~context w =
  if not (w >= 0.0 && w < infinity) then
    invalid_arg (Printf.sprintf "%s: %.17g is not a finite non-negative weight" context w)

let check_mass ~context mass =
  if Float.abs (mass -. 1.0) > Model_language.probability_tolerance then
    invalid_arg (Printf.sprintf "%s: total weight %.17g is not 1" context mass)
