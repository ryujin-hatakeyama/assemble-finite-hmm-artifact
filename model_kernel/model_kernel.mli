type edge = {
  target : int;
  probability : Weight.t;
}

type ('a, 'b) t = private {
  src : 'a Finite_domain.t;
  dst : 'b Finite_domain.t;
  rows : edge array array;
}

val make :
  src:'a Finite_domain.t ->
  dst:'b Finite_domain.t ->
  rows:edge array array ->
  ('a, 'b) t

val topology : ('a, 'b) t -> int array array
val nnz : ('a, 'b) t -> int
val is_constant : ('a, 'b) t -> bool
val check_stochastic : ('a, 'b) t -> unit

type ('a, 'b) numeric = {
  source : 'a Finite_domain.t;
  destination : 'b Finite_domain.t;
  targets : int array array;
  probabilities : float array array;
}

val validate_probability : context:string -> float -> unit
val validate_row_mass : context:string -> float array -> unit
val instantiate : ('a, 'b) t -> Parameter.values -> ('a, 'b) numeric
val columns : ('a, 'b) numeric -> (int * float) array array
val transpose : ('a, 'b) numeric -> ('b, 'a) numeric
