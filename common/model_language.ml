(* High-level operations shared by the finite-HMM interpretations. A finite
   domain supplies the information needed to represent an admissible host
   type. A concrete interpretation may tabulate a conditional distribution's
   row function eagerly. *)

module type MODEL = sig
  type 'a typ
  type weight
  type 'a dist
  type ('a, 'b) cdist

  val make_typ :
    name:string -> cardinal:int -> encode:('a -> int) -> decode:(int -> 'a) -> 'a typ
  val tint_range : name:string -> int -> int typ
  val tpair : 'a typ -> 'b typ -> ('a * 'b) typ
  val tiso :
    name:string -> rep:'a typ -> to_rep:('b -> 'a) -> of_rep:('a -> 'b) -> 'b typ

  val const : float -> weight
  val parameter : Parameter.t -> weight
  val complement : Parameter.t -> weight

  val eta : 'a typ -> 'a -> 'a dist
  val dist : 'a typ -> ('a * weight) list -> 'a dist
  val map : 'b typ -> ('a -> 'b) -> 'a dist -> 'b dist
  val map2 : 'c typ -> ('a -> 'b -> 'c) -> 'a dist -> 'b dist -> 'c dist
  val cdist : 'a typ -> 'b typ -> ('a -> 'b dist) -> ('a, 'b) cdist
  val capp : 'a dist -> ('a, 'b) cdist -> 'b dist
end

(* capp d k represents result(y) = sum_x d(x) * k(x,y). *)
let probability_tolerance = 1e-12
