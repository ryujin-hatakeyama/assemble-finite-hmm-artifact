(* Symbolic non-negative model weights. The Boolean field records strict
   positivity compositionally: addition uses or and multiplication uses and.
   Parameters and 1-p are positive because 0 < p < 1. Complement therefore
   accepts a parameter, not an arbitrary expression. *)

type t = {
  polynomial : Polynomial.t;
  positive : bool;
}

let const c =
  if not (c >= 0.0 && c < infinity) then
    invalid_arg (Printf.sprintf "Weight.const: %.17g is not a finite non-negative weight" c);
  { polynomial = Polynomial.const c; positive = c > 0.0 }

let parameter (p : Parameter.t) =
  { polynomial = Polynomial.variable p.index; positive = true }

let complement (p : Parameter.t) =
  { polynomial = Polynomial.one_minus_variable p.index; positive = true }

let add a b =
  { polynomial = Polynomial.add a.polynomial b.polynomial; positive = a.positive || b.positive }

let mul a b =
  { polynomial = Polynomial.mul a.polynomial b.polynomial; positive = a.positive && b.positive }

let eval values w = Polynomial.eval values w.polynomial
let is_constant w = Polynomial.is_constant w.polynomial
