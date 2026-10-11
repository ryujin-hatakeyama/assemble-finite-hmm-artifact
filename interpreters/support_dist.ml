(* Positive-support interpretation. For non-negative weights, positivity of a
   sum is disjunction and positivity of a product is conjunction. Under the
   assumption 0 < p < 1, parameters and their complements are positive. *)

type 'a typ = 'a Finite_domain.t
type weight = bool
type 'a dist = { typ : 'a typ; keys : int array }
type ('a, 'b) cdist = { src : 'a typ; dst : 'b typ; rows : int array array }

let make_typ = Finite_domain.make
let tint_range ~name n = Finite_domain.int_range ~name n
let tpair a b = Finite_domain.product a b
let tiso = Finite_domain.iso

let const x =
  Direct_evaluator.check_float_weight ~context:"Support_dist.const" x;
  x > 0.0
let parameter (_ : Parameter.t) = true
let complement (_ : Parameter.t) = true

let canonical typ keys =
  let a = Array.of_list keys in
  Array.sort Int.compare a;
  let out = ref [] in
  Array.iter (fun i -> match !out with j :: _ when i = j -> () | _ -> out := i :: !out) a;
  { typ; keys = Array.of_list (List.rev !out) }

let eta typ x = { typ; keys = [| Finite_domain.encode typ x |] }
let dist typ entries =
  canonical typ (List.filter_map (fun (x, b) -> if b then Some (Finite_domain.encode typ x) else None) entries)
let map typ f d =
  canonical typ (Array.to_list (Array.map (fun i ->
      Finite_domain.encode typ (f (Finite_domain.decode d.typ i))) d.keys))
let map2 typ f d1 d2 =
  canonical typ (List.concat_map (fun i ->
      let x = Finite_domain.decode d1.typ i in
      Array.to_list (Array.map (fun j -> Finite_domain.encode typ (f x (Finite_domain.decode d2.typ j))) d2.keys))
      (Array.to_list d1.keys))
let cdist (src : 'a typ) (dst : 'b typ) f =
  { src; dst;
    rows = Array.init src.Finite_domain.cardinal (fun i ->
        let d = f (Finite_domain.decode src i) in
        Finite_domain.require_same ~context:"Support_dist.cdist" dst d.typ;
        d.keys) }
let row k x = { typ = k.dst; keys = k.rows.(Finite_domain.encode k.src x) }
let capp d k =
  Finite_domain.require_same ~context:"Support_dist.capp" d.typ k.src;
  canonical k.dst (List.concat_map (fun i -> Array.to_list k.rows.(i)) (Array.to_list d.keys))

let cardinal (t : 'a typ) = t.Finite_domain.cardinal
let index t x = Finite_domain.encode t x
let value_of_index t i = Finite_domain.decode t i
let support d = Array.to_list (Array.map (Finite_domain.decode d.typ) d.keys)
let row_keys k i = k.rows.(i)
