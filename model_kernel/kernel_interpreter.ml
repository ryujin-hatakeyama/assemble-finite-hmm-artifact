(* Interpretation that reifies conditional distributions as Model_kernel.t.
   Model distributions are used only during construction; runtime inference
   vectors are represented separately. *)

type 'a typ = 'a Finite_domain.t
type weight = Weight.t
type 'a dist = { typ : 'a typ; entries : (int * Weight.t) array }      (* sorted, merged, positive *)
type ('a, 'b) cdist = ('a, 'b) Model_kernel.t

let make_typ = Finite_domain.make
let tint_range ~name n = Finite_domain.int_range ~name n
let tpair a b = Finite_domain.product a b
let tiso = Finite_domain.iso

let const = Weight.const
let parameter = Weight.parameter
let complement = Weight.complement

let canonical typ (entries : (int * Weight.t) list) =
  let a = Array.of_list entries in
  Array.stable_sort (fun (i, _) (j, _) -> Int.compare i j) a;
  let out = ref [] in
  Array.iter (fun (i, w) ->
      match !out with
      | (j, v) :: rest when i = j -> out := (j, Weight.add v w) :: rest
      | _ -> out := (i, w) :: !out) a;
  { typ; entries = Array.of_list (List.rev (List.filter (fun (_, w) -> w.Weight.positive) !out)) }

let check_literal_mass typ entries =
  let sum = List.fold_left (fun acc (_, w) -> Polynomial.add acc w.Weight.polynomial) Polynomial.zero entries in
  if not (Polynomial.approx_constant ~tolerance:Model_language.probability_tolerance sum 1.0) then
    invalid_arg (Printf.sprintf "Kernel_interpreter.dist %s: total weight %s is not 1"
                   (Finite_domain.name typ) (Polynomial.to_string sum))

let eta typ x = { typ; entries = [| (Finite_domain.encode typ x, Weight.const 1.0) |] }

let dist typ entries =
  check_literal_mass typ entries;
  canonical typ (List.map (fun (x, w) -> (Finite_domain.encode typ x, w)) entries)

let map typ f d =
  canonical typ (Array.to_list (Array.map (fun (i, w) ->
      (Finite_domain.encode typ (f (Finite_domain.decode d.typ i)), w)) d.entries))

let map2 typ f d1 d2 =
  canonical typ (List.concat_map (fun (i, p) ->
      let x = Finite_domain.decode d1.typ i in
      Array.to_list (Array.map (fun (j, q) ->
          (Finite_domain.encode typ (f x (Finite_domain.decode d2.typ j)), Weight.mul p q)) d2.entries))
      (Array.to_list d1.entries))

let cdist (src : 'a typ) (dst : 'b typ) f : ('a, 'b) cdist =
  let kernel =
    Model_kernel.make ~src ~dst
      ~rows:(Array.init (Finite_domain.cardinal src) (fun i ->
        let d = f (Finite_domain.decode src i) in
        Finite_domain.require_same ~context:"Kernel_interpreter.cdist" dst d.typ;
        Array.map
          (fun (target, probability) -> { Model_kernel.target; probability })
          d.entries))
  in
  Model_kernel.check_stochastic kernel;
  kernel

let capp d (k : ('a, 'b) cdist) =
  Finite_domain.require_same ~context:"Kernel_interpreter.capp" d.typ k.Model_kernel.src;
  canonical k.Model_kernel.dst (List.concat_map (fun (i, p) ->
      Array.to_list (Array.map (fun (e : Model_kernel.edge) -> (e.target, Weight.mul p e.probability))
                       k.Model_kernel.rows.(i))) (Array.to_list d.entries))

let entries d = Array.to_list d.entries

let initial_vector values d =
  Parameter.check_values values;
  let v = Array.make (Finite_domain.cardinal d.typ) 0.0 in
  Array.iter
    (fun (i, w) ->
       let p = Weight.eval values w in
       Model_kernel.validate_probability ~context:"Kernel_interpreter.initial_vector" p;
       if w.Weight.positive && p = 0.0 then
         invalid_arg "Kernel_interpreter.initial_vector: symbolic support lost";
       v.(i) <- p)
    d.entries;
  Model_kernel.validate_row_mass ~context:"Kernel_interpreter.initial_vector" v;
  v
