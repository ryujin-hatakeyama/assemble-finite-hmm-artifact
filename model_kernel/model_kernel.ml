(* Reified finite model kernel. Topology and symbolic model weights are kept
   separate from runtime observations and inference vectors. *)

type edge = {
  target : int;
  probability : Weight.t;
}

type ('a, 'b) t = {
  src : 'a Finite_domain.t;
  dst : 'b Finite_domain.t;
  rows : edge array array;
}

let make ~src ~dst ~rows =
  if Array.length rows <> src.Finite_domain.cardinal then
    invalid_arg
      (Printf.sprintf
         "Model_kernel.make: got %d rows for source domain of cardinality %d"
         (Array.length rows)
         src.Finite_domain.cardinal);
  Array.iteri
    (fun row_index row ->
       let seen = Array.make dst.Finite_domain.cardinal false in
       Array.iter
         (fun (edge : edge) ->
            if edge.target < 0 || edge.target >= dst.Finite_domain.cardinal then
              invalid_arg
                (Printf.sprintf
                   "Model_kernel.make: target %d in row %d is outside destination domain"
                   edge.target
                   row_index);
            if seen.(edge.target) then
              invalid_arg
                (Printf.sprintf
                   "Model_kernel.make: duplicate target %d in row %d"
                   edge.target
                   row_index);
            seen.(edge.target) <- true)
         row)
    rows;
  { src; dst; rows = Array.map Array.copy rows }

let topology k = Array.map (Array.map (fun (e : edge) -> e.target)) k.rows
let nnz k = Array.fold_left (fun acc row -> acc + Array.length row) 0 k.rows

let is_constant k =
  Array.for_all (Array.for_all (fun (e : edge) -> Weight.is_constant e.probability)) k.rows

(* Check that each row-sum polynomial is the constant 1 within the configured
   floating-point tolerance. This is a validation check, not an exact proof. *)
let check_stochastic k =
  Array.iteri (fun i row ->
      let sum =
        Array.fold_left
          (fun acc (e : edge) -> Polynomial.add acc e.probability.Weight.polynomial)
          Polynomial.zero row
      in
      if not (Polynomial.approx_constant ~tolerance:Model_language.probability_tolerance sum 1.0) then
        invalid_arg
          (Printf.sprintf "Model_kernel.check_stochastic: row %d sums to %s"
             i (Polynomial.to_string sum)))
    k.rows

type ('a, 'b) numeric = {
  source : 'a Finite_domain.t;
  destination : 'b Finite_domain.t;
  targets : int array array;
  probabilities : float array array;
}

let validate_probability ~context p =
  if not (p >= 0.0 && p < infinity) then
    invalid_arg
      (Printf.sprintf "%s: %.17g is not a finite non-negative probability" context p)

let validate_row_mass ~context row =
  let mass = Array.fold_left ( +. ) 0.0 row in
  if Float.abs (mass -. 1.0) > Model_language.probability_tolerance then
    invalid_arg
      (Printf.sprintf "%s: row mass %.17g is not 1" context mass)

(* Evaluate symbolic weights for one parameter valuation. The numerical rows
   are checked again for finite non-negative values and unit mass. Static edges
   are retained even when floating-point evaluation produces zero. *)
let instantiate k values =
  Parameter.check_values values;
  let probabilities =
    Array.mapi
      (fun row_index row ->
         let numeric_row =
           Array.mapi
             (fun column_index (e : edge) ->
                let p = Weight.eval values e.probability in
                validate_probability
                  ~context:(Printf.sprintf "Model_kernel.instantiate row %d column %d" row_index column_index)
                  p;
                p)
             row
         in
         validate_row_mass
           ~context:(Printf.sprintf "Model_kernel.instantiate row %d" row_index)
           numeric_row;
         numeric_row)
      k.rows
  in
  { source = k.src;
    destination = k.dst;
    targets = topology k;
    probabilities }

(* Group rows by destination index for observation scoring or pull traversal. *)
let columns (n : ('a, 'b) numeric) =
  let cols = Array.make n.destination.Finite_domain.cardinal [] in
  for x = Array.length n.targets - 1 downto 0 do
    Array.iteri
      (fun c y -> cols.(y) <- (x, n.probabilities.(x).(c)) :: cols.(y))
      n.targets.(x)
  done;
  Array.map Array.of_list cols

(* Reverse the direction of a numerical kernel. *)
let transpose (n : ('a, 'b) numeric) : ('b, 'a) numeric =
  let incoming = Array.make n.destination.Finite_domain.cardinal [] in
  for x = Array.length n.targets - 1 downto 0 do
    Array.iteri
      (fun c y -> incoming.(y) <- (x, n.probabilities.(x).(c)) :: incoming.(y))
      n.targets.(x)
  done;
  { source = n.destination;
    destination = n.source;
    targets = Array.map (fun l -> Array.of_list (List.map fst l)) incoming;
    probabilities = Array.map (fun l -> Array.of_list (List.map snd l)) incoming }
