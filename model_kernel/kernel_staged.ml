(* MetaOCaml lowerings of a reified finite transition kernel.

   A high-level finite-HMM description is interpreted first as a
   Model_kernel.t. This module does not reconstruct model semantics; it only
   chooses how an already reified kernel is represented in the residual
   inference program.

   Two transition lowerings share the same reified kernel boundary.

   Embedded turns static edge structure into residual syntax. Packed keeps
   static topology in packed arrays and emits a residual loop over those arrays.
   Parameterized entry points additionally residualize each distinct symbolic
   weight as a runtime arithmetic expression over a parameter array. Thus topology and
   symbolic dependency are generation-time data, while parameter values,
   observations, and inference vectors remain runtime data. *)

type 'a code = 'a Trx.code

type transition_lowering =
  | Packed
  | Embedded

type packed_rows = {
  offsets : int array;
  targets : int array;
  probabilities : float array;
}

let rec seq_balanced = function
  | [] -> .< () >.
  | [code] -> code
  | codes ->
      let count = List.length codes in
      let rec split n acc rest =
        if n = 0 then List.rev acc, rest
        else
          match rest with
          | [] -> List.rev acc, []
          | x :: xs -> split (n - 1) (x :: acc) xs
      in
      let left, right = split (count / 2) [] codes in
      .< begin .~(seq_balanced left); .~(seq_balanced right) end >.

let require_constant_transition (kernel : ('s, 's) Model_kernel.t) =
  Finite_domain.require_same
    ~context:"Kernel_staged.compile_push"
    kernel.Model_kernel.src
    kernel.Model_kernel.dst;
  if not (Model_kernel.is_constant kernel) then
    invalid_arg
      "Kernel_staged.compile_push: parameter-dependent transition kernel is not supported yet";
  Model_kernel.instantiate kernel [||]

let pack_rows (transition : ('s, 's) Model_kernel.numeric) =
  let state_count = transition.Model_kernel.source.Finite_domain.cardinal in
  if Array.length transition.Model_kernel.targets <> state_count
     || Array.length transition.Model_kernel.probabilities <> state_count then
    invalid_arg "Kernel_staged.pack_rows: malformed numerical kernel";
  let offsets = Array.make (state_count + 1) 0 in
  for source = 0 to state_count - 1 do
    let targets = transition.Model_kernel.targets.(source) in
    let probabilities = transition.Model_kernel.probabilities.(source) in
    if Array.length targets <> Array.length probabilities then
      invalid_arg
        (Printf.sprintf
           "Kernel_staged.pack_rows: malformed row %d (targets/probabilities length mismatch)"
           source);
    offsets.(source + 1) <- offsets.(source) + Array.length targets
  done;
  let edge_count = offsets.(state_count) in
  let targets = Array.make edge_count 0 in
  let probabilities = Array.make edge_count 0.0 in
  for source = 0 to state_count - 1 do
    let base = offsets.(source) in
    Array.iteri
      (fun column target ->
         targets.(base + column) <- target;
         probabilities.(base + column) <-
           transition.Model_kernel.probabilities.(source).(column))
      transition.Model_kernel.targets.(source)
  done;
  { offsets; targets; probabilities }

type indexed_symbolic_rows = {
  weight_table : Weight.t array;
  row_weight_ids : int array array;
}

type packed_symbolic_rows = {
  symbolic_offsets : int array;
  symbolic_targets : int array;
  symbolic_weight_ids : int array;
}

let rec equal_monomial left right =
  match left, right with
  | [], [] -> true
  | (left_index, left_exponent) :: left_rest,
    (right_index, right_exponent) :: right_rest ->
      Int.equal left_index right_index
      && Int.equal left_exponent right_exponent
      && equal_monomial left_rest right_rest
  | _ -> false

let rec equal_polynomial left right =
  match left, right with
  | [], [] -> true
  | (left_monomial, left_coefficient) :: left_rest,
    (right_monomial, right_coefficient) :: right_rest ->
      equal_monomial left_monomial right_monomial
      && Float.equal left_coefficient right_coefficient
      && equal_polynomial left_rest right_rest
  | _ -> false

let equal_weight left right =
  Bool.equal left.Weight.positive right.Weight.positive
  && equal_polynomial left.Weight.polynomial right.Weight.polynomial

let index_symbolic_rows (kernel : ('s, 's) Model_kernel.t) =
  Finite_domain.require_same
    ~context:"Kernel_staged.index_symbolic_rows"
    kernel.Model_kernel.src
    kernel.Model_kernel.dst;
  let weights = ref [] in
  let intern weight =
    let rec find index = function
      | [] ->
          weights := !weights @ [weight];
          index
      | candidate :: rest ->
          if equal_weight weight candidate then index
          else find (index + 1) rest
    in
    find 0 !weights
  in
  let row_weight_ids =
    Array.map
      (Array.map (fun (edge : Model_kernel.edge) -> intern edge.probability))
      kernel.Model_kernel.rows
  in
  { weight_table = Array.of_list !weights; row_weight_ids }

let pack_symbolic_rows
    (kernel : ('s, 's) Model_kernel.t)
    (indexed : indexed_symbolic_rows) =
  let state_count = kernel.Model_kernel.src.Finite_domain.cardinal in
  if Array.length kernel.Model_kernel.rows <> state_count
     || Array.length indexed.row_weight_ids <> state_count then
    invalid_arg "Kernel_staged.pack_symbolic_rows: malformed symbolic kernel";
  let symbolic_offsets = Array.make (state_count + 1) 0 in
  for source = 0 to state_count - 1 do
    let row = kernel.Model_kernel.rows.(source) in
    let weight_ids = indexed.row_weight_ids.(source) in
    if Array.length row <> Array.length weight_ids then
      invalid_arg
        (Printf.sprintf
           "Kernel_staged.pack_symbolic_rows: malformed row %d"
           source);
    symbolic_offsets.(source + 1) <-
      symbolic_offsets.(source) + Array.length row
  done;
  let edge_count = symbolic_offsets.(state_count) in
  let symbolic_targets = Array.make edge_count 0 in
  let symbolic_weight_ids = Array.make edge_count 0 in
  for source = 0 to state_count - 1 do
    let base = symbolic_offsets.(source) in
    Array.iteri
      (fun column (edge : Model_kernel.edge) ->
         symbolic_targets.(base + column) <- edge.target;
         symbolic_weight_ids.(base + column) <-
           indexed.row_weight_ids.(source).(column))
      kernel.Model_kernel.rows.(source)
  done;
  { symbolic_offsets; symbolic_targets; symbolic_weight_ids }

let parameter_count (weights : Weight.t array) =
  let largest = ref (-1) in
  Array.iter
    (fun weight ->
       List.iter
         (fun (monomial, _coefficient) ->
            List.iter
              (fun (index, _exponent) -> largest := Int.max !largest index)
              monomial)
         weight.Weight.polynomial)
    weights;
  !largest + 1

let emit_parameter_power
    ~(parameters : float array code)
    ~(index : int)
    ~(exponent : int) : float code =
  if exponent = 1 then
    .< Array.unsafe_get .~parameters index >.
  else
    let exponent = float_of_int exponent in
    .< (Array.unsafe_get .~parameters index) ** exponent >.

let emit_polynomial
    ~(parameters : float array code)
    (polynomial : Polynomial.t) : float code =
  List.fold_left
    (fun sum (monomial, coefficient) ->
       let term =
         List.fold_left
           (fun product (index, exponent) ->
              .< .~product *.
                 .~(emit_parameter_power ~parameters ~index ~exponent) >.)
           .< coefficient >.
           monomial
       in
       .< .~sum +. .~term >.)
    .< 0.0 >.
    polynomial

let rec emit_weight_list
    ~(parameters : float array code)
    (weights : Weight.t list) : float list code =
  match weights with
  | [] -> .< [] >.
  | weight :: rest ->
      .< .~(emit_polynomial ~parameters weight.Weight.polynomial)
         :: .~(emit_weight_list ~parameters rest) >.

let emit_weight_values
    ~(parameters : float array code)
    (weights : Weight.t array) : float array code =
  .< Array.of_list
       .~(emit_weight_list ~parameters (Array.to_list weights)) >.

let emit_edge_code
    ~(next : float array code)
    ~(source_weight : float code)
    ~(target : int)
    ~(probability : float code) : unit code =
  .<
    Array.unsafe_set
      .~next
      target
      (Array.unsafe_get .~next target +. .~source_weight *. .~probability)
  >.

let emit_symbolic_row
    ~(weight_values : float array code)
    ~(current : float array code)
    ~(next : float array code)
    ~(source : int)
    ~(row : Model_kernel.edge array)
    ~(weight_ids : int array) : unit code =
  if Array.length row <> Array.length weight_ids then
    invalid_arg
      (Printf.sprintf
         "Kernel_staged.emit_symbolic_row: malformed row %d"
         source);
  let source_weight = .< Array.unsafe_get .~current source >. in
  let body source_weight =
    List.init
      (Array.length row)
      (fun column ->
         let edge = row.(column) in
         let weight_id = weight_ids.(column) in
         emit_edge_code
           ~next
           ~source_weight
           ~target:edge.Model_kernel.target
           ~probability:.< Array.unsafe_get .~weight_values weight_id >.)
    |> seq_balanced
  in
  .<
    let p = .~source_weight in
    if p > 0.0 then
      .~(body .< p >.)
  >.

let emit_embedded_symbolic_transition
    ~(kernel : ('s, 's) Model_kernel.t)
    ~(indexed : indexed_symbolic_rows)
    ~(weight_values : float array code)
    ~(current : float array code)
    ~(next : float array code) =
  let state_count = kernel.Model_kernel.src.Finite_domain.cardinal in
  List.init state_count (fun source ->
      emit_symbolic_row
        ~weight_values
        ~current
        ~next
        ~source
        ~row:kernel.Model_kernel.rows.(source)
        ~weight_ids:indexed.row_weight_ids.(source))
  |> seq_balanced

let emit_packed_symbolic_transition
    ~(state_count : int)
    ~(offsets : int array code)
    ~(targets : int array code)
    ~(weight_ids : int array code)
    ~(weight_values : float array code)
    ~(current : float array code)
    ~(next : float array code) =
  .<
    for source = 0 to state_count - 1 do
      let source_weight = Array.unsafe_get .~current source in
      if source_weight > 0.0 then begin
        let first = Array.unsafe_get .~offsets source in
        let last = Array.unsafe_get .~offsets (source + 1) in
        for edge = first to last - 1 do
          let target = Array.unsafe_get .~targets edge in
          let weight_id = Array.unsafe_get .~weight_ids edge in
          let probability = Array.unsafe_get .~weight_values weight_id in
          Array.unsafe_set
            .~next
            target
            (Array.unsafe_get .~next target +. source_weight *. probability)
        done
      end
    done
  >.

let emit_edge ~(next : float array code) ~(source_weight : float code)
    ~(target : int) ~(probability : float) : unit code =
  if probability = 0.0 then
    .< () >.
  else
    .<
      Array.unsafe_set
        .~next
        target
        (Array.unsafe_get .~next target +. .~source_weight *. probability)
    >.

let emit_row ~(current : float array code) ~(next : float array code)
    ~(source : int) ~(targets : int array) ~(probabilities : float array) : unit code =
  if Array.length targets <> Array.length probabilities then
    invalid_arg
      (Printf.sprintf
         "Kernel_staged.compile_push: malformed row %d (targets/probabilities length mismatch)"
         source);
  let source_weight = .< Array.unsafe_get .~current source >. in
  let body source_weight =
    List.init
      (Array.length targets)
      (fun column ->
         emit_edge
           ~next
           ~source_weight
           ~target:targets.(column)
           ~probability:probabilities.(column))
    |> seq_balanced
  in
  .<
    let p = .~source_weight in
    if p > 0.0 then
      .~(body .< p >.)
  >.

let emit_embedded_transition
    ~(transition : ('s, 's) Model_kernel.numeric)
    ~(current : float array code)
    ~(next : float array code) =
  let state_count = transition.Model_kernel.source.Finite_domain.cardinal in
  List.init state_count (fun source ->
      emit_row
        ~current
        ~next
        ~source
        ~targets:transition.Model_kernel.targets.(source)
        ~probabilities:transition.Model_kernel.probabilities.(source))
  |> seq_balanced

let emit_packed_transition
    ~(state_count : int)
    ~(offsets : int array code)
    ~(targets : int array code)
    ~(probabilities : float array code)
    ~(current : float array code)
    ~(next : float array code) =
  .<
    for source = 0 to state_count - 1 do
      let source_weight = Array.unsafe_get .~current source in
      if source_weight > 0.0 then begin
        let first = Array.unsafe_get .~offsets source in
        let last = Array.unsafe_get .~offsets (source + 1) in
        for edge = first to last - 1 do
          let target = Array.unsafe_get .~targets edge in
          let probability = Array.unsafe_get .~probabilities edge in
          Array.unsafe_set
            .~next
            target
            (Array.unsafe_get .~next target +. source_weight *. probability)
        done
      end
    done
  >.

(* The generated core always takes packed-array arguments. Embedded ignores
   them and emits the static updates directly. Packed emits the residual
   loops above.  Keeping one residual interface makes the public wrapper and the
   Forward skeleton independent of the representation choice. *)
let compile_push_core_code
    ~(lowering : transition_lowering)
    (transition : ('s, 's) Model_kernel.numeric)
    : (int array -> int array -> float array ->
       float array -> float array -> unit) code =
  let state_count = transition.Model_kernel.source.Finite_domain.cardinal in
  let emit offsets targets probabilities current next =
    match lowering with
    | Embedded ->
        emit_embedded_transition ~transition ~current ~next
    | Packed ->
        emit_packed_transition
          ~state_count
          ~offsets
          ~targets
          ~probabilities
          ~current
          ~next
  in
  .<
    fun transition_offsets transition_targets transition_probabilities current next ->
      if Array.length current <> state_count then
        invalid_arg
          (Printf.sprintf
             "Kernel_staged.push: current vector length %d, expected %d"
             (Array.length current)
             state_count);
      if Array.length next <> state_count then
        invalid_arg
          (Printf.sprintf
             "Kernel_staged.push: next vector length %d, expected %d"
             (Array.length next)
             state_count);
      Array.fill next 0 state_count 0.0;
      .~(emit
           .< transition_offsets >.
           .< transition_targets >.
           .< transition_probabilities >.
           .< current >.
           .< next >.)
  >.

let compile_push ?(lowering = Embedded) kernel =
  let transition = require_constant_transition kernel in
  let packed =
    match lowering with
    | Packed -> pack_rows transition
    | Embedded -> { offsets = [||]; targets = [||]; probabilities = [||] }
  in
  let core = Runcode.run (compile_push_core_code ~lowering transition) in
  fun current next ->
    core packed.offsets packed.targets packed.probabilities current next

let compile_push_parameterized_core_code
    ~(lowering : transition_lowering)
    ~(indexed : indexed_symbolic_rows)
    (transition : ('s, 's) Model_kernel.t)
    : (float array -> int array -> int array -> int array ->
       float array -> float array -> unit) code =
  let state_count = transition.Model_kernel.src.Finite_domain.cardinal in
  let emit offsets targets weight_ids weight_values current next =
    match lowering with
    | Embedded ->
        emit_embedded_symbolic_transition
          ~kernel:transition
          ~indexed
          ~weight_values
          ~current
          ~next
    | Packed ->
        emit_packed_symbolic_transition
          ~state_count
          ~offsets
          ~targets
          ~weight_ids
          ~weight_values
          ~current
          ~next
  in
  .<
    fun parameters transition_offsets transition_targets transition_weight_ids
        current next ->
      if Array.length current <> state_count then
        invalid_arg
          (Printf.sprintf
             "Kernel_staged.push_parameterized: current vector length %d, expected %d"
             (Array.length current)
             state_count);
      if Array.length next <> state_count then
        invalid_arg
          (Printf.sprintf
             "Kernel_staged.push_parameterized: next vector length %d, expected %d"
             (Array.length next)
             state_count);
      let weight_values =
        .~(emit_weight_values
             ~parameters:.< parameters >.
             indexed.weight_table)
      in
      Array.fill next 0 state_count 0.0;
      .~(emit
           .< transition_offsets >.
           .< transition_targets >.
           .< transition_weight_ids >.
           .< weight_values >.
           .< current >.
           .< next >.)
  >.

let compile_push_parameterized ?(lowering = Embedded) kernel =
  Finite_domain.require_same
    ~context:"Kernel_staged.compile_push_parameterized"
    kernel.Model_kernel.src
    kernel.Model_kernel.dst;
  let indexed = index_symbolic_rows kernel in
  let required_parameters = parameter_count indexed.weight_table in
  let packed =
    match lowering with
    | Packed -> pack_symbolic_rows kernel indexed
    | Embedded ->
        { symbolic_offsets = [||];
          symbolic_targets = [||];
          symbolic_weight_ids = [||] }
  in
  let core =
    Runcode.run
      (compile_push_parameterized_core_code ~lowering ~indexed kernel)
  in
  fun parameters current next ->
    if Array.length parameters < required_parameters then
      invalid_arg
        (Printf.sprintf
           "Kernel_staged.push_parameterized: parameter vector length %d, expected at least %d"
           (Array.length parameters)
           required_parameters);
    Parameter.check_values parameters;
    core
      parameters
      packed.symbolic_offsets
      packed.symbolic_targets
      packed.symbolic_weight_ids
      current
      next

(* Column-packed emission data.  Emission lowering is intentionally held fixed
   while transition representation is varied.  This isolates the experiment:
   Packed versus Embedded below changes only the transition lowering. *)
type packed_columns = {
  column_offsets : int array;
  states : int array;
  emission_probabilities : float array;
}

let pack_columns (emission : ('s, 'o) Model_kernel.numeric) =
  let columns = Model_kernel.columns emission in
  let observation_count = Array.length columns in
  let column_offsets = Array.make (observation_count + 1) 0 in
  for observation = 0 to observation_count - 1 do
    column_offsets.(observation + 1) <-
      column_offsets.(observation) + Array.length columns.(observation)
  done;
  let entry_count = column_offsets.(observation_count) in
  let states = Array.make entry_count 0 in
  let emission_probabilities = Array.make entry_count 0.0 in
  Array.iteri
    (fun observation column ->
       let base = column_offsets.(observation) in
       Array.iteri
         (fun column_index (state, probability) ->
            states.(base + column_index) <- state;
            emission_probabilities.(base + column_index) <- probability)
         column)
    columns;
  { column_offsets; states; emission_probabilities }

let require_constant_emission
    ~(transition : ('s, 's) Model_kernel.t)
    (emission : ('s, 'o) Model_kernel.t) =
  Finite_domain.require_same
    ~context:"Kernel_staged.compile_forward"
    transition.Model_kernel.dst
    emission.Model_kernel.src;
  if not (Model_kernel.is_constant emission) then
    invalid_arg
      "Kernel_staged.compile_forward: parameter-dependent emission kernel is not supported yet";
  Model_kernel.instantiate emission [||]

let emit_score_packed
    ~(state_count : int)
    ~(observation_count : int)
    ~(source_values : float array code)
    ~(scored : float array code)
    ~(offsets : int array code)
    ~(states : int array code)
    ~(probabilities : float array code)
    ~(observation : int code) : unit code =
  .<
    Array.fill .~scored 0 state_count 0.0;
    let observation = .~observation in
    if observation < 0 || observation >= observation_count then
      invalid_arg
        (Printf.sprintf
           "Kernel_staged.forward: observation index %d outside [0,%d)"
           observation
           observation_count);
    let first = Array.unsafe_get .~offsets observation in
    let last = Array.unsafe_get .~offsets (observation + 1) in
    for entry = first to last - 1 do
      let state = Array.unsafe_get .~states entry in
      Array.unsafe_set
        .~scored
        state
        (Array.unsafe_get .~source_values state
         *. Array.unsafe_get .~probabilities entry)
    done
  >.

let emit_sum_rescale ~(state_count : int) (values : float array code)
    : float code =
  .<
    let mass = ref 0.0 in
    for state = 0 to state_count - 1 do
      mass := !mass +. Array.unsafe_get .~values state
    done;
    let mass = !mass in
    if mass > 0.0 then begin
      let reciprocal = 1.0 /. mass in
      for state = 0 to state_count - 1 do
        Array.unsafe_set
          .~values
          state
          (Array.unsafe_get .~values state *. reciprocal)
      done
    end;
    mass
  >.

(* Generate one scaled probability-domain Forward skeleton.  The only varying
   component is the transition lowering.  Initial distribution and emission
   columns are fixed by compile_forward; observations remain runtime data. *)
let compile_forward_indices_code
    ~(lowering : transition_lowering)
    ~(observation_count : int)
    (transition : ('s, 's) Model_kernel.numeric)
    : (float array ->
       int array -> int array -> float array ->
       int array -> int array -> float array ->
       int array -> float array * float) code =
  let state_count = transition.Model_kernel.source.Finite_domain.cardinal in
  let emit_transition offsets targets probabilities current predicted =
    match lowering with
    | Embedded ->
        emit_embedded_transition ~transition ~current ~next:predicted
    | Packed ->
        emit_packed_transition
          ~state_count
          ~offsets
          ~targets
          ~probabilities
          ~current
          ~next:predicted
  in
  .<
    fun initial
        transition_offsets transition_targets transition_probabilities
        emission_offsets emission_states emission_probabilities observations ->
      if Array.length initial <> state_count then
        invalid_arg
          (Printf.sprintf
             "Kernel_staged.forward: initial vector length %d, expected %d"
             (Array.length initial)
             state_count);
      if Array.length emission_offsets <> observation_count + 1 then
        invalid_arg "Kernel_staged.forward: malformed emission offsets";
      if Array.length emission_states <> Array.length emission_probabilities then
        invalid_arg "Kernel_staged.forward: malformed emission entries";
      let length = Array.length observations in
      if length = 0 then
        invalid_arg "Kernel_staged.forward: empty observation sequence";
      let current = Array.make state_count 0.0 in
      let predicted = Array.make state_count 0.0 in
      .~(emit_score_packed
           ~state_count
           ~observation_count
           ~source_values:.< initial >.
           ~scored:.< current >.
           ~offsets:.< emission_offsets >.
           ~states:.< emission_states >.
           ~probabilities:.< emission_probabilities >.
           ~observation:.< Array.unsafe_get observations 0 >.);
      let first_mass = .~(emit_sum_rescale ~state_count .< current >.) in
      let log_score = ref (log first_mass) in
      for time = 1 to length - 1 do
        Array.fill predicted 0 state_count 0.0;
        .~(emit_transition
             .< transition_offsets >.
             .< transition_targets >.
             .< transition_probabilities >.
             .< current >.
             .< predicted >.);
        .~(emit_score_packed
             ~state_count
             ~observation_count
             ~source_values:.< predicted >.
             ~scored:.< current >.
             ~offsets:.< emission_offsets >.
             ~states:.< emission_states >.
             ~probabilities:.< emission_probabilities >.
             ~observation:.< Array.unsafe_get observations time >.);
        let mass = .~(emit_sum_rescale ~state_count .< current >.) in
        log_score := !log_score +. log mass
      done;
      (current, !log_score)
  >.

let compile_forward
    ?(lowering = Embedded)
    ~(initial : float array)
    ~(transition : ('s, 's) Model_kernel.t)
    ~(emission : ('s, 'o) Model_kernel.t) =
  Finite_domain.require_same
    ~context:"Kernel_staged.compile_forward"
    transition.Model_kernel.src
    transition.Model_kernel.dst;
  let state_count = transition.Model_kernel.src.Finite_domain.cardinal in
  if Array.length initial <> state_count then
    invalid_arg
      (Printf.sprintf
         "Kernel_staged.compile_forward: initial vector length %d, expected %d"
         (Array.length initial)
         state_count);
  Array.iteri
    (fun state probability ->
       Model_kernel.validate_probability
         ~context:(Printf.sprintf "Kernel_staged.compile_forward initial state %d" state)
         probability)
    initial;
  Model_kernel.validate_row_mass
    ~context:"Kernel_staged.compile_forward initial"
    initial;
  let numeric_transition = require_constant_transition transition in
  let packed_transition =
    match lowering with
    | Packed -> pack_rows numeric_transition
    | Embedded -> { offsets = [||]; targets = [||]; probabilities = [||] }
  in
  let numeric_emission = require_constant_emission ~transition emission in
  let packed_emission = pack_columns numeric_emission in
  let observation_count = emission.Model_kernel.dst.Finite_domain.cardinal in
  let core =
    Runcode.run
      (compile_forward_indices_code
         ~lowering
         ~observation_count
         numeric_transition)
  in
  let initial = Array.copy initial in
  let encode_observation = Finite_domain.encode emission.Model_kernel.dst in
  fun observations ->
    let observation_indices = Array.map encode_observation observations in
    core
      initial
      packed_transition.offsets
      packed_transition.targets
      packed_transition.probabilities
      packed_emission.column_offsets
      packed_emission.states
      packed_emission.emission_probabilities
      observation_indices


(* Parameterized Forward keeps the transition topology static while residualizing
   distinct symbolic transition weights as runtime expressions over parameters.
   Initial and emission weights remain generation-time constants. The returned
   function can therefore be reused for many parameter valuations without
   regenerating code. *)
let compile_forward_parameterized_indices_code
    ~(lowering : transition_lowering)
    ~(observation_count : int)
    ~(indexed : indexed_symbolic_rows)
    (transition : ('s, 's) Model_kernel.t)
    : (float array -> float array ->
       int array -> int array -> int array ->
       int array -> int array -> float array ->
       int array -> float array * float) code =
  let state_count = transition.Model_kernel.src.Finite_domain.cardinal in
  let emit_transition offsets targets weight_ids weight_values current predicted =
    match lowering with
    | Embedded ->
        emit_embedded_symbolic_transition
          ~kernel:transition
          ~indexed
          ~weight_values
          ~current
          ~next:predicted
    | Packed ->
        emit_packed_symbolic_transition
          ~state_count
          ~offsets
          ~targets
          ~weight_ids
          ~weight_values
          ~current
          ~next:predicted
  in
  .<
    fun parameters initial
        transition_offsets transition_targets transition_weight_ids
        emission_offsets emission_states emission_probabilities observations ->
      if Array.length initial <> state_count then
        invalid_arg
          (Printf.sprintf
             "Kernel_staged.forward_parameterized: initial vector length %d, expected %d"
             (Array.length initial)
             state_count);
      if Array.length emission_offsets <> observation_count + 1 then
        invalid_arg "Kernel_staged.forward_parameterized: malformed emission offsets";
      if Array.length emission_states <> Array.length emission_probabilities then
        invalid_arg "Kernel_staged.forward_parameterized: malformed emission entries";
      let length = Array.length observations in
      if length = 0 then
        invalid_arg "Kernel_staged.forward_parameterized: empty observation sequence";
      let weight_values =
        .~(emit_weight_values
             ~parameters:.< parameters >.
             indexed.weight_table)
      in
      let current = Array.make state_count 0.0 in
      let predicted = Array.make state_count 0.0 in
      .~(emit_score_packed
           ~state_count
           ~observation_count
           ~source_values:.< initial >.
           ~scored:.< current >.
           ~offsets:.< emission_offsets >.
           ~states:.< emission_states >.
           ~probabilities:.< emission_probabilities >.
           ~observation:.< Array.unsafe_get observations 0 >.);
      let first_mass = .~(emit_sum_rescale ~state_count .< current >.) in
      let log_score = ref (log first_mass) in
      for time = 1 to length - 1 do
        Array.fill predicted 0 state_count 0.0;
        .~(emit_transition
             .< transition_offsets >.
             .< transition_targets >.
             .< transition_weight_ids >.
             .< weight_values >.
             .< current >.
             .< predicted >.);
        .~(emit_score_packed
             ~state_count
             ~observation_count
             ~source_values:.< predicted >.
             ~scored:.< current >.
             ~offsets:.< emission_offsets >.
             ~states:.< emission_states >.
             ~probabilities:.< emission_probabilities >.
             ~observation:.< Array.unsafe_get observations time >.);
        let mass = .~(emit_sum_rescale ~state_count .< current >.) in
        log_score := !log_score +. log mass
      done;
      (current, !log_score)
  >.

let compile_forward_parameterized
    ?(lowering = Embedded)
    ~(initial : float array)
    ~(transition : ('s, 's) Model_kernel.t)
    ~(emission : ('s, 'o) Model_kernel.t) =
  Finite_domain.require_same
    ~context:"Kernel_staged.compile_forward_parameterized"
    transition.Model_kernel.src
    transition.Model_kernel.dst;
  let state_count = transition.Model_kernel.src.Finite_domain.cardinal in
  if Array.length initial <> state_count then
    invalid_arg
      (Printf.sprintf
         "Kernel_staged.compile_forward_parameterized: initial vector length %d, expected %d"
         (Array.length initial)
         state_count);
  Array.iteri
    (fun state probability ->
       Model_kernel.validate_probability
         ~context:(Printf.sprintf
             "Kernel_staged.compile_forward_parameterized initial state %d"
             state)
         probability)
    initial;
  Model_kernel.validate_row_mass
    ~context:"Kernel_staged.compile_forward_parameterized initial"
    initial;
  let indexed = index_symbolic_rows transition in
  let required_parameters = parameter_count indexed.weight_table in
  let packed_transition =
    match lowering with
    | Packed -> pack_symbolic_rows transition indexed
    | Embedded ->
        { symbolic_offsets = [||];
          symbolic_targets = [||];
          symbolic_weight_ids = [||] }
  in
  let numeric_emission = require_constant_emission ~transition emission in
  let packed_emission = pack_columns numeric_emission in
  let observation_count = emission.Model_kernel.dst.Finite_domain.cardinal in
  let core =
    Runcode.run
      (compile_forward_parameterized_indices_code
         ~lowering
         ~observation_count
         ~indexed
         transition)
  in
  let initial = Array.copy initial in
  let encode_observation = Finite_domain.encode emission.Model_kernel.dst in
  fun parameters observations ->
    if Array.length parameters < required_parameters then
      invalid_arg
        (Printf.sprintf
           "Kernel_staged.forward_parameterized: parameter vector length %d, expected at least %d"
           (Array.length parameters)
           required_parameters);
    Parameter.check_values parameters;
    let observation_indices = Array.map encode_observation observations in
    core
      parameters
      initial
      packed_transition.symbolic_offsets
      packed_transition.symbolic_targets
      packed_transition.symbolic_weight_ids
      packed_emission.column_offsets
      packed_emission.states
      packed_emission.emission_probabilities
      observation_indices

(* Viterbi reuses exactly the same reified HMM kernel, transition lowering,
   emission representation, and parameter residualization as Forward.  The
   only inference-level change is the destination reducer: predecessor
   contributions are combined by strict maximum while recording the source that
   attained the maximum.  Model-internal path aggregation has already happened
   when Model_kernel.t was constructed and therefore remains probabilistic
   addition. *)

let emit_viterbi_edge
    ~(values : float array code)
    ~(backpointers : int array code)
    ~(source : int)
    ~(source_weight : float code)
    ~(target : int)
    ~(probability : float) : unit code =
  if probability = 0.0 then
    .< () >.
  else
    .<
      let candidate = .~source_weight *. probability in
      if candidate > Array.unsafe_get .~values target then begin
        Array.unsafe_set .~values target candidate;
        Array.unsafe_set .~backpointers target source
      end
    >.

let emit_viterbi_symbolic_edge
    ~(values : float array code)
    ~(backpointers : int array code)
    ~(source : int)
    ~(source_weight : float code)
    ~(target : int)
    ~(probability : float code) : unit code =
  .<
    let candidate = .~source_weight *. .~probability in
    if candidate > Array.unsafe_get .~values target then begin
      Array.unsafe_set .~values target candidate;
      Array.unsafe_set .~backpointers target source
    end
  >.

let emit_viterbi_row
    ~(current : float array code)
    ~(predicted : float array code)
    ~(backpointers : int array code)
    ~(source : int)
    ~(targets : int array)
    ~(probabilities : float array) : unit code =
  if Array.length targets <> Array.length probabilities then
    invalid_arg
      (Printf.sprintf
         "Kernel_staged.compile_viterbi: malformed row %d (targets/probabilities length mismatch)"
         source);
  let source_weight = .< Array.unsafe_get .~current source >. in
  let body source_weight =
    List.init
      (Array.length targets)
      (fun column ->
         emit_viterbi_edge
           ~values:predicted
           ~backpointers
           ~source
           ~source_weight
           ~target:targets.(column)
           ~probability:probabilities.(column))
    |> seq_balanced
  in
  .<
    let p = .~source_weight in
    if p > 0.0 then
      .~(body .< p >.)
  >.

let emit_embedded_viterbi_transition
    ~(transition : ('s, 's) Model_kernel.numeric)
    ~(current : float array code)
    ~(predicted : float array code)
    ~(backpointers : int array code) =
  let state_count = transition.Model_kernel.source.Finite_domain.cardinal in
  List.init state_count (fun source ->
      emit_viterbi_row
        ~current
        ~predicted
        ~backpointers
        ~source
        ~targets:transition.Model_kernel.targets.(source)
        ~probabilities:transition.Model_kernel.probabilities.(source))
  |> seq_balanced

let emit_packed_viterbi_transition
    ~(state_count : int)
    ~(offsets : int array code)
    ~(targets : int array code)
    ~(probabilities : float array code)
    ~(current : float array code)
    ~(predicted : float array code)
    ~(backpointers : int array code) =
  .<
    for source = 0 to state_count - 1 do
      let source_weight = Array.unsafe_get .~current source in
      if source_weight > 0.0 then begin
        let first = Array.unsafe_get .~offsets source in
        let last = Array.unsafe_get .~offsets (source + 1) in
        for edge = first to last - 1 do
          let target = Array.unsafe_get .~targets edge in
          let candidate =
            source_weight *. Array.unsafe_get .~probabilities edge
          in
          if candidate > Array.unsafe_get .~predicted target then begin
            Array.unsafe_set .~predicted target candidate;
            Array.unsafe_set .~backpointers target source
          end
        done
      end
    done
  >.

let emit_symbolic_viterbi_row
    ~(weight_values : float array code)
    ~(current : float array code)
    ~(predicted : float array code)
    ~(backpointers : int array code)
    ~(source : int)
    ~(row : Model_kernel.edge array)
    ~(weight_ids : int array) : unit code =
  if Array.length row <> Array.length weight_ids then
    invalid_arg
      (Printf.sprintf
         "Kernel_staged.emit_symbolic_viterbi_row: malformed row %d"
         source);
  let source_weight = .< Array.unsafe_get .~current source >. in
  let body source_weight =
    List.init
      (Array.length row)
      (fun column ->
         let edge = row.(column) in
         let weight_id = weight_ids.(column) in
         emit_viterbi_symbolic_edge
           ~values:predicted
           ~backpointers
           ~source
           ~source_weight
           ~target:edge.Model_kernel.target
           ~probability:.< Array.unsafe_get .~weight_values weight_id >.)
    |> seq_balanced
  in
  .<
    let p = .~source_weight in
    if p > 0.0 then
      .~(body .< p >.)
  >.

let emit_embedded_symbolic_viterbi_transition
    ~(kernel : ('s, 's) Model_kernel.t)
    ~(indexed : indexed_symbolic_rows)
    ~(weight_values : float array code)
    ~(current : float array code)
    ~(predicted : float array code)
    ~(backpointers : int array code) =
  let state_count = kernel.Model_kernel.src.Finite_domain.cardinal in
  List.init state_count (fun source ->
      emit_symbolic_viterbi_row
        ~weight_values
        ~current
        ~predicted
        ~backpointers
        ~source
        ~row:kernel.Model_kernel.rows.(source)
        ~weight_ids:indexed.row_weight_ids.(source))
  |> seq_balanced

let emit_packed_symbolic_viterbi_transition
    ~(state_count : int)
    ~(offsets : int array code)
    ~(targets : int array code)
    ~(weight_ids : int array code)
    ~(weight_values : float array code)
    ~(current : float array code)
    ~(predicted : float array code)
    ~(backpointers : int array code) =
  .<
    for source = 0 to state_count - 1 do
      let source_weight = Array.unsafe_get .~current source in
      if source_weight > 0.0 then begin
        let first = Array.unsafe_get .~offsets source in
        let last = Array.unsafe_get .~offsets (source + 1) in
        for edge = first to last - 1 do
          let target = Array.unsafe_get .~targets edge in
          let weight_id = Array.unsafe_get .~weight_ids edge in
          let candidate =
            source_weight *. Array.unsafe_get .~weight_values weight_id
          in
          if candidate > Array.unsafe_get .~predicted target then begin
            Array.unsafe_set .~predicted target candidate;
            Array.unsafe_set .~backpointers target source
          end
        done
      end
    done
  >.

let emit_max_rescale ~(state_count : int) (values : float array code)
    : float code =
  .<
    let maximum = ref 0.0 in
    for state = 0 to state_count - 1 do
      let value = Array.unsafe_get .~values state in
      if value > !maximum then maximum := value
    done;
    let maximum = !maximum in
    if maximum > 0.0 then begin
      let reciprocal = 1.0 /. maximum in
      for state = 0 to state_count - 1 do
        Array.unsafe_set
          .~values
          state
          (Array.unsafe_get .~values state *. reciprocal)
      done
    end;
    maximum
  >.

let emit_viterbi_path
    ~(state_count : int)
    ~(length : int code)
    ~(last : float array code)
    ~(backpointers : int array array code) : int array code =
  .<
    let best = ref 0 in
    for state = 1 to state_count - 1 do
      if Array.unsafe_get .~last state > Array.unsafe_get .~last !best then
        best := state
    done;
    let path = Array.make .~length !best in
    for time = .~length - 1 downto 1 do
      Array.unsafe_set
        path
        (time - 1)
        (Array.unsafe_get
           (Array.unsafe_get .~backpointers time)
           (Array.unsafe_get path time))
    done;
    path
  >.

let compile_viterbi_indices_code
    ~(lowering : transition_lowering)
    ~(observation_count : int)
    (transition : ('s, 's) Model_kernel.numeric)
    : (float array ->
       int array -> int array -> float array ->
       int array -> int array -> float array ->
       int array -> int array * float) code =
  let state_count = transition.Model_kernel.source.Finite_domain.cardinal in
  let emit_transition offsets targets probabilities current predicted backpointers =
    match lowering with
    | Embedded ->
        emit_embedded_viterbi_transition
          ~transition
          ~current
          ~predicted
          ~backpointers
    | Packed ->
        emit_packed_viterbi_transition
          ~state_count
          ~offsets
          ~targets
          ~probabilities
          ~current
          ~predicted
          ~backpointers
  in
  .<
    fun initial
        transition_offsets transition_targets transition_probabilities
        emission_offsets emission_states emission_probabilities observations ->
      if Array.length initial <> state_count then
        invalid_arg
          (Printf.sprintf
             "Kernel_staged.viterbi: initial vector length %d, expected %d"
             (Array.length initial)
             state_count);
      if Array.length emission_offsets <> observation_count + 1 then
        invalid_arg "Kernel_staged.viterbi: malformed emission offsets";
      if Array.length emission_states <> Array.length emission_probabilities then
        invalid_arg "Kernel_staged.viterbi: malformed emission entries";
      let length = Array.length observations in
      if length = 0 then
        invalid_arg "Kernel_staged.viterbi: empty observation sequence";
      let current = Array.make state_count 0.0 in
      let predicted = Array.make state_count 0.0 in
      .~(emit_score_packed
           ~state_count
           ~observation_count
           ~source_values:.< initial >.
           ~scored:.< current >.
           ~offsets:.< emission_offsets >.
           ~states:.< emission_states >.
           ~probabilities:.< emission_probabilities >.
           ~observation:.< Array.unsafe_get observations 0 >.);
      let first_maximum = .~(emit_max_rescale ~state_count .< current >.) in
      let log_score = ref (log first_maximum) in
      let backpointers = Array.make length [||] in
      for time = 1 to length - 1 do
        Array.fill predicted 0 state_count 0.0;
        (* Keep traceback in range when every candidate score is zero. *)
        let time_backpointers = Array.make state_count 0 in
        .~(emit_transition
             .< transition_offsets >.
             .< transition_targets >.
             .< transition_probabilities >.
             .< current >.
             .< predicted >.
             .< time_backpointers >.);
        .~(emit_score_packed
             ~state_count
             ~observation_count
             ~source_values:.< predicted >.
             ~scored:.< current >.
             ~offsets:.< emission_offsets >.
             ~states:.< emission_states >.
             ~probabilities:.< emission_probabilities >.
             ~observation:.< Array.unsafe_get observations time >.);
        let maximum = .~(emit_max_rescale ~state_count .< current >.) in
        log_score := !log_score +. log maximum;
        Array.unsafe_set backpointers time time_backpointers
      done;
      let path =
        .~(emit_viterbi_path
             ~state_count
             ~length:.< length >.
             ~last:.< current >.
             ~backpointers:.< backpointers >.)
      in
      (path, !log_score)
  >.

let compile_viterbi
    ?(lowering = Embedded)
    ~(initial : float array)
    ~(transition : ('s, 's) Model_kernel.t)
    ~(emission : ('s, 'o) Model_kernel.t) =
  Finite_domain.require_same
    ~context:"Kernel_staged.compile_viterbi"
    transition.Model_kernel.src
    transition.Model_kernel.dst;
  let state_count = transition.Model_kernel.src.Finite_domain.cardinal in
  if Array.length initial <> state_count then
    invalid_arg
      (Printf.sprintf
         "Kernel_staged.compile_viterbi: initial vector length %d, expected %d"
         (Array.length initial)
         state_count);
  Array.iteri
    (fun state probability ->
       Model_kernel.validate_probability
         ~context:(Printf.sprintf
             "Kernel_staged.compile_viterbi initial state %d"
             state)
         probability)
    initial;
  Model_kernel.validate_row_mass
    ~context:"Kernel_staged.compile_viterbi initial"
    initial;
  let numeric_transition = require_constant_transition transition in
  let packed_transition =
    match lowering with
    | Packed -> pack_rows numeric_transition
    | Embedded -> { offsets = [||]; targets = [||]; probabilities = [||] }
  in
  let numeric_emission = require_constant_emission ~transition emission in
  let packed_emission = pack_columns numeric_emission in
  let observation_count = emission.Model_kernel.dst.Finite_domain.cardinal in
  let core =
    Runcode.run
      (compile_viterbi_indices_code
         ~lowering
         ~observation_count
         numeric_transition)
  in
  let initial = Array.copy initial in
  let encode_observation = Finite_domain.encode emission.Model_kernel.dst in
  fun observations ->
    let observation_indices = Array.map encode_observation observations in
    core
      initial
      packed_transition.offsets
      packed_transition.targets
      packed_transition.probabilities
      packed_emission.column_offsets
      packed_emission.states
      packed_emission.emission_probabilities
      observation_indices

let compile_viterbi_parameterized_indices_code
    ~(lowering : transition_lowering)
    ~(observation_count : int)
    ~(indexed : indexed_symbolic_rows)
    (transition : ('s, 's) Model_kernel.t)
    : (float array -> float array ->
       int array -> int array -> int array ->
       int array -> int array -> float array ->
       int array -> int array * float) code =
  let state_count = transition.Model_kernel.src.Finite_domain.cardinal in
  let emit_transition offsets targets weight_ids weight_values current predicted backpointers =
    match lowering with
    | Embedded ->
        emit_embedded_symbolic_viterbi_transition
          ~kernel:transition
          ~indexed
          ~weight_values
          ~current
          ~predicted
          ~backpointers
    | Packed ->
        emit_packed_symbolic_viterbi_transition
          ~state_count
          ~offsets
          ~targets
          ~weight_ids
          ~weight_values
          ~current
          ~predicted
          ~backpointers
  in
  .<
    fun parameters initial
        transition_offsets transition_targets transition_weight_ids
        emission_offsets emission_states emission_probabilities observations ->
      if Array.length initial <> state_count then
        invalid_arg
          (Printf.sprintf
             "Kernel_staged.viterbi_parameterized: initial vector length %d, expected %d"
             (Array.length initial)
             state_count);
      if Array.length emission_offsets <> observation_count + 1 then
        invalid_arg "Kernel_staged.viterbi_parameterized: malformed emission offsets";
      if Array.length emission_states <> Array.length emission_probabilities then
        invalid_arg "Kernel_staged.viterbi_parameterized: malformed emission entries";
      let length = Array.length observations in
      if length = 0 then
        invalid_arg "Kernel_staged.viterbi_parameterized: empty observation sequence";
      let weight_values =
        .~(emit_weight_values
             ~parameters:.< parameters >.
             indexed.weight_table)
      in
      let current = Array.make state_count 0.0 in
      let predicted = Array.make state_count 0.0 in
      .~(emit_score_packed
           ~state_count
           ~observation_count
           ~source_values:.< initial >.
           ~scored:.< current >.
           ~offsets:.< emission_offsets >.
           ~states:.< emission_states >.
           ~probabilities:.< emission_probabilities >.
           ~observation:.< Array.unsafe_get observations 0 >.);
      let first_maximum = .~(emit_max_rescale ~state_count .< current >.) in
      let log_score = ref (log first_maximum) in
      let backpointers = Array.make length [||] in
      for time = 1 to length - 1 do
        Array.fill predicted 0 state_count 0.0;
        (* Keep traceback in range when every candidate score is zero. *)
        let time_backpointers = Array.make state_count 0 in
        .~(emit_transition
             .< transition_offsets >.
             .< transition_targets >.
             .< transition_weight_ids >.
             .< weight_values >.
             .< current >.
             .< predicted >.
             .< time_backpointers >.);
        .~(emit_score_packed
             ~state_count
             ~observation_count
             ~source_values:.< predicted >.
             ~scored:.< current >.
             ~offsets:.< emission_offsets >.
             ~states:.< emission_states >.
             ~probabilities:.< emission_probabilities >.
             ~observation:.< Array.unsafe_get observations time >.);
        let maximum = .~(emit_max_rescale ~state_count .< current >.) in
        log_score := !log_score +. log maximum;
        Array.unsafe_set backpointers time time_backpointers
      done;
      let path =
        .~(emit_viterbi_path
             ~state_count
             ~length:.< length >.
             ~last:.< current >.
             ~backpointers:.< backpointers >.)
      in
      (path, !log_score)
  >.

let compile_viterbi_parameterized
    ?(lowering = Embedded)
    ~(initial : float array)
    ~(transition : ('s, 's) Model_kernel.t)
    ~(emission : ('s, 'o) Model_kernel.t) =
  Finite_domain.require_same
    ~context:"Kernel_staged.compile_viterbi_parameterized"
    transition.Model_kernel.src
    transition.Model_kernel.dst;
  let state_count = transition.Model_kernel.src.Finite_domain.cardinal in
  if Array.length initial <> state_count then
    invalid_arg
      (Printf.sprintf
         "Kernel_staged.compile_viterbi_parameterized: initial vector length %d, expected %d"
         (Array.length initial)
         state_count);
  Array.iteri
    (fun state probability ->
       Model_kernel.validate_probability
         ~context:(Printf.sprintf
             "Kernel_staged.compile_viterbi_parameterized initial state %d"
             state)
         probability)
    initial;
  Model_kernel.validate_row_mass
    ~context:"Kernel_staged.compile_viterbi_parameterized initial"
    initial;
  let indexed = index_symbolic_rows transition in
  let required_parameters = parameter_count indexed.weight_table in
  let packed_transition =
    match lowering with
    | Packed -> pack_symbolic_rows transition indexed
    | Embedded ->
        { symbolic_offsets = [||];
          symbolic_targets = [||];
          symbolic_weight_ids = [||] }
  in
  let numeric_emission = require_constant_emission ~transition emission in
  let packed_emission = pack_columns numeric_emission in
  let observation_count = emission.Model_kernel.dst.Finite_domain.cardinal in
  let core =
    Runcode.run
      (compile_viterbi_parameterized_indices_code
         ~lowering
         ~observation_count
         ~indexed
         transition)
  in
  let initial = Array.copy initial in
  let encode_observation = Finite_domain.encode emission.Model_kernel.dst in
  fun parameters observations ->
    if Array.length parameters < required_parameters then
      invalid_arg
        (Printf.sprintf
           "Kernel_staged.viterbi_parameterized: parameter vector length %d, expected at least %d"
           (Array.length parameters)
           required_parameters);
    Parameter.check_values parameters;
    let observation_indices = Array.map encode_observation observations in
    core
      parameters
      initial
      packed_transition.symbolic_offsets
      packed_transition.symbolic_targets
      packed_transition.symbolic_weight_ids
      packed_emission.column_offsets
      packed_emission.states
      packed_emission.emission_probabilities
      observation_indices
