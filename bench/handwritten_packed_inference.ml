(* Handwritten packed probability-domain inference baseline.
   This module intentionally mirrors the runtime algorithm implemented by the
   staged Packed lowering, but contains no generated code.  Preparation of the
   packed topology is measured separately by the benchmark harness. *)

let check_observation ~observation_count observation =
  if observation < 0 || observation >= observation_count then
    invalid_arg
      (Printf.sprintf
         "Handwritten_packed_inference: observation index %d outside [0,%d)"
         observation observation_count)

let score_emission
    ~state_count
    ~observation_count
    ~source_values
    ~scored
    ~(emission : Kernel_staged.packed_columns)
    observation =
  check_observation ~observation_count observation;
  Array.fill scored 0 state_count 0.0;
  let first = Array.unsafe_get emission.column_offsets observation in
  let last = Array.unsafe_get emission.column_offsets (observation + 1) in
  for entry = first to last - 1 do
    let state = Array.unsafe_get emission.states entry in
    Array.unsafe_set
      scored
      state
      (Array.unsafe_get source_values state
       *. Array.unsafe_get emission.emission_probabilities entry)
  done

let rescale_sum values =
  let mass = ref 0.0 in
  for state = 0 to Array.length values - 1 do
    mass := !mass +. Array.unsafe_get values state
  done;
  let mass = !mass in
  if mass > 0.0 then begin
    let reciprocal = 1.0 /. mass in
    for state = 0 to Array.length values - 1 do
      Array.unsafe_set values state (Array.unsafe_get values state *. reciprocal)
    done
  end;
  mass

let rescale_max values =
  let best = ref 0.0 in
  for state = 0 to Array.length values - 1 do
    let value = Array.unsafe_get values state in
    if value > !best then best := value
  done;
  let best = !best in
  if best > 0.0 then begin
    let reciprocal = 1.0 /. best in
    for state = 0 to Array.length values - 1 do
      Array.unsafe_set values state (Array.unsafe_get values state *. reciprocal)
    done
  end;
  best

let push_sum_constant
    ~state_count
    ~(transition : Kernel_staged.packed_rows)
    current
    next =
  Array.fill next 0 state_count 0.0;
  for source = 0 to state_count - 1 do
    let source_value = Array.unsafe_get current source in
    if source_value > 0.0 then begin
      let first = Array.unsafe_get transition.offsets source in
      let last = Array.unsafe_get transition.offsets (source + 1) in
      for edge = first to last - 1 do
        let target = Array.unsafe_get transition.targets edge in
        let candidate = source_value *. Array.unsafe_get transition.probabilities edge in
        Array.unsafe_set next target (Array.unsafe_get next target +. candidate)
      done
    end
  done

let push_sum_symbolic
    ~state_count
    ~(transition : Kernel_staged.packed_symbolic_rows)
    ~weight_values
    current
    next =
  Array.fill next 0 state_count 0.0;
  for source = 0 to state_count - 1 do
    let source_value = Array.unsafe_get current source in
    if source_value > 0.0 then begin
      let first = Array.unsafe_get transition.symbolic_offsets source in
      let last = Array.unsafe_get transition.symbolic_offsets (source + 1) in
      for edge = first to last - 1 do
        let target = Array.unsafe_get transition.symbolic_targets edge in
        let weight_id = Array.unsafe_get transition.symbolic_weight_ids edge in
        let candidate = source_value *. Array.unsafe_get weight_values weight_id in
        Array.unsafe_set next target (Array.unsafe_get next target +. candidate)
      done
    end
  done

let push_max_constant
    ~state_count
    ~(transition : Kernel_staged.packed_rows)
    current
    next
    backpointers =
  Array.fill next 0 state_count 0.0;
  (* Keep traceback in range when every candidate score is zero. *)
  Array.fill backpointers 0 state_count 0;
  for source = 0 to state_count - 1 do
    let source_value = Array.unsafe_get current source in
    if source_value > 0.0 then begin
      let first = Array.unsafe_get transition.offsets source in
      let last = Array.unsafe_get transition.offsets (source + 1) in
      for edge = first to last - 1 do
        let target = Array.unsafe_get transition.targets edge in
        let candidate = source_value *. Array.unsafe_get transition.probabilities edge in
        if candidate > Array.unsafe_get next target then begin
          Array.unsafe_set next target candidate;
          Array.unsafe_set backpointers target source
        end
      done
    end
  done

let push_max_symbolic
    ~state_count
    ~(transition : Kernel_staged.packed_symbolic_rows)
    ~weight_values
    current
    next
    backpointers =
  Array.fill next 0 state_count 0.0;
  (* Keep traceback in range when every candidate score is zero. *)
  Array.fill backpointers 0 state_count 0;
  for source = 0 to state_count - 1 do
    let source_value = Array.unsafe_get current source in
    if source_value > 0.0 then begin
      let first = Array.unsafe_get transition.symbolic_offsets source in
      let last = Array.unsafe_get transition.symbolic_offsets (source + 1) in
      for edge = first to last - 1 do
        let target = Array.unsafe_get transition.symbolic_targets edge in
        let weight_id = Array.unsafe_get transition.symbolic_weight_ids edge in
        let candidate = source_value *. Array.unsafe_get weight_values weight_id in
        if candidate > Array.unsafe_get next target then begin
          Array.unsafe_set next target candidate;
          Array.unsafe_set backpointers target source
        end
      done
    end
  done

let evaluate_weights parameters weight_table =
  Array.map (Weight.eval parameters) weight_table

let forward_constant
    ~initial
    ~(transition : Kernel_staged.packed_rows)
    ~(emission : Kernel_staged.packed_columns)
    ~observation_count
    observations =
  let state_count = Array.length initial in
  let length = Array.length observations in
  if length = 0 then invalid_arg "Handwritten_packed_inference.forward: empty observations";
  let current = Array.make state_count 0.0 in
  let predicted = Array.make state_count 0.0 in
  score_emission
    ~state_count ~observation_count ~source_values:initial ~scored:current
    ~emission (Array.unsafe_get observations 0);
  let log_score = ref (log (rescale_sum current)) in
  for time = 1 to length - 1 do
    push_sum_constant ~state_count ~transition current predicted;
    score_emission
      ~state_count ~observation_count ~source_values:predicted ~scored:current
      ~emission (Array.unsafe_get observations time);
    log_score := !log_score +. log (rescale_sum current)
  done;
  !log_score

let forward_parameterized
    ~parameters
    ~weight_table
    ~initial
    ~(transition : Kernel_staged.packed_symbolic_rows)
    ~(emission : Kernel_staged.packed_columns)
    ~observation_count
    observations =
  let weight_values = evaluate_weights parameters weight_table in
  let state_count = Array.length initial in
  let length = Array.length observations in
  if length = 0 then invalid_arg "Handwritten_packed_inference.forward_parameterized: empty observations";
  let current = Array.make state_count 0.0 in
  let predicted = Array.make state_count 0.0 in
  score_emission
    ~state_count ~observation_count ~source_values:initial ~scored:current
    ~emission (Array.unsafe_get observations 0);
  let log_score = ref (log (rescale_sum current)) in
  for time = 1 to length - 1 do
    push_sum_symbolic ~state_count ~transition ~weight_values current predicted;
    score_emission
      ~state_count ~observation_count ~source_values:predicted ~scored:current
      ~emission (Array.unsafe_get observations time);
    log_score := !log_score +. log (rescale_sum current)
  done;
  !log_score

let best_state values =
  let best = ref 0 in
  for state = 1 to Array.length values - 1 do
    if Array.unsafe_get values state > Array.unsafe_get values !best then best := state
  done;
  !best

let backtrace backpointers last_state =
  let length = Array.length backpointers in
  let path = Array.make length last_state in
  for time = length - 1 downto 1 do
    Array.unsafe_set path (time - 1)
      (Array.unsafe_get (Array.unsafe_get backpointers time) (Array.unsafe_get path time))
  done;
  path

let viterbi_constant
    ~initial
    ~(transition : Kernel_staged.packed_rows)
    ~(emission : Kernel_staged.packed_columns)
    ~observation_count
    observations =
  let state_count = Array.length initial in
  let length = Array.length observations in
  if length = 0 then invalid_arg "Handwritten_packed_inference.viterbi: empty observations";
  let current = Array.make state_count 0.0 in
  let predicted = Array.make state_count 0.0 in
  let backpointers = Array.make length [||] in
  score_emission
    ~state_count ~observation_count ~source_values:initial ~scored:current
    ~emission (Array.unsafe_get observations 0);
  let log_score = ref (log (rescale_max current)) in
  for time = 1 to length - 1 do
    let bp = Array.make state_count 0 in
    push_max_constant ~state_count ~transition current predicted bp;
    score_emission
      ~state_count ~observation_count ~source_values:predicted ~scored:current
      ~emission (Array.unsafe_get observations time);
    log_score := !log_score +. log (rescale_max current);
    Array.unsafe_set backpointers time bp
  done;
  let _path = backtrace backpointers (best_state current) in
  !log_score

let viterbi_parameterized
    ~parameters
    ~weight_table
    ~initial
    ~(transition : Kernel_staged.packed_symbolic_rows)
    ~(emission : Kernel_staged.packed_columns)
    ~observation_count
    observations =
  let weight_values = evaluate_weights parameters weight_table in
  let state_count = Array.length initial in
  let length = Array.length observations in
  if length = 0 then invalid_arg "Handwritten_packed_inference.viterbi_parameterized: empty observations";
  let current = Array.make state_count 0.0 in
  let predicted = Array.make state_count 0.0 in
  let backpointers = Array.make length [||] in
  score_emission
    ~state_count ~observation_count ~source_values:initial ~scored:current
    ~emission (Array.unsafe_get observations 0);
  let log_score = ref (log (rescale_max current)) in
  for time = 1 to length - 1 do
    let bp = Array.make state_count 0 in
    push_max_symbolic ~state_count ~transition ~weight_values current predicted bp;
    score_emission
      ~state_count ~observation_count ~source_values:predicted ~scored:current
      ~emission (Array.unsafe_get observations time);
    log_score := !log_score +. log (rescale_max current);
    Array.unsafe_set backpointers time bp
  done;
  let _path = backtrace backpointers (best_state current) in
  !log_score
