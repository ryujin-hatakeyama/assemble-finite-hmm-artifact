(* Experiment-only characterization of Variant command/rotation trace
   aggregation. The alternative replaces addition of duplicate-target trace
   weights by maximum, so its rows are scores rather than probabilities.

   The trace-max row is not computed here: trace_max_score is a fixed,
   hand-specified table of the expected 0.72 / 0.12 / 0.04 scores. The
   trace-max interpretation itself (Trace_max_dist) is computed and compared
   with these scores in test_description_invariance.ml and in the
   model-generated experiment. *)

open Check
open Grid_sensor

module Kernel_variant = Variant (Kernel_interpreter)

let close x y = Float.abs (x -. y) < 1e-12

let trace_max_score source target =
  if target.m = source.m then 0.72
  else if target.m = Kernel_variant.right source.m then 0.12
  else if target.m = Kernel_variant.right (Kernel_variant.right source.m) then 0.04
  else
    invalid_arg "test_aggregation_scope: unexpected Variant transition target"

let make_trace_max_transition
    (summed : (state, state) Model_kernel.numeric) =
  let probabilities =
    Array.mapi
      (fun source_index targets ->
         let source = Finite_domain.decode summed.source source_index in
         Array.map
           (fun target_index ->
              let target = Finite_domain.decode summed.destination target_index in
              trace_max_score source target)
           targets)
      summed.targets
  in
  { summed with probabilities }

let row_scores transition source =
  let source_index = Finite_domain.encode transition.Model_kernel.source source in
  let scores = Array.make Kernel_variant.n_modes 0.0 in
  Array.iteri
    (fun column target_index ->
       let target = Finite_domain.decode transition.destination target_index in
       scores.(target.m) <- scores.(target.m) +. transition.probabilities.(source_index).(column))
    transition.targets.(source_index);
  scores

let same_path left right =
  Array.length left = Array.length right
  && begin
       let equal = ref true in
       for time = 0 to Array.length left - 1 do
         if left.(time) <> right.(time) then equal := false
       done;
       !equal
     end

let first_difference left right =
  let time = ref 0 in
  while !time < Array.length left && left.(!time) = right.(!time) do
    incr time
  done;
  if !time = Array.length left then None else Some !time

let string_of_observation observation =
  Printf.sprintf "(%d,%d)" observation.y1 observation.y2

let string_of_state state =
  Printf.sprintf "(%d,%d,%d)" state.x1 state.x2 state.m

let string_of_array f values =
  "[" ^ String.concat "; " (Array.to_list (Array.map f values)) ^ "]"

let transition_score transition source_index target_index =
  let result = ref None in
  Array.iteri
    (fun column target ->
       if target = target_index then
         result := Some transition.Model_kernel.probabilities.(source_index).(column))
    transition.targets.(source_index);
  match !result with
  | Some score -> score
  | None -> 0.0

type witness = {
  family : string;
  observations : obs array;
  summed_path : int array;
  summed_log_score : float;
  trace_max_path : int array;
  trace_max_log_score : float;
}

let decode summed path =
  Array.map (Finite_domain.decode summed.Model_kernel.source) path

let run_pair ~initial ~summed ~trace_max ~emission observations =
  let summed_path, summed_log_score =
    Kernel_inference_prob.Viterbi.run
      ~initial ~transition:summed ~emission observations
  in
  let trace_max_path, trace_max_log_score =
    Kernel_inference_prob.Viterbi.run
      ~initial ~transition:trace_max ~emission observations
  in
  summed_path, summed_log_score, trace_max_path, trace_max_log_score

let final_maximizer_count ~initial ~transition ~emission observations =
  let _, final_scores, _ =
    Kernel_inference_prob.Viterbi.T.run
      ~initial ~transition ~emission observations
  in
  let maximum = Array.fold_left Float.max 0.0 final_scores in
  Array.fold_left
    (fun count score -> if close score maximum then count + 1 else count)
    0 final_scores

let find_weak_family ~initial ~summed ~trace_max ~emission =
  let rec search length =
    if length > 20 then None
    else
      let observations =
        Array.init length (fun time ->
            if time = 0 then { y1 = 1; y2 = 1 }
            else { y1 = 21; y2 = 21 })
      in
      let summed_path, summed_log_score, trace_max_path, trace_max_log_score =
        run_pair ~initial ~summed ~trace_max ~emission observations
      in
      if same_path summed_path trace_max_path then search (length + 1)
      else
        Some
          { family = "weak-observation family";
            observations;
            summed_path;
            summed_log_score;
            trace_max_path;
            trace_max_log_score }
  in
  search 2

let find_small_alphabet ~initial ~summed ~trace_max ~emission =
  let alphabet =
    [| { y1 = 21; y2 = 21 };
       { y1 = 1; y2 = 21 };
       { y1 = 21; y2 = 1 };
       { y1 = 1; y2 = 1 } |]
  in
  let rec search_length length =
    if length > 6 then None
    else
      let sequence_count =
        let count = ref 1 in
        for _ = 1 to length do count := !count * Array.length alphabet done;
        !count
      in
      let rec search_code code =
        if code = sequence_count then search_length (length + 1)
        else
          let remaining = ref code in
          let observations =
            Array.init length (fun _ ->
                let observation = alphabet.(!remaining mod Array.length alphabet) in
                remaining := !remaining / Array.length alphabet;
                observation)
          in
          let summed_path, summed_log_score, trace_max_path, trace_max_log_score =
            run_pair ~initial ~summed ~trace_max ~emission observations
          in
          if same_path summed_path trace_max_path then search_code (code + 1)
          else
            Some
              { family = "small alphabet (base-4 enumeration, least-significant digit first)";
                observations;
                summed_path;
                summed_log_score;
                trace_max_path;
                trace_max_log_score }
      in
      search_code 0
  in
  search_length 2

let print_transition_context summed trace_max witness divergence =
  if Array.length witness.observations > 1 then begin
    let edge_time = if divergence = 0 then 0 else divergence - 1 in
    let print_edge label path =
      let source = path.(edge_time) and target = path.(edge_time + 1) in
      let source_state = Finite_domain.decode summed.Model_kernel.source source
      and target_state = Finite_domain.decode summed.Model_kernel.destination target in
      Printf.printf
        "%s transition t=%d->%d %s -> %s: summed=%.2f trace-max=%.2f\n"
        label edge_time (edge_time + 1)
        (string_of_state source_state) (string_of_state target_state)
        (transition_score summed source target)
        (transition_score trace_max source target)
    in
    print_edge "ordinary-kernel path" witness.summed_path;
    print_edge "max-over-internal-traces path" witness.trace_max_path
  end

let () =
  let summed = Model_kernel.instantiate Kernel_variant.transition [||]
  and emission = Model_kernel.instantiate Kernel_variant.emission [||] in
  let trace_max = make_trace_max_transition summed in
  let initial =
    Kernel_interpreter.initial_vector [||] Kernel_variant.initial
  in

  let representative = { x1 = 10; x2 = 10; m = 0 } in
  let summed_scores = row_scores summed representative
  and trace_max_scores = row_scores trace_max representative in
  Printf.printf
    "representative source %s\ncorrect grouped weights: same=%.2f right=%.2f right^2=%.2f\ntrace-max grouped scores: same=%.2f right=%.2f right^2=%.2f (row total %.2f; not a distribution)\n"
    (string_of_state representative)
    summed_scores.(0) summed_scores.(1) summed_scores.(2)
    trace_max_scores.(0) trace_max_scores.(1) trace_max_scores.(2)
    (Array.fold_left ( +. ) 0.0 trace_max_scores);
  report
    "representative summed row is 0.76 / 0.20 / 0.04"
    (close summed_scores.(0) 0.76
     && close summed_scores.(1) 0.20
     && close summed_scores.(2) 0.04)
    "";
  report
    "representative trace-max row is 0.72 / 0.12 / 0.04"
    (close trace_max_scores.(0) 0.72
     && close trace_max_scores.(1) 0.12
     && close trace_max_scores.(2) 0.04)
    "unnormalised scores";

  let witness =
    match find_weak_family ~initial ~summed ~trace_max ~emission with
    | Some witness -> Some witness
    | None ->
        print_endline "no weak-observation witness for T=2,...,20";
        find_small_alphabet ~initial ~summed ~trace_max ~emission
  in
  begin match witness with
  | None ->
      report
        "a projected HMM-state path differs in the documented search"
        false
        "none through weak T=20 or small-alphabet T=6"
  | Some witness ->
      let divergence =
        match first_difference witness.summed_path witness.trace_max_path with
        | Some time -> time
        | None -> assert false
      in
      let summed_states = decode summed witness.summed_path
      and trace_max_states = decode summed witness.trace_max_path in
      let summed_maximizers =
        final_maximizer_count
          ~initial ~transition:summed ~emission witness.observations
      and trace_max_maximizers =
        final_maximizer_count
          ~initial ~transition:trace_max ~emission witness.observations
      in
      let expected_summed_path =
        Array.map
          (Finite_domain.encode summed.Model_kernel.source)
          [| { x1 = 1; x2 = 1; m = 0 };
             { x1 = 1; x2 = 2; m = 0 };
             { x1 = 2; x2 = 2; m = 1 };
             { x1 = 3; x2 = 2; m = 1 };
             { x1 = 4; x2 = 2; m = 1 } |]
      and expected_trace_max_path =
        Array.map
          (Finite_domain.encode summed.Model_kernel.source)
          [| { x1 = 1; x2 = 1; m = 0 };
             { x1 = 1; x2 = 2; m = 0 };
             { x1 = 1; x2 = 3; m = 0 };
             { x1 = 1; x2 = 4; m = 0 };
             { x1 = 1; x2 = 5; m = 0 } |]
      in
      Printf.printf
        "witness family: %s\nobservation sequence: %s\nordinary-kernel path: %s\nmax-over-internal-traces path: %s\nordinary-kernel log score: %.15g\nmax-over-internal-traces log score: %.15g\nfirst divergent HMM state: t=%d (zero-based)\nfinal-state maximizers: ordinary=%d alternative=%d\n"
        witness.family
        (string_of_array string_of_observation witness.observations)
        (string_of_array string_of_state summed_states)
        (string_of_array string_of_state trace_max_states)
        witness.summed_log_score witness.trace_max_log_score divergence
        summed_maximizers trace_max_maximizers;
      print_transition_context summed trace_max witness divergence;
      print_endline
        "interpretation: the ordinary kernel adds internal trace weights by HMM successor before Viterbi; the test-only alternative takes their maximum instead.";
      report
        "shortest weak-observation witness is T=5 with divergence t=2"
        (witness.family = "weak-observation family"
         && Array.length witness.observations = 5
         && divergence = 2)
        "";
      report
        "witness final-state argmax multiplicity is 1 versus 2"
        (summed_maximizers = 1 && trace_max_maximizers = 2)
        "ordinary versus max-over-internal-traces";
      report
        "T=5 witness paths and log scores match the characterization"
        (same_path witness.summed_path expected_summed_path
         && same_path witness.trace_max_path expected_trace_max_path
         && close witness.summed_log_score (-6.94033830718861)
         && close witness.trace_max_log_score (-7.2483059012923))
        ""
  end;
  finish ()
