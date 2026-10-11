(* Fixed-design semantic experiment for aggregation scope in the Original and
   Variant Grid Sensors models. The constants below were fixed before the
   first experimental run. *)

open Grid_sensor

module Kernel_original = Original (Kernel_interpreter)
module Kernel_variant = Variant (Kernel_interpreter)
module Indexed_original = Original (Indexed_dist)
module Indexed_variant = Variant (Indexed_dist)
module Trace_max_original = Original (Trace_max_dist)
module Trace_max_variant = Variant (Trace_max_dist)

(* Historical output labels. The archived results in result_directory were
   produced when the models were named Grid3 and Grid4, and
   make verify-model-generated-aggregation-scope requires byte-identical
   output. The labels "Grid3" (Original) and "Grid4" (Variant), and the
   path-max / pathmax wording in the output schema, are therefore preserved
   deliberately in everything this program prints or writes. *)
let original_label = "Grid3"
let variant_label = "Grid4"

let seed = 2027
let sample_count = 500
let sequence_length = 20
let score_tolerance = Model_language.probability_tolerance
let wilson_z = 1.959963984540054

let result_directory =
  "results/aggregation_scope/model_generated_20261006"

let require condition message =
  if not condition then failwith message

let close left right =
  Float.abs (left -. right) <= score_tolerance

let remap_indexed_numeric ~source ~destination
    (kernel : ('a, 'b) Indexed_dist.cdist) =
  let rows =
    Array.init source.Finite_domain.cardinal (fun source_index ->
        let source_value = Finite_domain.decode source source_index in
        Indexed_dist.row kernel source_value
        |> Indexed_dist.to_table
        |> Array.of_list)
  in
  let targets =
    Array.map
      (Array.map (fun (target_value, _) ->
           Finite_domain.encode destination target_value))
      rows
  and probabilities =
    Array.map (Array.map snd) rows
  in
  { Model_kernel.source;
    destination;
    targets;
    probabilities }

let sorted_row transition source =
  Array.to_list
    (Array.mapi
       (fun column target ->
          target, transition.Model_kernel.probabilities.(source).(column))
       transition.targets.(source))
  |> List.sort (fun (left, _) (right, _) -> Int.compare left right)

let numeric_max_difference left right =
  if Array.length left.Model_kernel.targets <> Array.length right.Model_kernel.targets
  then infinity
  else begin
    let difference = ref 0.0 in
    for source = 0 to Array.length left.targets - 1 do
      let left_row = sorted_row left source
      and right_row = sorted_row right source in
      if List.length left_row <> List.length right_row then
        difference := infinity
      else
        List.iter2
          (fun (left_target, left_score) (right_target, right_score) ->
             if left_target <> right_target then difference := infinity
             else
               difference :=
                 Float.max !difference
                   (Float.abs (left_score -. right_score)))
          left_row right_row
    done;
    !difference
  end

let row_score_by_mode transition source_state target_mode =
  let source = Finite_domain.encode transition.Model_kernel.source source_state in
  let score = ref 0.0 in
  Array.iteri
    (fun column target ->
       let target_state =
         Finite_domain.decode transition.Model_kernel.destination target
       in
       if target_state.m = target_mode then
         score := !score +. transition.probabilities.(source).(column))
    transition.targets.(source);
  !score

let validate_interpretation () =
  let original_sum =
    remap_indexed_numeric
      ~source:Kernel_original.state_typ
      ~destination:Kernel_original.state_typ
      Indexed_original.transition
  and original_max =
    Trace_max_dist.remap_numeric
      ~source:Kernel_original.state_typ
      ~destination:Kernel_original.state_typ
      Trace_max_original.transition
  and original_kernel =
    Model_kernel.instantiate Kernel_original.transition [||]
  in
  let original_sum_max_difference =
    numeric_max_difference original_sum original_max
  and original_indexed_kernel_difference =
    numeric_max_difference original_sum original_kernel
  in
  require (original_sum_max_difference <= score_tolerance)
    "Grid3 validation failed: summed and path-max transition rows differ";
  require (original_indexed_kernel_difference <= score_tolerance)
    "Grid3 validation failed: Indexed and kernel transition rows differ";

  let variant_sum =
    remap_indexed_numeric
      ~source:Kernel_variant.state_typ
      ~destination:Kernel_variant.state_typ
      Indexed_variant.transition
  and variant_max =
    Trace_max_dist.remap_numeric
      ~source:Kernel_variant.state_typ
      ~destination:Kernel_variant.state_typ
      Trace_max_variant.transition
  and variant_kernel =
    Model_kernel.instantiate Kernel_variant.transition [||]
  in
  require
    (numeric_max_difference variant_sum variant_kernel <= score_tolerance)
    "Grid4 validation failed: Indexed and kernel transition rows differ";
  let representative = { x1 = 10; x2 = 10; m = 0 } in
  let sum_scores =
    [| row_score_by_mode variant_sum representative 0;
       row_score_by_mode variant_sum representative 1;
       row_score_by_mode variant_sum representative 2 |]
  and max_scores =
    [| row_score_by_mode variant_max representative 0;
       row_score_by_mode variant_max representative 1;
       row_score_by_mode variant_max representative 2 |]
  in
  require
    (close sum_scores.(0) 0.76
     && close sum_scores.(1) 0.20
     && close sum_scores.(2) 0.04)
    "Grid4 validation failed: summed scores are not 0.76/0.20/0.04";
  require
    (close max_scores.(0) 0.72
     && close max_scores.(1) 0.12
     && close max_scores.(2) 0.04)
    "Grid4 validation failed: path-max scores are not 0.72/0.12/0.04";
  Printf.printf
    "Grid3 validation: all %d source rows sum=max (max difference %.1e)\n"
    Kernel_original.state_typ.Finite_domain.cardinal
    original_sum_max_difference;
  Printf.printf
    "Grid4 validation: summed same/right/right^2 = %.2f/%.2f/%.2f\n"
    sum_scores.(0) sum_scores.(1) sum_scores.(2);
  Printf.printf
    "Grid4 validation: path-max same/right/right^2 = %.2f/%.2f/%.2f (not a probability row)\n%!"
    max_scores.(0) max_scores.(1) max_scores.(2);
  original_max, variant_max

type model = {
  name : string;
  initial : float array;
  transition_sum : (state, state) Model_kernel.numeric;
  transition_max : (state, state) Model_kernel.numeric;
  emission : (state, obs) Model_kernel.numeric;
}

type sample = {
  model : string;
  sequence_id : int;
  hidden_path : int array;
  observations : obs array;
  path_sum : int array;
  path_max : int array;
  returned_paths_differ : bool;
  optimum_sum : float;
  optimum_max : float;
  sum_score_of_max_path : float;
  max_score_of_sum_path : float;
  hamming_distance : int;
  both_returned_paths_cross_suboptimal : bool;
}

let draw_sparse random targets weights =
  require (Array.length targets = Array.length weights)
    "sampling row has inconsistent target and weight lengths";
  require (Array.length targets > 0) "cannot sample an empty row";
  let mass = Array.fold_left ( +. ) 0.0 weights in
  require (mass > 0.0) "cannot sample a zero-mass row";
  let needle = Random.State.float random mass in
  let cumulative = ref 0.0
  and selected = ref targets.(Array.length targets - 1)
  and found = ref false in
  for column = 0 to Array.length targets - 1 do
    if not !found then begin
      cumulative := !cumulative +. weights.(column);
      if needle < !cumulative then begin
        selected := targets.(column);
        found := true
      end
    end
  done;
  !selected

let sample_sequence random model =
  let state_targets = Array.init (Array.length model.initial) Fun.id in
  let hidden_path = Array.make sequence_length 0
  and observations = Array.make sequence_length { y1 = 1; y2 = 1 } in
  hidden_path.(0) <- draw_sparse random state_targets model.initial;
  let sample_observation time =
    let state = hidden_path.(time) in
    let observation_index =
      draw_sparse random
        model.emission.targets.(state)
        model.emission.probabilities.(state)
    in
    observations.(time) <-
      Finite_domain.decode model.emission.destination observation_index
  in
  sample_observation 0;
  for time = 1 to sequence_length - 1 do
    let previous = hidden_path.(time - 1) in
    hidden_path.(time) <-
      draw_sparse random
        model.transition_sum.targets.(previous)
        model.transition_sum.probabilities.(previous);
    sample_observation time
  done;
  hidden_path, observations

let lookup transition source target =
  let result = ref 0.0 in
  Array.iteri
    (fun column candidate ->
       if candidate = target then
         result := transition.Model_kernel.probabilities.(source).(column))
    transition.targets.(source);
  !result

let path_log_score ~initial ~transition ~emission observations path =
  require (Array.length observations = Array.length path)
    "path and observation lengths differ";
  let add_log total weight =
    if total = neg_infinity || weight <= 0.0 then neg_infinity
    else total +. log weight
  in
  let observation_index time =
    Finite_domain.encode emission.Model_kernel.destination observations.(time)
  in
  let total =
    ref
      (add_log
         (log initial.(path.(0)))
         (lookup emission path.(0) (observation_index 0)))
  in
  for time = 1 to Array.length path - 1 do
    total :=
      add_log !total
        (lookup transition path.(time - 1) path.(time));
    total :=
      add_log !total
        (lookup emission path.(time) (observation_index time))
  done;
  !total

let hamming_distance left right =
  require (Array.length left = Array.length right)
    "cannot compare paths of different lengths";
  let distance = ref 0 in
  for time = 0 to Array.length left - 1 do
    if left.(time) <> right.(time) then incr distance
  done;
  !distance

let strictly_below candidate optimum =
  candidate < optimum -. score_tolerance

let run_model random model =
  Array.init sample_count (fun index ->
      let hidden_path, observations = sample_sequence random model in
      let path_sum, optimum_sum =
        Kernel_inference_prob.Viterbi.run
          ~initial:model.initial
          ~transition:model.transition_sum
          ~emission:model.emission
          observations
      and path_max, optimum_max =
        Kernel_inference_prob.Viterbi.run
          ~initial:model.initial
          ~transition:model.transition_max
          ~emission:model.emission
          observations
      in
      let attained_sum =
        path_log_score
          ~initial:model.initial
          ~transition:model.transition_sum
          ~emission:model.emission observations path_sum
      and attained_max =
        path_log_score
          ~initial:model.initial
          ~transition:model.transition_max
          ~emission:model.emission observations path_max
      and sum_score_of_max_path =
        path_log_score
          ~initial:model.initial
          ~transition:model.transition_sum
          ~emission:model.emission observations path_max
      and max_score_of_sum_path =
        path_log_score
          ~initial:model.initial
          ~transition:model.transition_max
          ~emission:model.emission observations path_sum
      in
      require (close attained_sum optimum_sum)
        (Printf.sprintf "%s sequence %d: summed Viterbi score mismatch"
           model.name (index + 1));
      require (close attained_max optimum_max)
        (Printf.sprintf "%s sequence %d: path-max Viterbi score mismatch"
           model.name (index + 1));
      let distance = hamming_distance path_sum path_max in
      let returned_paths_differ = distance > 0 in
      let both_returned_paths_cross_suboptimal =
        strictly_below max_score_of_sum_path optimum_max
        && strictly_below sum_score_of_max_path optimum_sum
      in
      require
        (not both_returned_paths_cross_suboptimal || returned_paths_differ)
        "mutual cross-suboptimality without differing returned paths";
      { model = model.name;
        sequence_id = index + 1;
        hidden_path;
        observations;
        path_sum;
        path_max;
        returned_paths_differ;
        optimum_sum;
        optimum_max;
        sum_score_of_max_path;
        max_score_of_sum_path;
        hamming_distance = distance;
        both_returned_paths_cross_suboptimal })

type interval = {
  lower : float;
  upper : float;
}

type hamming_summary = {
  median : float;
  q1 : float;
  q3 : float;
  iqr : float;
}

type aggregate = {
  model : string;
  returned_path_disagreement_count : int;
  returned_path_disagreement_fraction : float;
  returned_path_disagreement_interval : interval;
  cross_suboptimal_count : int;
  cross_suboptimal_fraction : float;
  cross_suboptimal_interval : interval;
  differing_without_cross_suboptimality_count : int;
  differing_hamming : hamming_summary option;
}

let wilson_interval count total =
  let n = float_of_int total
  and proportion = float_of_int count /. float_of_int total in
  let z_squared = wilson_z *. wilson_z in
  let denominator = 1.0 +. z_squared /. n in
  let center =
    (proportion +. z_squared /. (2.0 *. n)) /. denominator
  and margin =
    wilson_z
    *. sqrt
         ((proportion *. (1.0 -. proportion) +. z_squared /. (4.0 *. n))
          /. n)
    /. denominator
  in
  { lower =
      if count = 0 then 0.0
      else Float.max 0.0 (Float.min 1.0 (center -. margin));
    upper =
      if count = total then 1.0
      else Float.max 0.0 (Float.min 1.0 (center +. margin)) }

let validate_wilson_interval () =
  let zero = wilson_interval 0 10
  and full = wilson_interval 10 10
  and interior = wilson_interval 5 10 in
  require (zero.lower = 0.0 && zero.upper > 0.0 && zero.upper <= 1.0)
    "Wilson interval failed at zero successes";
  require (full.upper = 1.0 && full.lower >= 0.0 && full.lower < 1.0)
    "Wilson interval failed at all successes";
  require
    (close interior.lower 0.236593090512564
     && close interior.upper 0.763406909487436)
    "Wilson interval failed at an interior count"

(* Linear interpolation with h=(n-1)p, commonly called type-7 quantiles. *)
let quantile sorted probability =
  let length = Array.length sorted in
  require (length > 0) "quantile of an empty array";
  let position = probability *. float_of_int (length - 1) in
  let lower_index = int_of_float (floor position) in
  let upper_index = int_of_float (ceil position) in
  let fraction = position -. float_of_int lower_index in
  let lower = float_of_int sorted.(lower_index)
  and upper = float_of_int sorted.(upper_index) in
  lower +. fraction *. (upper -. lower)

let aggregate model samples =
  let returned_path_disagreement_count =
    Array.fold_left
      (fun count sample ->
         if sample.returned_paths_differ then count + 1 else count)
      0 samples
  and cross_suboptimal_count =
    Array.fold_left
      (fun count sample ->
         if sample.both_returned_paths_cross_suboptimal then count + 1
         else count)
      0 samples
  in
  let differing_distances =
    Array.to_list samples
    |> List.filter_map (fun sample ->
           if sample.returned_paths_differ then Some sample.hamming_distance
           else None)
    |> Array.of_list
  in
  Array.sort Int.compare differing_distances;
  let differing_hamming =
    if Array.length differing_distances = 0 then None
    else
      let q1 = quantile differing_distances 0.25
      and median = quantile differing_distances 0.50
      and q3 = quantile differing_distances 0.75 in
      Some { median; q1; q3; iqr = q3 -. q1 }
  in
  { model;
    returned_path_disagreement_count;
    returned_path_disagreement_fraction =
      float_of_int returned_path_disagreement_count /. float_of_int sample_count;
    returned_path_disagreement_interval =
      wilson_interval returned_path_disagreement_count sample_count;
    cross_suboptimal_count;
    cross_suboptimal_fraction =
      float_of_int cross_suboptimal_count /. float_of_int sample_count;
    cross_suboptimal_interval =
      wilson_interval cross_suboptimal_count sample_count;
    differing_without_cross_suboptimality_count =
      returned_path_disagreement_count - cross_suboptimal_count;
    differing_hamming }

let string_of_state_path domain path =
  Array.to_list path
  |> List.map (fun index ->
         let state = Finite_domain.decode domain index in
         Printf.sprintf "%d:%d:%d" state.x1 state.x2 state.m)
  |> String.concat "|"

let string_of_observations observations =
  Array.to_list observations
  |> List.map (fun observation ->
         Printf.sprintf "%d:%d" observation.y1 observation.y2)
  |> String.concat "|"

let rec ensure_directory path =
  if path = "." || path = "" then ()
  else if Sys.file_exists path then
    require (Sys.is_directory path) (path ^ " exists and is not a directory")
  else begin
    ensure_directory (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let create_result_directory () =
  require (not (Sys.file_exists result_directory))
    (result_directory ^ " already exists; refusing to overwrite results");
  ensure_directory (Filename.dirname result_directory);
  Unix.mkdir result_directory 0o755

let with_exclusive_output filename f =
  let channel =
    open_out_gen
      [ Open_wronly; Open_creat; Open_excl; Open_text ]
      0o644 filename
  in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> f channel)

let result_path filename = Filename.concat result_directory filename

let optional_float = function
  | None -> ""
  | Some value -> Printf.sprintf "%.17g" value

let write_raw models samples =
  with_exclusive_output (result_path "raw_per_sequence.csv") (fun channel ->
      output_string channel
        "model,sequence_id,seed,N,T,hidden_path,observations,path_sum,path_max,returned_paths_differ,optimum_sum_log_score,optimum_pathmax_log_score,sum_log_score_of_pathmax_path,pathmax_log_score_of_sum_path,hamming_distance,both_returned_paths_strictly_suboptimal_under_other_objective\n";
      List.iter2
        (fun model model_samples ->
           Array.iter
             (fun (sample : sample) ->
                Printf.fprintf channel
                  "%s,%d,%d,%d,%d,%s,%s,%s,%s,%b,%.17g,%.17g,%.17g,%.17g,%d,%b\n"
                  sample.model sample.sequence_id seed sample_count
                  sequence_length
                  (string_of_state_path
                     model.transition_sum.Model_kernel.source
                     sample.hidden_path)
                  (string_of_observations sample.observations)
                  (string_of_state_path
                     model.transition_sum.Model_kernel.source
                     sample.path_sum)
                  (string_of_state_path
                     model.transition_sum.Model_kernel.source
                     sample.path_max)
                  sample.returned_paths_differ
                  sample.optimum_sum sample.optimum_max
                  sample.sum_score_of_max_path
                  sample.max_score_of_sum_path
                  sample.hamming_distance
                  sample.both_returned_paths_cross_suboptimal)
             model_samples)
        models samples)

let write_aggregates aggregates =
  with_exclusive_output (result_path "aggregate.csv") (fun channel ->
      output_string channel
        "model,N,T,seed,returned_path_disagreement_count,returned_path_disagreement_fraction,returned_path_disagreement_wilson95_low,returned_path_disagreement_wilson95_high,both_returned_paths_strictly_suboptimal_under_other_objective_count,both_returned_paths_strictly_suboptimal_under_other_objective_fraction,both_returned_paths_strictly_suboptimal_under_other_objective_wilson95_low,both_returned_paths_strictly_suboptimal_under_other_objective_wilson95_high,differing_returned_paths_not_meeting_cross_suboptimality_criterion_count,differing_cases,hamming_q1,hamming_median,hamming_q3,hamming_iqr\n";
      List.iter
        (fun aggregate ->
           let differing_cases, q1, median, q3, iqr =
             match aggregate.differing_hamming with
             | None -> 0, None, None, None, None
             | Some summary ->
                 ( aggregate.returned_path_disagreement_count,
                   Some summary.q1,
                   Some summary.median,
                   Some summary.q3,
                   Some summary.iqr )
           in
           Printf.fprintf channel
             "%s,%d,%d,%d,%d,%.17g,%.17g,%.17g,%d,%.17g,%.17g,%.17g,%d,%d,%s,%s,%s,%s\n"
             aggregate.model sample_count sequence_length seed
             aggregate.returned_path_disagreement_count
             aggregate.returned_path_disagreement_fraction
             aggregate.returned_path_disagreement_interval.lower
             aggregate.returned_path_disagreement_interval.upper
             aggregate.cross_suboptimal_count
             aggregate.cross_suboptimal_fraction
             aggregate.cross_suboptimal_interval.lower
             aggregate.cross_suboptimal_interval.upper
             aggregate.differing_without_cross_suboptimality_count
             differing_cases
             (optional_float q1) (optional_float median)
             (optional_float q3) (optional_float iqr))
        aggregates)

let hamming_markdown = function
  | None -> "NA"
  | Some summary ->
      Printf.sprintf "median %.2f; Q1 %.2f; Q3 %.2f; IQR %.2f"
        summary.median summary.q1 summary.q3 summary.iqr

let write_report aggregates =
  with_exclusive_output (result_path "report.md") (fun channel ->
      Printf.fprintf channel
        "# Model-generated aggregation-scope experiment\n\nSeed: `%d`; sequences per model: `%d`; sequence length: `%d`; model order: Grid3 then Grid4.\n\nThe data were sampled from the ordinary summed HMM initial, transition, and emission distributions. The alternative decoder uses unnormalised multiplicative scores derived by interpreting the same model with max, rather than addition, when duplicate target indices are combined.\n\n## Interpretation validation\n\n- Grid3: summed and path-max transition scores agree for every source state.\n- Grid4 summed same/right/right^2 weights: `0.76 / 0.20 / 0.04`.\n- Grid4 path-max same/right/right^2 scores: `0.72 / 0.12 / 0.04`; these do not form a probability row.\n\n## Aggregate results\n\n| Model | Returned decoded paths differ | Wilson 95%% CI | Both returned paths strictly suboptimal under the other objective | Wilson 95%% CI | Differing path pairs not meeting the stricter criterion | Hamming distance among differing paths |\n|---|---:|---:|---:|---:|---:|---|\n"
        seed sample_count sequence_length;
      List.iter
        (fun aggregate ->
           Printf.fprintf channel
             "| %s | %d/%d (%.6f) | [%.6f, %.6f] | %d/%d (%.6f) | [%.6f, %.6f] | %d | %s |\n"
             aggregate.model
             aggregate.returned_path_disagreement_count sample_count
             aggregate.returned_path_disagreement_fraction
             aggregate.returned_path_disagreement_interval.lower
             aggregate.returned_path_disagreement_interval.upper
             aggregate.cross_suboptimal_count sample_count
             aggregate.cross_suboptimal_fraction
             aggregate.cross_suboptimal_interval.lower
             aggregate.cross_suboptimal_interval.upper
             aggregate.differing_without_cross_suboptimality_count
             (hamming_markdown aggregate.differing_hamming))
        aggregates;
      output_string channel
        "\nThe stricter criterion holds only when the summed-objective path is strictly suboptimal under the path-max objective and the path-max-objective path is strictly suboptimal under the summed objective, in each case by more than the absolute `1e-12` score tolerance. Failing this criterion does not identify the reason for the returned-path difference. Passing it concerns only the two returned paths and does not establish that the complete optimal-path sets are disjoint. Quartiles use linear interpolation with `h=(n-1)p` (type 7). The rates characterize only these fixed Grid3 and Grid4 models and do not establish superiority of either decoder.\n")

let write_config () =
  with_exclusive_output (result_path "config.txt") (fun channel ->
      Printf.fprintf channel
        "seed=%d\nsequences_per_model=%d\nsequence_length=%d\nmodel_order=Grid3,Grid4\nrng=OCaml Random.State.make [|2027|]\nsampling=initial then emission; repeated ordinary-summed transition then emission\nscore_tolerance=%.17g\nreturned_paths_differ=path_sum differs from path_pathmax\nboth_returned_paths_strictly_suboptimal_under_other_objective=score_pathmax(path_sum) < optimum_pathmax - tolerance AND score_sum(path_pathmax) < optimum_sum - tolerance\ncross_suboptimality_scope=the criterion evaluates only the two returned paths, not the complete optimal-path sets\nwilson_z=%.15g\nquantiles=type-7 linear interpolation h=(n-1)p\n"
        seed sample_count sequence_length score_tolerance wilson_z)

let print_aggregate aggregate =
  Printf.printf
    "%s returned decoded paths differ: %d/%d = %.6f, Wilson 95%% [%.6f, %.6f]\n"
    aggregate.model aggregate.returned_path_disagreement_count sample_count
    aggregate.returned_path_disagreement_fraction
    aggregate.returned_path_disagreement_interval.lower
    aggregate.returned_path_disagreement_interval.upper;
  Printf.printf
    "%s both returned paths strictly suboptimal under the other objective: %d/%d = %.6f, Wilson 95%% [%.6f, %.6f]\n"
    aggregate.model aggregate.cross_suboptimal_count sample_count
    aggregate.cross_suboptimal_fraction
    aggregate.cross_suboptimal_interval.lower
    aggregate.cross_suboptimal_interval.upper;
  Printf.printf
    "%s differing returned paths not meeting the stricter criterion: %d\n"
    aggregate.model aggregate.differing_without_cross_suboptimality_count;
  Printf.printf "%s Hamming among differing paths: %s\n"
    aggregate.model (hamming_markdown aggregate.differing_hamming)

let () =
  validate_wilson_interval ();
  let original_max, variant_max = validate_interpretation () in
  let original =
    { name = original_label;
      initial =
        Kernel_interpreter.initial_vector [||] Kernel_original.initial;
      transition_sum =
        Model_kernel.instantiate Kernel_original.transition [||];
      transition_max = original_max;
      emission = Model_kernel.instantiate Kernel_original.emission [||] }
  and variant =
    { name = variant_label;
      initial =
        Kernel_interpreter.initial_vector [||] Kernel_variant.initial;
      transition_sum =
        Model_kernel.instantiate Kernel_variant.transition [||];
      transition_max = variant_max;
      emission = Model_kernel.instantiate Kernel_variant.emission [||] }
  in
  let models = [ original; variant ] in
  let random = Random.State.make [| seed |] in
  let samples = List.map (run_model random) models in
  let aggregates =
    List.map2 (fun model rows -> aggregate model.name rows) models samples
  in
  create_result_directory ();
  write_raw models samples;
  write_aggregates aggregates;
  write_report aggregates;
  write_config ();
  List.iter print_aggregate aggregates;
  Printf.printf "fixed design: N=%d per model, T=%d, seed=%d\n"
    sample_count sequence_length seed;
  Printf.printf "results written to %s\n%!" result_directory
