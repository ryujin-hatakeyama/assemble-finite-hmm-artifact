open Check
open Grid_sensor

module Nested_kernel = Variant (Kernel_interpreter)
module Nested_max = Variant (Trace_max_dist)

(* A second, flat description of the same Variant HMM: it assigns the
   aggregated heading weights 0.76 / 0.20 / 0.04 directly instead of composing
   a command with a rotation. It is not a separate Grid Sensors model. *)
module Flat_description (M : Model_language.MODEL) = struct
  include Types (M)

  let n_modes = 4
  let mode_typ = M.tint_range ~name:"flat-heading" n_modes
  let state_typ = state_typ ~modes:mode_typ

  let right mode = (mode + 1) mod n_modes

  let displacement = function
    | 0 -> 0, 1
    | 1 -> 1, 0
    | 2 -> 0, -1
    | 3 -> -1, 0
    | _ -> invalid_arg "Flat_description.displacement"

  let step source next_mode =
    let dx1, dx2 = displacement next_mode in
    { x1 = clamp_position (source.x1 + dx1);
      x2 = clamp_position (source.x2 + dx2);
      m = next_mode }

  let transition =
    M.cdist state_typ state_typ (fun source ->
        M.dist mode_typ
          [ source.m, M.const 0.76;
            right source.m, M.const 0.20;
            right (right source.m), M.const 0.04 ]
        |> M.map state_typ (step source))
end

module Flat_indexed = Flat_description (Indexed_dist)
module Flat_max = Flat_description (Trace_max_dist)

let close left right = Float.abs (left -. right) <= 1e-12

let remap_indexed_numeric ~source ~destination
    (kernel : ('a, 'b) Indexed_dist.cdist) =
  let rows =
    Array.init source.Finite_domain.cardinal (fun source_index ->
        let source_value = Finite_domain.decode source source_index in
        Indexed_dist.row kernel source_value
        |> Indexed_dist.to_table
        |> Array.of_list)
  in
  { Model_kernel.source;
    destination;
    targets =
      Array.map
        (Array.map (fun (target, _) -> Finite_domain.encode destination target))
        rows;
    probabilities = Array.map (Array.map snd) rows }

let sorted_row transition source =
  Array.mapi
    (fun column target ->
       target, transition.Model_kernel.probabilities.(source).(column))
    transition.targets.(source)
  |> Array.to_list
  |> List.sort (fun (left, _) (right, _) -> Int.compare left right)

let compare_kernels left right =
  let same_targets = ref true and maximum_difference = ref 0.0 in
  for source = 0 to Array.length left.Model_kernel.targets - 1 do
    let left_row = sorted_row left source
    and right_row = sorted_row right source in
    if List.length left_row <> List.length right_row then
      same_targets := false
    else
      List.iter2
        (fun (left_target, left_weight) (right_target, right_weight) ->
           if left_target <> right_target then same_targets := false;
           maximum_difference :=
             Float.max !maximum_difference
               (Float.abs (left_weight -. right_weight)))
        left_row right_row
  done;
  !same_targets, !maximum_difference

let transition_score transition source target =
  let source_index = Finite_domain.encode transition.Model_kernel.source source
  and target_index = Finite_domain.encode transition.Model_kernel.destination target in
  let score = ref 0.0 in
  Array.iteri
    (fun column candidate ->
       if candidate = target_index then
         score := transition.Model_kernel.probabilities.(source_index).(column))
    transition.targets.(source_index);
  !score

let same_path left right =
  Array.length left = Array.length right
  && Array.to_list left = Array.to_list right

let string_of_state state =
  Printf.sprintf "(%d,%d,%d)" state.x1 state.x2 state.m

let string_of_path domain path =
  Array.to_list (Array.map (Finite_domain.decode domain) path)
  |> List.map string_of_state
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

let () =
  let ordinary_nested =
    Model_kernel.instantiate Nested_kernel.transition [||]
  and ordinary_flat =
    remap_indexed_numeric
      ~source:Nested_kernel.state_typ
      ~destination:Nested_kernel.state_typ
      Flat_indexed.transition
  and max_nested =
    Trace_max_dist.remap_numeric
      ~source:Nested_kernel.state_typ
      ~destination:Nested_kernel.state_typ
      Nested_max.transition
  and max_flat =
    Trace_max_dist.remap_numeric
      ~source:Nested_kernel.state_typ
      ~destination:Nested_kernel.state_typ
      Flat_max.transition
  in
  let ordinary_targets_equal, ordinary_maximum_difference =
    compare_kernels ordinary_nested ordinary_flat
  in
  report
    "nested and flat descriptions induce the same ordinary kernel"
    (ordinary_targets_equal && ordinary_maximum_difference <= 1e-15)
    (Printf.sprintf "sources=%d maxdiff=%.1e"
       Nested_kernel.state_typ.Finite_domain.cardinal
       ordinary_maximum_difference);

  let representative = { x1 = 10; x2 = 10; m = 0 } in
  let same_heading = { x1 = 10; x2 = 11; m = 0 }
  and right_heading = { x1 = 11; x2 = 10; m = 1 }
  and opposite_heading = { x1 = 10; x2 = 9; m = 2 } in
  let nested_scores =
    [| transition_score max_nested representative same_heading;
       transition_score max_nested representative right_heading;
       transition_score max_nested representative opposite_heading |]
  and flat_scores =
    [| transition_score max_flat representative same_heading;
       transition_score max_flat representative right_heading;
       transition_score max_flat representative opposite_heading |]
  in
  report
    "max-over-internal-traces depends on the transition description"
    (close nested_scores.(0) 0.72
     && close nested_scores.(1) 0.12
     && close nested_scores.(2) 0.04
     && close flat_scores.(0) 0.76
     && close flat_scores.(1) 0.20
     && close flat_scores.(2) 0.04)
    (Printf.sprintf "nested=%.2f/%.2f/%.2f flat=%.2f/%.2f/%.2f"
       nested_scores.(0) nested_scores.(1) nested_scores.(2)
       flat_scores.(0) flat_scores.(1) flat_scores.(2));

  let initial = Kernel_interpreter.initial_vector [||] Nested_kernel.initial
  and emission = Model_kernel.instantiate Nested_kernel.emission [||]
  and observations =
    [| { y1 = 1; y2 = 1 };
       { y1 = 21; y2 = 21 };
       { y1 = 21; y2 = 21 };
       { y1 = 21; y2 = 21 };
       { y1 = 21; y2 = 21 } |]
  in
  let ordinary_path, ordinary_score =
    Kernel_inference_prob.Viterbi.run
      ~initial ~transition:ordinary_nested ~emission observations
  and nested_max_path, nested_max_score =
    Kernel_inference_prob.Viterbi.run
      ~initial ~transition:max_nested ~emission observations
  and flat_max_path, flat_max_score =
    Kernel_inference_prob.Viterbi.run
      ~initial ~transition:max_flat ~emission observations
  in
  Printf.printf
    "T=5 ordinary path: %s\nT=5 nested-max path: %s\nT=5 flat-max path: %s\nT=5 scores: ordinary=%.15g nested-max=%.15g flat-max=%.15g\n"
    (string_of_path ordinary_nested.Model_kernel.source ordinary_path)
    (string_of_path ordinary_nested.Model_kernel.source nested_max_path)
    (string_of_path ordinary_nested.Model_kernel.source flat_max_path)
    ordinary_score nested_max_score flat_max_score;
  report
    "T=5 flat-max decoding agrees with the ordinary kernel"
    (same_path flat_max_path ordinary_path && close flat_max_score ordinary_score)
    (Printf.sprintf "path=%b scorediff=%.1e"
       (same_path flat_max_path ordinary_path)
       (Float.abs (flat_max_score -. ordinary_score)));
  report
    "T=5 nested-max and flat-max decodings differ"
    (not (same_path nested_max_path flat_max_path)
     && close ordinary_score (-6.94033830718861)
     && close flat_max_score (-6.94033830718861)
     && close nested_max_score (-7.2483059012923))
    (Printf.sprintf "path=%b nested=%.15g flat=%.15g"
       (same_path nested_max_path flat_max_path)
       nested_max_score flat_max_score);
  finish ()
