(* Internal consistency checks for the finite-HMM interpretations. *)

open Check
open Grid_sensor

module Ordered_original = Original (Ordered_dist)
module Indexed_original = Original (Indexed_dist)
module Support_original = Original (Support_dist)
module Kernel_original = Original (Kernel_interpreter)
module Ordered_variant = Variant (Ordered_dist)
module Indexed_variant = Variant (Indexed_dist)
module Support_variant = Variant (Support_dist)
module Kernel_variant = Variant (Kernel_interpreter)

module Rows (P : Direct_evaluator.PROBABILITY_EVALUATOR) = struct
  let of_cdist src_typ dst_typ k =
    Array.init (P.cardinal src_typ) (fun i ->
        List.map (fun (y, p) -> (P.index dst_typ y, p)) (P.to_table (P.row k (P.value_of_index src_typ i))))
end
module Ordered_rows = Rows (Ordered_dist)
module Indexed_rows = Rows (Indexed_dist)

let support_rows src_typ (k : ('a, 'b) Support_dist.cdist) =
  Array.init (Support_dist.cardinal src_typ) (fun i -> Array.to_list (Support_dist.row_keys k i))

let kernel_rows ?(values = [||]) (k : ('a, 'b) Model_kernel.t) =
  let n = Model_kernel.instantiate k values in
  Array.mapi (fun i targets -> Array.to_list (Array.mapi (fun c t -> (t, n.probabilities.(i).(c))) targets)) n.targets

let keys rows = Array.map (List.map fst) rows

let valid_path state_count path =
  Array.fold_left
    (fun valid state -> valid && state >= 0 && state < state_count)
    true
    path

let equal_int_array left right =
  Array.length left = Array.length right
  && Array.to_list left = Array.to_list right

let () =
  let original_ordered = Ordered_rows.of_cdist Ordered_original.state_typ Ordered_original.state_typ Ordered_original.transition in
  let original_indexed = Indexed_rows.of_cdist Indexed_original.state_typ Indexed_original.state_typ Indexed_original.transition in
  let d = max_row_diff original_ordered original_indexed in
  report "Original transition: Ordered = Indexed" (d < 1e-15) (Printf.sprintf "maxdiff=%.1e" d);
  report "Original transition: Support = positive entries"
    (same_keys (support_rows Support_original.state_typ Support_original.transition) (keys original_indexed)) "";
  let d = max_row_diff (kernel_rows Kernel_original.transition) original_indexed in
  report "Original transition: Kernel = Indexed" (d < 1e-15)
    (Printf.sprintf "maxdiff=%.1e nnz=%d constant=%b" d (Model_kernel.nnz Kernel_original.transition)
       (Model_kernel.is_constant Kernel_original.transition));
  let variant_ordered = Ordered_rows.of_cdist Ordered_variant.state_typ Ordered_variant.state_typ Ordered_variant.transition in
  let variant_indexed = Indexed_rows.of_cdist Indexed_variant.state_typ Indexed_variant.state_typ Indexed_variant.transition in
  let d = max_row_diff variant_ordered variant_indexed in
  report "Variant transition: Ordered = Indexed" (d < 1e-15) (Printf.sprintf "maxdiff=%.1e" d);
  report "Variant transition: Support = positive entries"
    (same_keys (support_rows Support_variant.state_typ Support_variant.transition) (keys variant_indexed)) "";
  let d = max_row_diff (kernel_rows Kernel_variant.transition) variant_indexed in
  report "Variant transition: Kernel = Indexed" (d < 1e-15) (Printf.sprintf "maxdiff=%.1e" d);
  let representative = { x1 = 10; x2 = 10; m = 0 } in
  let source = Finite_domain.encode Kernel_variant.state_typ representative in
  let kernel_row = Kernel_variant.transition.Model_kernel.rows.(source) in
  let targets =
    Array.to_list
      (Array.map (fun (edge : Model_kernel.edge) -> edge.target) kernel_row)
  in
  let weights =
    List.sort Float.compare
      (Array.to_list
         (Array.map
            (fun (edge : Model_kernel.edge) -> Weight.eval [||] edge.probability)
            kernel_row))
  in
  report "Variant kernel row is canonical with aggregated 0.04 / 0.20 / 0.76"
    (Array.length kernel_row = 3
     && List.length (List.sort_uniq Int.compare targets) = 3
     && match weights with
        | [ a; b; c ] ->
            Float.abs (a -. 0.04) < 1e-12
            && Float.abs (b -. 0.20) < 1e-12
            && Float.abs (c -. 0.76) < 1e-12
        | _ -> false)
    (String.concat " " (List.map (Printf.sprintf "%.3f") weights));
  let variant_emission_ordered = Ordered_rows.of_cdist Ordered_variant.state_typ Ordered_variant.obs_typ Ordered_variant.emission in
  let variant_emission_indexed = Indexed_rows.of_cdist Indexed_variant.state_typ Indexed_variant.obs_typ Indexed_variant.emission in
  report "Variant emission: Ordered = Indexed" (max_row_diff variant_emission_ordered variant_emission_indexed < 1e-15) "";
  let row = Indexed_dist.to_table (Indexed_dist.row Indexed_variant.transition { x1 = 10; x2 = 10; m = 0 }) in
  let ws = List.sort Float.compare (List.map snd row) in
  report "Variant capp merges internal traces into 0.04 / 0.20 / 0.76"
    (match ws with [ a; b; c ] -> Float.abs (a -. 0.04) < 1e-12 && Float.abs (b -. 0.2) < 1e-12
                                   && Float.abs (c -. 0.76) < 1e-12 | _ -> false)
    (String.concat " " (List.map (Printf.sprintf "%.3f") ws));
  List.iter (fun (name, check) ->
      report name (match check () with () -> true | exception Invalid_argument _ -> false) "")
    [ ("Original row-sum polynomial is 1 within tolerance", fun () -> Model_kernel.check_stochastic Kernel_original.transition);
      ("Variant row-sum polynomial is 1 within tolerance", fun () -> Model_kernel.check_stochastic Kernel_variant.transition);
      ("Variant parameterized row-sum polynomial is 1 within tolerance",
       fun () -> Model_kernel.check_stochastic (Kernel_variant.transition_parameterized ())) ];
  expect_invalid "capp rejects distinct domains of the same OCaml type" (fun () ->
      let left = Indexed_dist.tint_range ~name:"left" 2 and right = Indexed_dist.tint_range ~name:"right" 2 in
      Indexed_dist.capp (Indexed_dist.dist left [ (0, 1.0) ])
        (Indexed_dist.cdist right right (fun v -> Indexed_dist.eta right v)));
  expect_invalid "dist rejects a literal whose weights do not sum to 1" (fun () ->
      Indexed_dist.dist Indexed_original.mode_typ [ (0, 0.5) ]);
  expect_invalid "kernel dist rejects a non-stochastic parameterized literal" (fun () ->
      Kernel_interpreter.dist Kernel_original.mode_typ [ (0, Kernel_interpreter.parameter keep_parameter) ]);
  expect_invalid "model kernel rejects a duplicate target within one row" (fun () ->
      let typ = Finite_domain.int_range ~name:"duplicate-target-test" 1 in
      Model_kernel.make ~src:typ ~dst:typ
        ~rows:[| [| { Model_kernel.target = 0; probability = Weight.const 0.5 };
                      { Model_kernel.target = 0; probability = Weight.const 0.5 } |] |]);
  expect_invalid "instantiate rejects p outside (0,1)" (fun () ->
      Model_kernel.instantiate (Kernel_original.transition_parameterized ()) [| 1.0 |]);
  let tr = Model_kernel.instantiate Kernel_variant.transition [||]
  and em = Model_kernel.instantiate Kernel_variant.emission [||] in
  let init = Kernel_interpreter.initial_vector [||] Kernel_variant.initial in
  let obs = Array.init 50 (fun t -> if t = 0 then { y1 = 1; y2 = 1 } else { y1 = 21; y2 = 21 }) in
  let _, llp = Kernel_inference_prob.Forward.run ~initial:init ~transition:tr ~emission:em obs in
  let _, lll = Kernel_inference_log.Forward.run ~initial:init ~transition:tr ~emission:em obs in
  report "Variant Forward: prob domain = log domain" (Float.abs (llp -. lll) < 1e-9 || (llp = neg_infinity && lll = neg_infinity))
    (Printf.sprintf "%.12f vs %.12f" llp lll);
  let _, vp = Kernel_inference_prob.Viterbi.run ~initial:init ~transition:tr ~emission:em obs in
  let _, vl = Kernel_inference_log.Viterbi.run ~initial:init ~transition:tr ~emission:em obs in
  report "Variant Viterbi: prob domain = log domain" (Float.abs (vp -. vl) < 1e-9 || (vp = neg_infinity && vl = neg_infinity))
    (Printf.sprintf "%.12f vs %.12f" vp vl);
  let zero_likelihood_obs =
    [| { y1 = 2; y2 = 2 }; { y1 = 21; y2 = 21 }; { y1 = 21; y2 = 21 } |]
  in
  let probability_path, probability_score =
    Kernel_inference_prob.Viterbi.run
      ~initial:init ~transition:tr ~emission:em zero_likelihood_obs
  in
  let log_path, log_score =
    Kernel_inference_log.Viterbi.run
      ~initial:init ~transition:tr ~emission:em zero_likelihood_obs
  in
  report
    "zero-likelihood Viterbi: probability and log paths are valid and consistent"
    (valid_path (Array.length init) probability_path
     && valid_path (Array.length init) log_path
     && equal_int_array probability_path log_path)
    (Printf.sprintf
       "probability=%s log=%s"
       (String.concat "," (List.map string_of_int (Array.to_list probability_path)))
       (String.concat "," (List.map string_of_int (Array.to_list log_path))));
  report
    "zero-likelihood Viterbi: probability score = log score"
    (probability_score = neg_infinity && log_score = neg_infinity)
    (Printf.sprintf "%.12f vs %.12f" probability_score log_score);
  finish ()
