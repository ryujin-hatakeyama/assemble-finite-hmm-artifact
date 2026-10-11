(* Cross-check the two MetaOCaml transition lowerings against the ordinary
   probability-domain traversal. Parameterized tests compile each symbolic
   transition once and then reuse the
   generated Forward / Viterbi functions for several runtime parameter
   valuations. *)

open Check
open Grid_sensor

module Kernel_original = Original (Kernel_interpreter)
module Kernel_variant = Variant (Kernel_interpreter)
module Direct_sum =
  Kernel_inference_prob.Traverse (Kernel_inference_prob.Sum_reducer)

let retained_zero_parameter =
  Parameter.make ~index:0 ~name:"retained-zero-p"

(* A deliberately tiny test fixture.  Two independent Bernoulli choices are
   merged into two destinations: destination 0 has weight (1-p)^2, while
   destination 1 receives the other three derivations.  Thus every transition
   row is stochastic in exact arithmetic. *)
module Retained_zero_fixture (M : Model_language.MODEL) = struct
  let state_typ = M.tint_range ~name:"retained-zero-state" 2
  let observation_typ = M.tint_range ~name:"retained-zero-observation" 1
  let choice_typ = M.tint_range ~name:"retained-zero-choice" 2

  let choice () =
    M.dist choice_typ
      [ 0, M.complement retained_zero_parameter;
        1, M.parameter retained_zero_parameter ]

  let transition =
    M.cdist state_typ state_typ (fun _source ->
        M.map2 state_typ
          (fun left right -> if left = 0 && right = 0 then 0 else 1)
          (choice ())
          (choice ()))

  let initial = M.dist state_typ [ 0, M.const 1.0 ]

  let emission =
    M.cdist state_typ observation_typ (fun _state ->
        M.dist observation_typ [ 0, M.const 1.0 ])
end

module Retained_zero_kernel = Retained_zero_fixture (Kernel_interpreter)

let max_abs_diff left right =
  if Array.length left <> Array.length right then infinity
  else begin
    let difference = ref 0.0 in
    Array.iteri
      (fun index x ->
         difference := Float.max !difference (Float.abs (x -. right.(index))))
      left;
    !difference
  end

let direct_push transition current =
  let next = Array.make (Array.length current) 0.0 in
  let unused_backpointers = Array.make (Array.length current) (-1) in
  Direct_sum.push transition current next unused_backpointers;
  next

let staged_push compiled current =
  let next = Array.make (Array.length current) nan in
  compiled current next;
  next

let normalized_dense_input cardinal =
  let values =
    Array.init cardinal (fun index -> float_of_int ((index mod 17) + 1))
  in
  let mass = Array.fold_left ( +. ) 0.0 values in
  Array.map (fun x -> x /. mass) values

let count_positive values =
  Array.fold_left (fun count x -> if x > 0.0 then count + 1 else count) 0 values

let observations length =
  Array.init length (fun time ->
      if time = 0 then { y1 = 1; y2 = 1 }
      else { y1 = 21; y2 = 21 })

let check_forward name initial symbolic_transition symbolic_emission observations =
  let numeric_transition = Model_kernel.instantiate symbolic_transition [||] in
  let numeric_emission = Model_kernel.instantiate symbolic_emission [||] in
  let expected_posterior, expected_ll =
    Kernel_inference_prob.Forward.run
      ~initial
      ~transition:numeric_transition
      ~emission:numeric_emission
      observations
  in
  let compile_and_run lowering =
    let compiled, compile_seconds =
      time (fun () ->
          Kernel_staged.compile_forward
            ~lowering
            ~initial
            ~transition:symbolic_transition
            ~emission:symbolic_emission)
    in
    let posterior, ll = compiled observations in
    posterior, ll, compile_seconds
  in
  let packed_posterior, packed_ll, packed_compile =
    compile_and_run Kernel_staged.Packed
  in
  let embedded_posterior, embedded_ll, embedded_compile =
    compile_and_run Kernel_staged.Embedded
  in
  let packed_posterior_diff = max_abs_diff expected_posterior packed_posterior in
  let embedded_posterior_diff = max_abs_diff expected_posterior embedded_posterior in
  let lowerings_posterior_diff = max_abs_diff packed_posterior embedded_posterior in
  let packed_ll_diff = Float.abs (expected_ll -. packed_ll) in
  let embedded_ll_diff = Float.abs (expected_ll -. embedded_ll) in
  let lowerings_ll_diff = Float.abs (packed_ll -. embedded_ll) in
  report
    (name ^ ": Packed Forward = kernel Forward, T=300")
    (packed_posterior_diff < 1e-12 &&
     (packed_ll_diff < 1e-12 ||
      (expected_ll = neg_infinity && packed_ll = neg_infinity)))
    (Printf.sprintf
       "posterior=%.1e ll=%.1e compile=%.3fs"
       packed_posterior_diff packed_ll_diff packed_compile);
  report
    (name ^ ": Embedded Forward = kernel Forward, T=300")
    (embedded_posterior_diff < 1e-12 &&
     (embedded_ll_diff < 1e-12 ||
      (expected_ll = neg_infinity && embedded_ll = neg_infinity)))
    (Printf.sprintf
       "posterior=%.1e ll=%.1e compile=%.3fs"
       embedded_posterior_diff embedded_ll_diff embedded_compile);
  report
    (name ^ ": Packed Forward = Embedded Forward, T=300")
    (lowerings_posterior_diff < 1e-12 &&
     (lowerings_ll_diff < 1e-12 ||
      (packed_ll = neg_infinity && embedded_ll = neg_infinity)))
    (Printf.sprintf
       "posterior=%.1e ll=%.1e"
       lowerings_posterior_diff lowerings_ll_diff)

let check_parameterized_forward
    name initial symbolic_transition symbolic_emission observations =
  let numeric_emission = Model_kernel.instantiate symbolic_emission [||] in
  let compile lowering =
    time (fun () ->
        Kernel_staged.compile_forward_parameterized
          ~lowering
          ~initial
          ~transition:symbolic_transition
          ~emission:symbolic_emission)
  in
  let packed_forward, packed_compile = compile Kernel_staged.Packed in
  let embedded_forward, embedded_compile = compile Kernel_staged.Embedded in
  List.iter
    (fun p ->
       let parameters = [| p |] in
       let numeric_transition =
         Model_kernel.instantiate symbolic_transition parameters
       in
       let expected_posterior, expected_ll =
         Kernel_inference_prob.Forward.run
           ~initial
           ~transition:numeric_transition
           ~emission:numeric_emission
           observations
       in
       let packed_posterior, packed_ll =
         packed_forward parameters observations
       in
       let embedded_posterior, embedded_ll =
         embedded_forward parameters observations
       in
       let packed_posterior_diff =
         max_abs_diff expected_posterior packed_posterior
       in
       let embedded_posterior_diff =
         max_abs_diff expected_posterior embedded_posterior
       in
       let lowerings_posterior_diff =
         max_abs_diff packed_posterior embedded_posterior
       in
       let packed_ll_diff = Float.abs (expected_ll -. packed_ll) in
       let embedded_ll_diff = Float.abs (expected_ll -. embedded_ll) in
       let lowerings_ll_diff = Float.abs (packed_ll -. embedded_ll) in
       report
         (Printf.sprintf
            "%s-param: p=%.2f Packed Forward = instantiated kernel"
            name p)
         (packed_posterior_diff < 1e-12 && packed_ll_diff < 1e-12)
         (Printf.sprintf
            "posterior=%.1e ll=%.1e compile=%.3fs"
            packed_posterior_diff packed_ll_diff packed_compile);
       report
         (Printf.sprintf
            "%s-param: p=%.2f Embedded Forward = instantiated kernel"
            name p)
         (embedded_posterior_diff < 1e-12 && embedded_ll_diff < 1e-12)
         (Printf.sprintf
            "posterior=%.1e ll=%.1e compile=%.3fs"
            embedded_posterior_diff embedded_ll_diff embedded_compile);
       report
         (Printf.sprintf
            "%s-param: p=%.2f Packed Forward = Embedded Forward"
            name p)
         (lowerings_posterior_diff < 1e-12 && lowerings_ll_diff < 1e-12)
         (Printf.sprintf
            "posterior=%.1e ll=%.1e"
            lowerings_posterior_diff lowerings_ll_diff))
    [ 0.60; 0.70; 0.80; 0.95 ]


let equal_int_array left right =
  Array.length left = Array.length right
  && begin
       let equal = ref true in
       for index = 0 to Array.length left - 1 do
         if left.(index) <> right.(index) then equal := false
       done;
       !equal
     end

(* Test-only numeric oracle: evaluate the same symbolic rows and omit entries
   whose current floating value is zero.  Comparing against this oracle checks
   that retaining the zero in the static topology is neutral for inference. *)
let numeric_omitting_runtime_zeros
    (kernel : ('a, 'b) Model_kernel.t)
    (parameters : Parameter.values) : ('a, 'b) Model_kernel.numeric =
  Parameter.check_values parameters;
  let rows =
    Array.mapi
      (fun row_index row ->
         let evaluated =
           Array.to_list
             (Array.map
                (fun (edge : Model_kernel.edge) ->
                   let probability = Weight.eval parameters edge.probability in
                   Model_kernel.validate_probability
                     ~context:(Printf.sprintf
                         "retained-zero oracle row %d"
                         row_index)
                     probability;
                   edge.target, probability)
                row)
         in
         let retained =
           Array.of_list
             (List.filter (fun (_target, probability) -> probability <> 0.0) evaluated)
         in
         Model_kernel.validate_row_mass
           ~context:(Printf.sprintf "retained-zero oracle row %d" row_index)
           (Array.map snd retained);
         retained)
      kernel.Model_kernel.rows
  in
  { Model_kernel.source = kernel.Model_kernel.src;
    destination = kernel.Model_kernel.dst;
    targets = Array.map (Array.map fst) rows;
    probabilities = Array.map (Array.map snd) rows }

let characterize_retained_runtime_zero () =
  let parameter = Float.pred 1.0 in
  let parameters = [| parameter |] in
  let parameter_accepted =
    match Parameter.check_values parameters with
    | () -> true
    | exception Invalid_argument _ -> false
  in
  report
    "retained-zero characterization: Float.pred 1 is admissible"
    (parameter_accepted && parameter > 0.0 && parameter < 1.0)
    (Printf.sprintf "p=%.17g" parameter);

  let complement = 1.0 -. parameter in
  let factored_weight = complement *. complement in
  report
    "retained-zero characterization: abstract (1-p)^2 is positive"
    (complement > 0.0 && factored_weight > 0.0)
    (Printf.sprintf "1-p=%.17g factored=%.17g" complement factored_weight);

  let transition = Retained_zero_kernel.transition in
  let first_row = transition.Model_kernel.rows.(0) in
  let zero_edge =
    match
      List.find_opt
        (fun (edge : Model_kernel.edge) -> edge.target = 0)
        (Array.to_list first_row)
    with
    | Some edge -> edge
    | None -> failwith "retained-zero fixture lost destination 0"
  in
  let evaluated_zero = Weight.eval parameters zero_edge.probability in
  report
    "retained-zero characterization: symbolic edge is structurally positive"
    zero_edge.probability.Weight.positive
    (Printf.sprintf "static-row-width=%d" (Array.length first_row));
  report
    "retained-zero characterization: expanded evaluation is exactly zero"
    (evaluated_zero = 0.0)
    (Printf.sprintf "expanded=%.17g factored=%.17g" evaluated_zero factored_weight);
  report
    "retained-zero characterization: symbolic fixture is stochastic"
    (match Model_kernel.check_stochastic transition with
     | () -> true
     | exception Invalid_argument _ -> false)
    "two Bernoulli choices partition all four derivations";

  let instantiated_transition =
    Model_kernel.instantiate transition parameters
  in
  report
    "retained-zero policy: instantiate retains the zero edge"
    (Array.length instantiated_transition.Model_kernel.targets.(0) = 2
     && instantiated_transition.Model_kernel.targets.(0).(0) = 0
     && instantiated_transition.Model_kernel.probabilities.(0).(0) = 0.0)
    (Printf.sprintf
       "numeric-row-width=%d probability=%.17g"
       (Array.length instantiated_transition.Model_kernel.targets.(0))
       instantiated_transition.Model_kernel.probabilities.(0).(0));

  let evaluated_entries =
    Array.to_list
      (Array.map
         (fun (edge : Model_kernel.edge) ->
            edge.target, Weight.eval parameters edge.probability)
         first_row)
  in
  let omitted_entries =
    List.filter (fun (_target, probability) -> probability <> 0.0) evaluated_entries
  in
  let direct_state_typ =
    Indexed_dist.tint_range ~name:"retained-zero-direct-state" 2
  in
  let direct_initial =
    Indexed_dist.dist direct_state_typ [ 0, Indexed_dist.const 1.0 ]
  in
  let direct_transition entries =
    Indexed_dist.cdist direct_state_typ direct_state_typ (fun _source ->
        Indexed_dist.dist direct_state_typ entries)
  in
  let direct_with_zero =
    Indexed_dist.capp direct_initial (direct_transition evaluated_entries)
    |> Indexed_dist.to_table
  in
  let direct_without_zero =
    Indexed_dist.capp direct_initial (direct_transition omitted_entries)
    |> Indexed_dist.to_table
  in
  report
    "retained-zero characterization: Indexed drops zero like omission"
    (direct_with_zero = direct_without_zero
     && direct_with_zero = [ 1, 1.0 ])
    (Printf.sprintf "retained-entries=%d" (List.length direct_with_zero));

  let oracle_transition =
    numeric_omitting_runtime_zeros transition parameters
  in
  let numeric_emission =
    Model_kernel.instantiate Retained_zero_kernel.emission [||]
  in
  let initial =
    Kernel_interpreter.initial_vector [||] Retained_zero_kernel.initial
  in
  let observations = [| 0; 0; 0 |] in
  let expected_posterior, expected_forward_score =
    Kernel_inference_prob.Forward.run
      ~initial
      ~transition:oracle_transition
      ~emission:numeric_emission
      observations
  in
  let reference_posterior, reference_forward_score =
    Kernel_inference_prob.Forward.run
      ~initial
      ~transition:instantiated_transition
      ~emission:numeric_emission
      observations
  in
  report
    "retained-zero policy: reference Forward = omitted-zero oracle"
    (max_abs_diff expected_posterior reference_posterior < 1e-15
     && Float.abs (expected_forward_score -. reference_forward_score) < 1e-15)
    (Printf.sprintf
       "posterior=%.1e score=%.1e"
       (max_abs_diff expected_posterior reference_posterior)
       (Float.abs (expected_forward_score -. reference_forward_score)));
  let packed_forward =
    Kernel_staged.compile_forward_parameterized
      ~lowering:Kernel_staged.Packed
      ~initial
      ~transition
      ~emission:Retained_zero_kernel.emission
  in
  let embedded_forward =
    Kernel_staged.compile_forward_parameterized
      ~lowering:Kernel_staged.Embedded
      ~initial
      ~transition
      ~emission:Retained_zero_kernel.emission
  in
  let packed_posterior, packed_forward_score =
    packed_forward parameters observations
  in
  let embedded_posterior, embedded_forward_score =
    embedded_forward parameters observations
  in
  report
    "retained-zero characterization: Packed Forward = omitted-zero oracle"
    (max_abs_diff expected_posterior packed_posterior < 1e-15
     && Float.abs (expected_forward_score -. packed_forward_score) < 1e-15)
    (Printf.sprintf
       "posterior=%.1e score=%.1e"
       (max_abs_diff expected_posterior packed_posterior)
       (Float.abs (expected_forward_score -. packed_forward_score)));
  report
    "retained-zero characterization: Embedded Forward = omitted-zero oracle"
    (max_abs_diff expected_posterior embedded_posterior < 1e-15
     && Float.abs (expected_forward_score -. embedded_forward_score) < 1e-15)
    (Printf.sprintf
       "posterior=%.1e score=%.1e"
       (max_abs_diff expected_posterior embedded_posterior)
       (Float.abs (expected_forward_score -. embedded_forward_score)));

  let expected_path, expected_viterbi_score =
    Kernel_inference_prob.Viterbi.run
      ~initial
      ~transition:oracle_transition
      ~emission:numeric_emission
      observations
  in
  report
    "retained-zero characterization: oracle has a positive winning path"
    (equal_int_array expected_path [| 0; 1; 1 |]
     && expected_viterbi_score = 0.0)
    (Printf.sprintf "path=%d,%d,%d score=%.17g"
       expected_path.(0) expected_path.(1) expected_path.(2)
       expected_viterbi_score);
  let reference_path, reference_viterbi_score =
    Kernel_inference_prob.Viterbi.run
      ~initial
      ~transition:instantiated_transition
      ~emission:numeric_emission
      observations
  in
  report
    "retained-zero policy: reference Viterbi path/score = oracle"
    (equal_int_array expected_path reference_path
     && Float.abs (expected_viterbi_score -. reference_viterbi_score) < 1e-15)
    (Printf.sprintf
       "path=%b score=%.1e"
       (equal_int_array expected_path reference_path)
       (Float.abs (expected_viterbi_score -. reference_viterbi_score)));
  let packed_viterbi =
    Kernel_staged.compile_viterbi_parameterized
      ~lowering:Kernel_staged.Packed
      ~initial
      ~transition
      ~emission:Retained_zero_kernel.emission
  in
  let embedded_viterbi =
    Kernel_staged.compile_viterbi_parameterized
      ~lowering:Kernel_staged.Embedded
      ~initial
      ~transition
      ~emission:Retained_zero_kernel.emission
  in
  let packed_path, packed_viterbi_score =
    packed_viterbi parameters observations
  in
  let embedded_path, embedded_viterbi_score =
    embedded_viterbi parameters observations
  in
  report
    "retained-zero characterization: Packed Viterbi path/score = oracle"
    (equal_int_array expected_path packed_path
     && Float.abs (expected_viterbi_score -. packed_viterbi_score) < 1e-15)
    (Printf.sprintf
       "path=%b score=%.1e"
       (equal_int_array expected_path packed_path)
       (Float.abs (expected_viterbi_score -. packed_viterbi_score)));
  report
    "retained-zero characterization: Embedded Viterbi path/score = oracle"
    (equal_int_array expected_path embedded_path
     && Float.abs (expected_viterbi_score -. embedded_viterbi_score) < 1e-15)
    (Printf.sprintf
       "path=%b score=%.1e"
       (equal_int_array expected_path embedded_path)
       (Float.abs (expected_viterbi_score -. embedded_viterbi_score)))

let check_viterbi name initial symbolic_transition symbolic_emission observations =
  let numeric_transition = Model_kernel.instantiate symbolic_transition [||] in
  let numeric_emission = Model_kernel.instantiate symbolic_emission [||] in
  let expected_path, expected_score =
    Kernel_inference_prob.Viterbi.run
      ~initial
      ~transition:numeric_transition
      ~emission:numeric_emission
      observations
  in
  let compile_and_run lowering =
    let compiled, compile_seconds =
      time (fun () ->
          Kernel_staged.compile_viterbi
            ~lowering
            ~initial
            ~transition:symbolic_transition
            ~emission:symbolic_emission)
    in
    let path, score = compiled observations in
    path, score, compile_seconds
  in
  let packed_path, packed_score, packed_compile =
    compile_and_run Kernel_staged.Packed
  in
  let embedded_path, embedded_score, embedded_compile =
    compile_and_run Kernel_staged.Embedded
  in
  let packed_score_diff = Float.abs (expected_score -. packed_score) in
  let embedded_score_diff = Float.abs (expected_score -. embedded_score) in
  let lowerings_score_diff = Float.abs (packed_score -. embedded_score) in
  report
    (name ^ ": Packed Viterbi = kernel Viterbi, T=300")
    (equal_int_array expected_path packed_path && packed_score_diff < 1e-12)
    (Printf.sprintf
       "path=%b score=%.1e compile=%.3fs"
       (equal_int_array expected_path packed_path)
       packed_score_diff
       packed_compile);
  report
    (name ^ ": Embedded Viterbi = kernel Viterbi, T=300")
    (equal_int_array expected_path embedded_path && embedded_score_diff < 1e-12)
    (Printf.sprintf
       "path=%b score=%.1e compile=%.3fs"
       (equal_int_array expected_path embedded_path)
       embedded_score_diff
       embedded_compile);
  report
    (name ^ ": Packed Viterbi = Embedded Viterbi, T=300")
    (equal_int_array packed_path embedded_path && lowerings_score_diff < 1e-12)
    (Printf.sprintf
       "path=%b score=%.1e"
       (equal_int_array packed_path embedded_path)
       lowerings_score_diff)

let check_parameterized_viterbi
    name initial symbolic_transition symbolic_emission observations =
  let numeric_emission = Model_kernel.instantiate symbolic_emission [||] in
  let compile lowering =
    time (fun () ->
        Kernel_staged.compile_viterbi_parameterized
          ~lowering
          ~initial
          ~transition:symbolic_transition
          ~emission:symbolic_emission)
  in
  let packed_viterbi, packed_compile = compile Kernel_staged.Packed in
  let embedded_viterbi, embedded_compile = compile Kernel_staged.Embedded in
  List.iter
    (fun p ->
       let parameters = [| p |] in
       let numeric_transition =
         Model_kernel.instantiate symbolic_transition parameters
       in
       let expected_path, expected_score =
         Kernel_inference_prob.Viterbi.run
           ~initial
           ~transition:numeric_transition
           ~emission:numeric_emission
           observations
       in
       let packed_path, packed_score =
         packed_viterbi parameters observations
       in
       let embedded_path, embedded_score =
         embedded_viterbi parameters observations
       in
       let packed_score_diff = Float.abs (expected_score -. packed_score) in
       let embedded_score_diff = Float.abs (expected_score -. embedded_score) in
       let lowerings_score_diff = Float.abs (packed_score -. embedded_score) in
       report
         (Printf.sprintf
            "%s-param: p=%.2f Packed Viterbi = instantiated kernel"
            name p)
         (equal_int_array expected_path packed_path && packed_score_diff < 1e-12)
         (Printf.sprintf
            "path=%b score=%.1e compile=%.3fs"
            (equal_int_array expected_path packed_path)
            packed_score_diff
            packed_compile);
       report
         (Printf.sprintf
            "%s-param: p=%.2f Embedded Viterbi = instantiated kernel"
            name p)
         (equal_int_array expected_path embedded_path && embedded_score_diff < 1e-12)
         (Printf.sprintf
            "path=%b score=%.1e compile=%.3fs"
            (equal_int_array expected_path embedded_path)
            embedded_score_diff
            embedded_compile);
       report
         (Printf.sprintf
            "%s-param: p=%.2f Packed Viterbi = Embedded Viterbi"
            name p)
         (equal_int_array packed_path embedded_path && lowerings_score_diff < 1e-12)
         (Printf.sprintf
            "path=%b score=%.1e"
            (equal_int_array packed_path embedded_path)
            lowerings_score_diff))
    [ 0.60; 0.70; 0.80; 0.95 ]

let valid_path state_count path =
  Array.fold_left
    (fun valid state -> valid && state >= 0 && state < state_count)
    true
    path

let check_zero_likelihood_viterbi () =
  let initial = Kernel_interpreter.initial_vector [||] Kernel_variant.initial in
  let transition = Model_kernel.instantiate Kernel_variant.transition [||] in
  let emission = Model_kernel.instantiate Kernel_variant.emission [||] in
  let observations =
    [| { y1 = 2; y2 = 2 }; { y1 = 21; y2 = 21 }; { y1 = 21; y2 = 21 } |]
  in
  let expected_path, expected_score =
    Kernel_inference_prob.Viterbi.run
      ~initial ~transition ~emission observations
  in
  let compile_and_run lowering =
    Kernel_staged.compile_viterbi
      ~lowering
      ~initial
      ~transition:Kernel_variant.transition
      ~emission:Kernel_variant.emission
    |> fun viterbi -> viterbi observations
  in
  let packed_path, packed_score = compile_and_run Kernel_staged.Packed in
  let embedded_path, embedded_score = compile_and_run Kernel_staged.Embedded in
  let state_count = Array.length initial in
  report
    "zero-likelihood Viterbi: ordinary, Packed, and Embedded paths are valid"
    (valid_path state_count expected_path
     && valid_path state_count packed_path
     && valid_path state_count embedded_path
     && equal_int_array expected_path packed_path
     && equal_int_array expected_path embedded_path)
    (Printf.sprintf
       "ordinary=%s packed=%s embedded=%s"
       (String.concat "," (List.map string_of_int (Array.to_list expected_path)))
       (String.concat "," (List.map string_of_int (Array.to_list packed_path)))
       (String.concat "," (List.map string_of_int (Array.to_list embedded_path))));
  report
    "zero-likelihood Viterbi: ordinary, Packed, and Embedded scores agree"
    (expected_score = neg_infinity
     && packed_score = neg_infinity
     && embedded_score = neg_infinity)
    (Printf.sprintf "ordinary=%.12f packed=%.12f embedded=%.12f"
       expected_score packed_score embedded_score);

  let parameters = [| 0.80 |] in
  let parameterized_transition = Kernel_variant.transition_parameterized () in
  let compile_parameterized_and_run lowering =
    Kernel_staged.compile_viterbi_parameterized
      ~lowering
      ~initial
      ~transition:parameterized_transition
      ~emission:Kernel_variant.emission
    |> fun viterbi -> viterbi parameters observations
  in
  let packed_parameterized_path, packed_parameterized_score =
    compile_parameterized_and_run Kernel_staged.Packed
  in
  let embedded_parameterized_path, embedded_parameterized_score =
    compile_parameterized_and_run Kernel_staged.Embedded
  in
  report
    "zero-likelihood parameterized Viterbi: Packed and Embedded paths are valid"
    (valid_path state_count packed_parameterized_path
     && valid_path state_count embedded_parameterized_path
     && equal_int_array packed_parameterized_path embedded_parameterized_path)
    (Printf.sprintf
       "packed=%s embedded=%s"
       (String.concat ","
          (List.map string_of_int (Array.to_list packed_parameterized_path)))
       (String.concat ","
          (List.map string_of_int (Array.to_list embedded_parameterized_path))));
  report
    "zero-likelihood parameterized Viterbi: Packed and Embedded scores agree"
    (packed_parameterized_score = neg_infinity
     && embedded_parameterized_score = neg_infinity)
    (Printf.sprintf "packed=%.12f embedded=%.12f"
       packed_parameterized_score embedded_parameterized_score);

  let packed_transition = Kernel_staged.pack_rows transition in
  let packed_emission = Kernel_staged.pack_columns emission in
  let observation_indices =
    Array.map (Finite_domain.encode Kernel_variant.obs_typ) observations
  in
  let handwritten_score =
    Handwritten_packed_inference.viterbi_constant
      ~initial
      ~transition:packed_transition
      ~emission:packed_emission
      ~observation_count:Kernel_variant.obs_typ.Finite_domain.cardinal
      observation_indices
  in
  let zero_current = Array.make state_count 0.0 in
  let zero_next = Array.make state_count nan in
  let zero_backpointers = Array.make state_count (-1) in
  Handwritten_packed_inference.push_max_constant
    ~state_count
    ~transition:packed_transition
    zero_current
    zero_next
    zero_backpointers;
  let handwritten_path =
    Handwritten_packed_inference.backtrace
      [| [||]; Array.copy zero_backpointers; Array.copy zero_backpointers |]
      0
  in
  report
    "zero-likelihood Viterbi: handwritten packed backtrace is valid"
    (handwritten_score = neg_infinity
     && valid_path state_count handwritten_path
     && equal_int_array expected_path handwritten_path)
    (Printf.sprintf "path=%s score=%.12f"
       (String.concat "," (List.map string_of_int (Array.to_list handwritten_path)))
       handwritten_score)

let () =
  characterize_retained_runtime_zero ();
  check_zero_likelihood_viterbi ();

  let symbolic_transition = Kernel_variant.transition in
  let numeric_transition = Model_kernel.instantiate symbolic_transition [||] in
  let state_count = Kernel_variant.state_typ.Finite_domain.cardinal in
  let compiled_embedded_push, embedded_compile =
    time (fun () ->
        Kernel_staged.compile_push
          ~lowering:Kernel_staged.Embedded
          symbolic_transition)
  in
  let compiled_packed_push, packed_compile =
    time (fun () ->
        Kernel_staged.compile_push
          ~lowering:Kernel_staged.Packed
          symbolic_transition)
  in

  let representative = { x1 = 20; x2 = 20; m = 0 } in
  let source = Finite_domain.encode Kernel_variant.state_typ representative in
  let one_hot = Array.make state_count 0.0 in
  one_hot.(source) <- 1.0;
  let expected_one_hot = direct_push numeric_transition one_hot in
  let embedded_one_hot = staged_push compiled_embedded_push one_hot in
  let packed_one_hot = staged_push compiled_packed_push one_hot in
  let embedded_representative_diff =
    max_abs_diff expected_one_hot embedded_one_hot
  in
  let packed_representative_diff =
    max_abs_diff expected_one_hot packed_one_hot
  in
  report
    "Embedded Variant push: representative row = direct push"
    (embedded_representative_diff < 1e-15 && count_positive embedded_one_hot = 3)
    (Printf.sprintf
       "source=%d maxdiff=%.1e nonzero=%d compile=%.3fs"
       source
       embedded_representative_diff
       (count_positive embedded_one_hot)
       embedded_compile);
  report
    "Packed Variant push: representative row = direct push"
    (packed_representative_diff < 1e-15 && count_positive packed_one_hot = 3)
    (Printf.sprintf
       "source=%d maxdiff=%.1e nonzero=%d compile=%.3fs"
       source
       packed_representative_diff
       (count_positive packed_one_hot)
       packed_compile);

  let dense = normalized_dense_input state_count in
  let expected_dense = direct_push numeric_transition dense in
  let embedded_dense = staged_push compiled_embedded_push dense in
  let packed_dense = staged_push compiled_packed_push dense in
  let embedded_dense_diff = max_abs_diff expected_dense embedded_dense in
  let packed_dense_diff = max_abs_diff expected_dense packed_dense in
  let lowerings_dense_diff = max_abs_diff embedded_dense packed_dense in
  report
    "Embedded Variant push: all 6400 outputs = direct push"
    (embedded_dense_diff < 1e-15)
    (Printf.sprintf "maxdiff=%.1e" embedded_dense_diff);
  report
    "Packed Variant push: all 6400 outputs = direct push"
    (packed_dense_diff < 1e-15)
    (Printf.sprintf "maxdiff=%.1e" packed_dense_diff);
  report
    "Variant push: Packed = Embedded"
    (lowerings_dense_diff < 1e-15)
    (Printf.sprintf "maxdiff=%.1e" lowerings_dense_diff);

  let obs = observations 300 in
  check_forward
    "Original"
    (Kernel_interpreter.initial_vector [||] Kernel_original.initial)
    Kernel_original.transition
    Kernel_original.emission
    obs;
  check_forward
    "Variant"
    (Kernel_interpreter.initial_vector [||] Kernel_variant.initial)
    Kernel_variant.transition
    Kernel_variant.emission
    obs;

  check_viterbi
    "Original"
    (Kernel_interpreter.initial_vector [||] Kernel_original.initial)
    Kernel_original.transition
    Kernel_original.emission
    obs;
  check_viterbi
    "Variant"
    (Kernel_interpreter.initial_vector [||] Kernel_variant.initial)
    Kernel_variant.transition
    Kernel_variant.emission
    obs;

  let variant_parameterized_transition =
    Kernel_variant.transition_parameterized ()
  in
  let variant_parameterized_numeric =
    Model_kernel.instantiate variant_parameterized_transition [| 0.80 |]
  in
  let parameterized_embedded_push =
    Kernel_staged.compile_push_parameterized
      ~lowering:Kernel_staged.Embedded
      variant_parameterized_transition
  in
  let parameterized_packed_push =
    Kernel_staged.compile_push_parameterized
      ~lowering:Kernel_staged.Packed
      variant_parameterized_transition
  in
  let expected_parameterized_push =
    direct_push variant_parameterized_numeric dense
  in
  let embedded_parameterized_push =
    let next = Array.make state_count nan in
    parameterized_embedded_push [| 0.80 |] dense next;
    next
  in
  let packed_parameterized_push =
    let next = Array.make state_count nan in
    parameterized_packed_push [| 0.80 |] dense next;
    next
  in
  report
    "Variant parameterized push p=0.80: Embedded = instantiated kernel"
    (max_abs_diff expected_parameterized_push embedded_parameterized_push < 1e-15)
    (Printf.sprintf
       "maxdiff=%.1e"
       (max_abs_diff expected_parameterized_push embedded_parameterized_push));
  report
    "Variant parameterized push p=0.80: Packed = instantiated kernel"
    (max_abs_diff expected_parameterized_push packed_parameterized_push < 1e-15)
    (Printf.sprintf
       "maxdiff=%.1e"
       (max_abs_diff expected_parameterized_push packed_parameterized_push));

  check_parameterized_forward
    "Original"
    (Kernel_interpreter.initial_vector [||] Kernel_original.initial)
    (Kernel_original.transition_parameterized ())
    Kernel_original.emission
    obs;
  check_parameterized_forward
    "Variant"
    (Kernel_interpreter.initial_vector [||] Kernel_variant.initial)
    variant_parameterized_transition
    Kernel_variant.emission
    obs;

  check_parameterized_viterbi
    "Original"
    (Kernel_interpreter.initial_vector [||] Kernel_original.initial)
    (Kernel_original.transition_parameterized ())
    Kernel_original.emission
    obs;
  check_parameterized_viterbi
    "Variant"
    (Kernel_interpreter.initial_vector [||] Kernel_variant.initial)
    variant_parameterized_transition
    Kernel_variant.emission
    obs;

  expect_invalid
    "constant Embedded push rejects parameter-dependent transition"
    (fun () ->
       Kernel_staged.compile_push
         ~lowering:Kernel_staged.Embedded
         (Kernel_variant.transition_parameterized ()));

  expect_invalid
    "constant Packed push rejects parameter-dependent transition"
    (fun () ->
       Kernel_staged.compile_push
         ~lowering:Kernel_staged.Packed
         (Kernel_variant.transition_parameterized ()));

  expect_invalid
    "constant Packed Forward rejects parameter-dependent transition"
    (fun () ->
       Kernel_staged.compile_forward
         ~lowering:Kernel_staged.Packed
         ~initial:(Kernel_interpreter.initial_vector [||] Kernel_variant.initial)
         ~transition:(Kernel_variant.transition_parameterized ())
         ~emission:Kernel_variant.emission);

  expect_invalid
    "constant Packed Viterbi rejects parameter-dependent transition"
    (fun () ->
       Kernel_staged.compile_viterbi
         ~lowering:Kernel_staged.Packed
         ~initial:(Kernel_interpreter.initial_vector [||] Kernel_variant.initial)
         ~transition:(Kernel_variant.transition_parameterized ())
         ~emission:Kernel_variant.emission);

  let parameterized_packed_forward =
    Kernel_staged.compile_forward_parameterized
      ~lowering:Kernel_staged.Packed
      ~initial:(Kernel_interpreter.initial_vector [||] Kernel_variant.initial)
      ~transition:variant_parameterized_transition
      ~emission:Kernel_variant.emission
  in
  expect_invalid
    "parameterized Forward rejects missing parameter value"
    (fun () -> parameterized_packed_forward [||] obs);
  expect_invalid
    "parameterized Forward rejects p=1"
    (fun () -> parameterized_packed_forward [| 1.0 |] obs);

  let parameterized_packed_viterbi =
    Kernel_staged.compile_viterbi_parameterized
      ~lowering:Kernel_staged.Packed
      ~initial:(Kernel_interpreter.initial_vector [||] Kernel_variant.initial)
      ~transition:variant_parameterized_transition
      ~emission:Kernel_variant.emission
  in
  expect_invalid
    "parameterized Viterbi rejects missing parameter value"
    (fun () -> parameterized_packed_viterbi [||] obs);
  expect_invalid
    "parameterized Viterbi rejects p=1"
    (fun () -> parameterized_packed_viterbi [| 1.0 |] obs);

  expect_invalid
    "Embedded push checks runtime vector length"
    (fun () ->
       let short = Array.make (state_count - 1) 0.0 in
       let next = Array.make state_count 0.0 in
       compiled_embedded_push short next);

  expect_invalid
    "Packed push checks runtime vector length"
    (fun () ->
       let short = Array.make (state_count - 1) 0.0 in
       let next = Array.make state_count 0.0 in
       compiled_packed_push short next);

  finish ()
