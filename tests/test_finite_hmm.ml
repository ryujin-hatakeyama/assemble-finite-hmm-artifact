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

let max_abs_diff left right =
  if Array.length left <> Array.length right then infinity
  else
    Array.fold_left
      Float.max
      0.0
      (Array.mapi
         (fun index value -> Float.abs (value -. right.(index)))
         left)

let bad_encode = function
  | 0 -> 0
  | 1 -> 1
  | 999 -> 0
  | value ->
      invalid_arg (Printf.sprintf "bad finite-domain value %d" value)

let bad_decode = function
  | 0 -> 0
  | 1 -> 1
  | index ->
      invalid_arg (Printf.sprintf "bad finite-domain index %d" index)

let make_bad_indexed name =
  Indexed_dist.make_typ
    ~name ~cardinal:2 ~equal:Int.equal
    ~encode:bad_encode ~decode:bad_decode

let make_bad_ordered name =
  Ordered_dist.make_typ
    ~name ~cardinal:2 ~equal:Int.equal
    ~encode:bad_encode ~decode:bad_decode

let run_permuted_binary_model ~reversed =
  let encode value =
    if value < 0 || value >= 2 then
      invalid_arg "binary model value outside carrier";
    if reversed then 1 - value else value
  and decode index =
    if reversed then 1 - index else index
  in
  let state =
    Finite_domain.make
      ~name:(if reversed then "binary-state-reversed" else "binary-state")
      ~cardinal:2 ~equal:Int.equal ~encode ~decode
  and observation =
    Finite_domain.make
      ~name:(if reversed then "binary-observation-reversed" else "binary-observation")
      ~cardinal:2 ~equal:Int.equal ~encode ~decode
  in
  let initial =
    Kernel_interpreter.dist state
      [ 0, Kernel_interpreter.const 0.65;
        1, Kernel_interpreter.const 0.35 ]
  and transition =
    Kernel_interpreter.cdist state state (function
      | 0 ->
          Kernel_interpreter.dist state
            [ 0, Kernel_interpreter.const 0.8;
              1, Kernel_interpreter.const 0.2 ]
      | 1 ->
          Kernel_interpreter.dist state
            [ 0, Kernel_interpreter.const 0.3;
              1, Kernel_interpreter.const 0.7 ]
      | _ -> assert false)
  and emission =
    Kernel_interpreter.cdist state observation (function
      | 0 ->
          Kernel_interpreter.dist observation
            [ 0, Kernel_interpreter.const 0.9;
              1, Kernel_interpreter.const 0.1 ]
      | 1 ->
          Kernel_interpreter.dist observation
            [ 0, Kernel_interpreter.const 0.2;
              1, Kernel_interpreter.const 0.8 ]
      | _ -> assert false)
  in
  let initial = Kernel_interpreter.initial_vector [||] initial
  and transition = Model_kernel.instantiate transition [||]
  and emission = Model_kernel.instantiate emission [||]
  and observations = [| 0; 1; 1; 0 |] in
  let posterior, forward_score =
    Kernel_inference_prob.Forward.run
      ~initial ~transition ~emission observations
  and path, viterbi_score =
    Kernel_inference_prob.Viterbi.run
      ~initial ~transition ~emission observations
  in
  ( Array.init 2 (fun value ->
        posterior.(Finite_domain.encode state value)),
    forward_score,
    Array.map (Finite_domain.decode state) path,
    viterbi_score )

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
  let finite_bad =
    Finite_domain.make
      ~name:"finite-bad-alias" ~cardinal:2 ~equal:Int.equal
      ~encode:bad_encode ~decode:bad_decode
  in
  expect_invalid "finite domain rejects an out-of-carrier alias"
    (fun () -> Finite_domain.encode finite_bad 999);
  let bad_indexed = make_bad_indexed "indexed-bad-alias" in
  let bad_ordered = make_bad_ordered "ordered-bad-alias" in
  expect_invalid "Indexed eta rejects an out-of-carrier value"
    (fun () -> Indexed_dist.eta bad_indexed 999);
  expect_invalid "Ordered eta rejects an out-of-carrier value"
    (fun () -> Ordered_dist.eta bad_ordered 999);
  expect_invalid "dist rejects an out-of-carrier support value"
    (fun () -> Indexed_dist.dist bad_indexed [ 999, 1.0 ]);
  expect_invalid "map rejects an out-of-carrier result"
    (fun () ->
       Indexed_dist.map bad_indexed (fun _ -> 999)
         (Indexed_dist.eta bad_indexed 0));
  expect_invalid "map2 rejects an out-of-carrier result"
    (fun () ->
       Indexed_dist.map2 bad_indexed (fun _ _ -> 999)
         (Indexed_dist.eta bad_indexed 0)
         (Indexed_dist.eta bad_indexed 1));
  expect_invalid "cdist rejects an out-of-carrier row result"
    (fun () ->
       Indexed_dist.cdist bad_indexed bad_indexed (fun source ->
           Indexed_dist.eta bad_indexed
             (if Int.equal source 1 then 999 else source)));
  let bad_identity =
    Indexed_dist.cdist bad_indexed bad_indexed
      (fun value -> Indexed_dist.eta bad_indexed value)
  in
  expect_invalid "conditional row lookup rejects an out-of-carrier source"
    (fun () -> Indexed_dist.row bad_identity 999);
  expect_invalid "direct observation scoring rejects an out-of-carrier value"
    (fun () ->
       Indexed_dist.score_observation bad_identity 999
         (Indexed_dist.eta bad_indexed 0));
  let kernel_bad =
    Kernel_interpreter.make_typ
      ~name:"kernel-bad-alias" ~cardinal:2 ~equal:Int.equal
      ~encode:bad_encode ~decode:bad_decode
  in
  let kernel_initial = Kernel_interpreter.eta kernel_bad 0
  and kernel_identity =
    Kernel_interpreter.cdist kernel_bad kernel_bad
      (fun value -> Kernel_interpreter.eta kernel_bad value)
  in
  let kernel_initial =
    Kernel_interpreter.initial_vector [||] kernel_initial
  and kernel_identity =
    Model_kernel.instantiate kernel_identity [||]
  in
  expect_invalid "kernel inference rejects an out-of-carrier observation"
    (fun () ->
       Kernel_inference_prob.Forward.run
         ~initial:kernel_initial
         ~transition:kernel_identity
         ~emission:kernel_identity
         [| 999 |]);
  let alias_iso =
    Finite_domain.iso
      ~name:"alias-iso" ~equal:Int.equal
      ~rep:(Finite_domain.int_range ~name:"alias-representation" 2)
      ~to_rep:bad_encode ~of_rep:Fun.id
  in
  expect_invalid "isomorphism rejects a value aliased by to_rep"
    (fun () -> Finite_domain.encode alias_iso 999);
  let pair =
    Finite_domain.product
      (Finite_domain.int_range ~name:"pair-left" 2)
      (Finite_domain.int_range ~name:"pair-right" 2)
  in
  expect_invalid "product admission rejects an invalid component"
    (fun () -> Finite_domain.encode pair (999, 0));
  expect_invalid "negative finite-domain cardinality is rejected"
    (fun () ->
       Finite_domain.make
         ~name:"negative" ~cardinal:(-1) ~equal:Int.equal
         ~encode:Fun.id ~decode:Fun.id);
  let empty = Finite_domain.int_range ~name:"empty" 0 in
  report "empty finite domain has no representatives"
    (Finite_domain.cardinal empty = 0
     && Array.length (Finite_domain.elements empty) = 0)
    "cardinal=0";
  expect_invalid "empty finite domain rejects every value"
    (fun () -> Finite_domain.encode empty 0);
  expect_invalid "empty finite domain rejects every index"
    (fun () -> Finite_domain.decode empty 0);
  let changed = ref false in
  let mutable_encoder =
    Finite_domain.make
      ~name:"mutable-encoder" ~cardinal:2 ~equal:Int.equal
      ~encode:(fun value -> if !changed then 1 - value else value)
      ~decode:Fun.id
  in
  changed := true;
  expect_invalid "changed encoder cannot silently remap a representative"
    (fun () -> Finite_domain.encode mutable_encoder 0);
  expect_invalid "non-reflexive supplied equality is rejected on representatives"
    (fun () ->
       Finite_domain.make
         ~name:"non-reflexive" ~cardinal:1
         ~equal:(fun _ _ -> false) ~encode:Fun.id ~decode:Fun.id);
  let copied = Finite_domain.elements finite_bad in
  copied.(0) <- 999;
  report "representative array access is defensive"
    (Int.equal (Finite_domain.decode finite_bad 0) 0)
    "mutating elements copy leaves domain unchanged";
  let reversed =
    Finite_domain.make
      ~name:"reversed" ~cardinal:2 ~equal:Int.equal
      ~encode:(fun value ->
        if value < 0 || value >= 2 then invalid_arg "reversed value";
        1 - value)
      ~decode:(fun index -> 1 - index)
  in
  report "user-specified finite index enumeration is preserved"
    (Finite_domain.decode reversed 0 = 1
     && Finite_domain.decode reversed 1 = 0
     && Finite_domain.encode reversed 1 = 0
     && Finite_domain.encode reversed 0 = 1)
    "decode=[1;0]";
  let natural_posterior, natural_forward, natural_path, natural_viterbi =
    run_permuted_binary_model ~reversed:false
  and reversed_posterior, reversed_forward, reversed_path, reversed_viterbi =
    run_permuted_binary_model ~reversed:true
  in
  report "Forward is invariant under a finite-index permutation"
    (max_abs_diff natural_posterior reversed_posterior < 1e-12
     && Float.abs (natural_forward -. reversed_forward) < 1e-12)
    (Printf.sprintf "posterior=%.1e score=%.1e"
       (max_abs_diff natural_posterior reversed_posterior)
       (Float.abs (natural_forward -. reversed_forward)));
  report "Viterbi is invariant under a tie-free finite-index permutation"
    (equal_int_array natural_path reversed_path
     && Float.abs (natural_viterbi -. reversed_viterbi) < 1e-12)
    (Printf.sprintf "path=%b score=%.1e"
       (equal_int_array natural_path reversed_path)
       (Float.abs (natural_viterbi -. reversed_viterbi)));
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
