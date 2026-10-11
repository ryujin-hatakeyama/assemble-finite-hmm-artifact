open Grid_sensor

type query = Forward | Viterbi
type weights_mode = Constant | Parameterized
type implementation = Handwritten_packed | Kernel_reference | Staged_packed | Staged_embedded
type mode = Full | Compile_only

type selected_model = {
  name : string;
  initial : float array;
  transition : (state, state) Model_kernel.t;
  transition_parameterized : unit -> (state, state) Model_kernel.t;
  emission : (state, obs) Model_kernel.t;
}

type prepared = {
  lowering_prep_sec : float;
  code_construction_sec : float;
  stage_run_sec : float;
  residual_source_bytes : int;
  distinct_weights : int;
  run : unit -> float;
}

let now () = Unix.gettimeofday ()

let timed f =
  let start = now () in
  let value = f () in
  let stop = now () in
  value, stop -. start

let parse_query = function
  | "forward" -> Forward
  | "viterbi" -> Viterbi
  | value -> invalid_arg ("query must be forward or viterbi, got " ^ value)

let parse_weights = function
  | "constant" -> Constant
  | "parameterized" -> Parameterized
  | value -> invalid_arg ("weights must be constant or parameterized, got " ^ value)

let parse_impl = function
  | "handwritten" -> Handwritten_packed
  | "kernel" -> Kernel_reference
  | "packed" -> Staged_packed
  | "embedded" -> Staged_embedded
  | value -> invalid_arg ("impl must be handwritten, kernel, packed, or embedded, got " ^ value)

let parse_mode = function
  | "full" -> Full
  | "compile-only" -> Compile_only
  | value -> invalid_arg ("mode must be full or compile-only, got " ^ value)

let string_of_query = function Forward -> "forward" | Viterbi -> "viterbi"
let string_of_weights = function Constant -> "constant" | Parameterized -> "parameterized"
let string_of_impl = function
  | Handwritten_packed -> "handwritten_packed"
  | Kernel_reference -> "kernel_reference"
  | Staged_packed -> "staged_packed"
  | Staged_embedded -> "staged_embedded"

let lowering_of_impl = function
  | Staged_packed -> Kernel_staged.Packed
  | Staged_embedded -> Kernel_staged.Embedded
  | (Handwritten_packed | Kernel_reference) ->
      invalid_arg "non-staged implementation has no staged lowering"

let select_model = function
  | "original" ->
      let module Original_kernel = Original (Kernel_interpreter) in
      {
        name = "original";
        initial = Kernel_interpreter.initial_vector [||] Original_kernel.initial;
        transition = Original_kernel.transition;
        transition_parameterized = Original_kernel.transition_parameterized;
        emission = Original_kernel.emission;
      }
  | "variant" ->
      let module Variant_kernel = Variant (Kernel_interpreter) in
      {
        name = "variant";
        initial = Kernel_interpreter.initial_vector [||] Variant_kernel.initial;
        transition = Variant_kernel.transition;
        transition_parameterized = Variant_kernel.transition_parameterized;
        emission = Variant_kernel.emission;
      }
  | value -> invalid_arg ("model must be original or variant, got " ^ value)

let observations length =
  if length <= 0 then invalid_arg "t_len must be positive";
  Array.init length (fun time ->
      if time = 0 then { y1 = 1; y2 = 1 }
      else { y1 = 21; y2 = 21 })

let edge_count (kernel : ('a, 'b) Model_kernel.t) =
  Array.fold_left (fun total row -> total + Array.length row) 0 kernel.Model_kernel.rows

let residual_source_bytes (code : 'a Trx.code) =
  String.length (Format.asprintf "%a" Codelib.print_code code)

let pack_constant lowering numeric_transition =
  match lowering with
  | Kernel_staged.Packed -> Kernel_staged.pack_rows numeric_transition
  | Kernel_staged.Embedded ->
      { Kernel_staged.offsets = [||]; targets = [||]; probabilities = [||] }

let pack_symbolic lowering symbolic_transition indexed =
  match lowering with
  | Kernel_staged.Packed -> Kernel_staged.pack_symbolic_rows symbolic_transition indexed
  | Kernel_staged.Embedded ->
      {
        Kernel_staged.symbolic_offsets = [||];
        symbolic_targets = [||];
        symbolic_weight_ids = [||];
      }

let prepare_handwritten ~query ~weights ~parameter ~symbolic_transition model observations =
  let parameters = match weights with Constant -> [||] | Parameterized -> [| parameter |] in
  Parameter.check_values parameters;
  let observation_indices =
    Array.map (Finite_domain.encode model.emission.Model_kernel.dst) observations
  in
  let observation_count = model.emission.Model_kernel.dst.Finite_domain.cardinal in
  match weights with
  | Constant ->
      let prep, lowering_prep_sec =
        timed (fun () ->
            let numeric_transition =
              Kernel_staged.require_constant_transition symbolic_transition
            in
            let packed_transition = Kernel_staged.pack_rows numeric_transition in
            let numeric_emission =
              Kernel_staged.require_constant_emission
                ~transition:symbolic_transition model.emission
            in
            let packed_emission = Kernel_staged.pack_columns numeric_emission in
            packed_transition, packed_emission)
      in
      let packed_transition, packed_emission = prep in
      let run =
        match query with
        | Forward ->
            (fun () ->
               Handwritten_packed_inference.forward_constant
                 ~initial:model.initial
                 ~transition:packed_transition
                 ~emission:packed_emission
                 ~observation_count
                 observation_indices)
        | Viterbi ->
            (fun () ->
               Handwritten_packed_inference.viterbi_constant
                 ~initial:model.initial
                 ~transition:packed_transition
                 ~emission:packed_emission
                 ~observation_count
                 observation_indices)
      in
      {
        lowering_prep_sec;
        code_construction_sec = 0.0;
        stage_run_sec = 0.0;
        residual_source_bytes = 0;
        distinct_weights = 0;
        run;
      }
  | Parameterized ->
      let prep, lowering_prep_sec =
        timed (fun () ->
            let indexed = Kernel_staged.index_symbolic_rows symbolic_transition in
            let packed_transition =
              Kernel_staged.pack_symbolic_rows symbolic_transition indexed
            in
            let numeric_emission =
              Kernel_staged.require_constant_emission
                ~transition:symbolic_transition model.emission
            in
            let packed_emission = Kernel_staged.pack_columns numeric_emission in
            indexed, packed_transition, packed_emission)
      in
      let indexed, packed_transition, packed_emission = prep in
      let weight_table = indexed.Kernel_staged.weight_table in
      let run =
        match query with
        | Forward ->
            (fun () ->
               Handwritten_packed_inference.forward_parameterized
                 ~parameters
                 ~weight_table
                 ~initial:model.initial
                 ~transition:packed_transition
                 ~emission:packed_emission
                 ~observation_count
                 observation_indices)
        | Viterbi ->
            (fun () ->
               Handwritten_packed_inference.viterbi_parameterized
                 ~parameters
                 ~weight_table
                 ~initial:model.initial
                 ~transition:packed_transition
                 ~emission:packed_emission
                 ~observation_count
                 observation_indices)
      in
      {
        lowering_prep_sec;
        code_construction_sec = 0.0;
        stage_run_sec = 0.0;
        residual_source_bytes = 0;
        distinct_weights = Array.length weight_table;
        run;
      }

let prepare_kernel_reference ~query ~weights ~parameter ~transition model observations =
  let parameters = match weights with Constant -> [||] | Parameterized -> [| parameter |] in
  let (numeric_transition, numeric_emission), lowering_prep_sec =
    timed (fun () ->
        Parameter.check_values parameters;
        (Model_kernel.instantiate transition parameters,
         Model_kernel.instantiate model.emission [||]))
  in
  let run =
    (* Ordinary inference constructs emission columns inside each timed call.
       Handwritten and staged runs receive columns prepared before timing, so
       their runtime ratios against this reference are end-to-end comparisons. *)
    match query with
    | Forward ->
        (fun () ->
           let _posterior, score =
             Kernel_inference_prob.Forward.run
               ~initial:model.initial
               ~transition:numeric_transition
               ~emission:numeric_emission
               observations
           in
           score)
    | Viterbi ->
        (fun () ->
           let _path, score =
             Kernel_inference_prob.Viterbi.run
               ~initial:model.initial
               ~transition:numeric_transition
               ~emission:numeric_emission
               observations
           in
           score)
  in
  {
    lowering_prep_sec;
    code_construction_sec = 0.0;
    stage_run_sec = 0.0;
    residual_source_bytes = 0;
    distinct_weights = 0;
    run;
  }

let prepare_staged_constant ~query ~lowering ~measure_code_size ~transition model observations =
  let observation_indices =
    Array.map (Finite_domain.encode model.emission.Model_kernel.dst) observations
  in
  let prep, lowering_prep_sec =
    timed (fun () ->
        let numeric_transition = Kernel_staged.require_constant_transition transition in
        let packed_transition = pack_constant lowering numeric_transition in
        let numeric_emission =
          Kernel_staged.require_constant_emission
            ~transition model.emission
        in
        let packed_emission = Kernel_staged.pack_columns numeric_emission in
        numeric_transition, packed_transition, packed_emission)
  in
  let numeric_transition, packed_transition, packed_emission = prep in
  let observation_count = model.emission.Model_kernel.dst.Finite_domain.cardinal in
  match query with
  | Forward ->
      let code, code_construction_sec =
        timed (fun () ->
            Kernel_staged.compile_forward_indices_code
              ~lowering ~observation_count numeric_transition)
      in
      let residual_source_bytes =
        if measure_code_size then residual_source_bytes code else 0
      in
      Gc.full_major ();
      let core, stage_run_sec = timed (fun () -> Runcode.run code) in
      let run () =
        let _posterior, score =
          core
            model.initial
            packed_transition.Kernel_staged.offsets
            packed_transition.Kernel_staged.targets
            packed_transition.Kernel_staged.probabilities
            packed_emission.Kernel_staged.column_offsets
            packed_emission.Kernel_staged.states
            packed_emission.Kernel_staged.emission_probabilities
            observation_indices
        in
        score
      in
      {
        lowering_prep_sec;
        code_construction_sec;
        stage_run_sec;
        residual_source_bytes;
        distinct_weights = 0;
        run;
      }
  | Viterbi ->
      let code, code_construction_sec =
        timed (fun () ->
            Kernel_staged.compile_viterbi_indices_code
              ~lowering ~observation_count numeric_transition)
      in
      let residual_source_bytes =
        if measure_code_size then residual_source_bytes code else 0
      in
      Gc.full_major ();
      let core, stage_run_sec = timed (fun () -> Runcode.run code) in
      let run () =
        let _path, score =
          core
            model.initial
            packed_transition.Kernel_staged.offsets
            packed_transition.Kernel_staged.targets
            packed_transition.Kernel_staged.probabilities
            packed_emission.Kernel_staged.column_offsets
            packed_emission.Kernel_staged.states
            packed_emission.Kernel_staged.emission_probabilities
            observation_indices
        in
        score
      in
      {
        lowering_prep_sec;
        code_construction_sec;
        stage_run_sec;
        residual_source_bytes;
        distinct_weights = 0;
        run;
      }

let prepare_staged_parameterized
    ~query ~lowering ~measure_code_size ~parameter ~symbolic_transition model observations =
  let parameters = [| parameter |] in
  Parameter.check_values parameters;
  let observation_indices =
    Array.map (Finite_domain.encode model.emission.Model_kernel.dst) observations
  in
  let prep, lowering_prep_sec =
    timed (fun () ->
        let indexed = Kernel_staged.index_symbolic_rows symbolic_transition in
        let packed_transition = pack_symbolic lowering symbolic_transition indexed in
        let numeric_emission =
          Kernel_staged.require_constant_emission
            ~transition:symbolic_transition model.emission
        in
        let packed_emission = Kernel_staged.pack_columns numeric_emission in
        indexed, packed_transition, packed_emission)
  in
  let indexed, packed_transition, packed_emission = prep in
  let distinct_weights = Array.length indexed.Kernel_staged.weight_table in
  let observation_count = model.emission.Model_kernel.dst.Finite_domain.cardinal in
  match query with
  | Forward ->
      let code, code_construction_sec =
        timed (fun () ->
            Kernel_staged.compile_forward_parameterized_indices_code
              ~lowering ~observation_count ~indexed symbolic_transition)
      in
      let residual_source_bytes =
        if measure_code_size then residual_source_bytes code else 0
      in
      Gc.full_major ();
      let core, stage_run_sec = timed (fun () -> Runcode.run code) in
      let run () =
        let _posterior, score =
          core
            parameters
            model.initial
            packed_transition.Kernel_staged.symbolic_offsets
            packed_transition.Kernel_staged.symbolic_targets
            packed_transition.Kernel_staged.symbolic_weight_ids
            packed_emission.Kernel_staged.column_offsets
            packed_emission.Kernel_staged.states
            packed_emission.Kernel_staged.emission_probabilities
            observation_indices
        in
        score
      in
      {
        lowering_prep_sec;
        code_construction_sec;
        stage_run_sec;
        residual_source_bytes;
        distinct_weights;
        run;
      }
  | Viterbi ->
      let code, code_construction_sec =
        timed (fun () ->
            Kernel_staged.compile_viterbi_parameterized_indices_code
              ~lowering ~observation_count ~indexed symbolic_transition)
      in
      let residual_source_bytes =
        if measure_code_size then residual_source_bytes code else 0
      in
      Gc.full_major ();
      let core, stage_run_sec = timed (fun () -> Runcode.run code) in
      let run () =
        let _path, score =
          core
            parameters
            model.initial
            packed_transition.Kernel_staged.symbolic_offsets
            packed_transition.Kernel_staged.symbolic_targets
            packed_transition.Kernel_staged.symbolic_weight_ids
            packed_emission.Kernel_staged.column_offsets
            packed_emission.Kernel_staged.states
            packed_emission.Kernel_staged.emission_probabilities
            observation_indices
        in
        score
      in
      {
        lowering_prep_sec;
        code_construction_sec;
        stage_run_sec;
        residual_source_bytes;
        distinct_weights;
        run;
      }

let prepare ~query ~weights ~implementation ~parameter ~mode ~symbolic_transition model observations =
  match implementation with
  | Handwritten_packed ->
      prepare_handwritten
        ~query ~weights ~parameter ~symbolic_transition model observations
  | Kernel_reference ->
      prepare_kernel_reference ~query ~weights ~parameter ~transition:symbolic_transition model observations
  | (Staged_packed | Staged_embedded) as implementation ->
      let lowering = lowering_of_impl implementation in
      let measure_code_size = mode = Full in
      begin
        match weights with
        | Constant ->
            prepare_staged_constant
              ~query ~lowering ~measure_code_size ~transition:symbolic_transition model observations
        | Parameterized ->
            prepare_staged_parameterized
              ~query ~lowering ~measure_code_size ~parameter ~symbolic_transition model observations
      end

let print_header () =
  print_endline
    "model,query,weights,impl,parameter,t_len,states,edges,distinct_weights,\
     model_build_sec,transition_build_sec,lowering_prep_sec,code_construction_sec,\
     stage_run_sec,residual_source_bytes,warmup,repetitions,repetition,run_time_sec,result_score"

let print_row
    ~model ~query ~weights ~implementation ~parameter ~t_len ~states ~edges
    ~model_build_sec ~transition_build_sec
    ~warmup ~repetitions ~repetition ~run_time_sec ~result_score prepared =
  Printf.printf
    "%s,%s,%s,%s,%.17g,%d,%d,%d,%d,%.17g,%.17g,%.17g,%.17g,%.17g,%d,%d,%d,%d,%.17g,%.17g\n%!"
    model.name
    (string_of_query query)
    (string_of_weights weights)
    (string_of_impl implementation)
    parameter
    t_len
    states
    edges
    prepared.distinct_weights
    model_build_sec
    transition_build_sec
    prepared.lowering_prep_sec
    prepared.code_construction_sec
    prepared.stage_run_sec
    prepared.residual_source_bytes
    warmup
    repetitions
    repetition
    run_time_sec
    result_score

let () =
  if Array.length Sys.argv <> 10 then begin
    prerr_endline
      "usage: bench_kernel_evaluation.byte MODEL QUERY WEIGHTS IMPL T_LEN REPETITIONS WARMUP PARAMETER MODE";
    prerr_endline
      "  MODEL=original|variant QUERY=forward|viterbi WEIGHTS=constant|parameterized";
    prerr_endline
      "  IMPL=handwritten|kernel|packed|embedded MODE=full|compile-only";
    exit 2
  end;
  let model_name = Sys.argv.(1) in
  let model, model_build_sec = timed (fun () -> select_model model_name) in
  let query = parse_query Sys.argv.(2) in
  let weights = parse_weights Sys.argv.(3) in
  let implementation = parse_impl Sys.argv.(4) in
  let t_len = int_of_string Sys.argv.(5) in
  let repetitions = int_of_string Sys.argv.(6) in
  let warmup = int_of_string Sys.argv.(7) in
  let parameter = float_of_string Sys.argv.(8) in
  let output_parameter = if weights = Parameterized then parameter else nan in
  let mode = parse_mode Sys.argv.(9) in
  if repetitions <= 0 then invalid_arg "repetitions must be positive";
  if warmup < 0 then invalid_arg "warmup must be nonnegative";
  let observations = observations t_len in
  let symbolic_transition, transition_build_sec =
    timed (fun () ->
        match weights with
        | Constant -> model.transition
        | Parameterized -> model.transition_parameterized ())
  in
  let states = symbolic_transition.Model_kernel.src.Finite_domain.cardinal in
  let edges = edge_count symbolic_transition in
  let prepared =
    prepare
      ~query ~weights ~implementation ~parameter ~mode ~symbolic_transition model observations
  in
  print_header ();
  match mode with
  | Compile_only ->
      print_row
        ~model ~query ~weights ~implementation ~parameter:output_parameter ~t_len ~states ~edges
        ~model_build_sec ~transition_build_sec
        ~warmup:0 ~repetitions:0 ~repetition:0 ~run_time_sec:nan
        ~result_score:nan prepared
  | Full ->
      for _ = 1 to warmup do ignore (prepared.run ()) done;
      for repetition = 1 to repetitions do
        Gc.full_major ();
        let result_score, run_time_sec = timed prepared.run in
        print_row
          ~model ~query ~weights ~implementation ~parameter:output_parameter ~t_len ~states ~edges
          ~model_build_sec ~transition_build_sec
          ~warmup ~repetitions ~repetition ~run_time_sec ~result_score prepared
      done
