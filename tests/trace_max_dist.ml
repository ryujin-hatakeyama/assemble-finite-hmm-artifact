(* Experiment-only finite-index score interpretation.  It implements the
   model language with non-negative multiplicative weights, replacing the
   probability interpretation's addition of duplicate targets by maximum.
   Resulting conditional rows are scores and are not required to be
   stochastic. Because the weights are non-negative and multiplication
   distributes over maximum, each entry is the maximum over complete
   transition-local internal choice traces, not over HMM state paths. *)

module Make (V : Parameter.VALUES) = struct
  let () = Parameter.check_values V.values

  type 'a typ = 'a Finite_domain.t
  type weight = float
  type 'a dist = {
    typ : 'a typ;
    entries : (int * float) array;
  }
  type ('a, 'b) cdist = {
    src : 'a typ;
    dst : 'b typ;
    rows : (int * float) array array;
  }

  let make_typ = Finite_domain.make
  let tint_range ~name n = Finite_domain.int_range ~name n
  let tpair left right = Finite_domain.product left right
  let tiso = Finite_domain.iso

  let const weight =
    Direct_evaluator.check_float_weight
      ~context:"Trace_max_dist.const" weight;
    weight

  let parameter parameter = Parameter.value V.values parameter
  let complement parameter = 1.0 -. Parameter.value V.values parameter

  let canonical typ entries =
    let sorted = Array.of_list entries in
    Array.stable_sort
      (fun (left, _) (right, _) -> Int.compare left right)
      sorted;
    let merged = ref [] in
    Array.iter
      (fun (index, score) ->
         match !merged with
         | (previous, previous_score) :: rest when index = previous ->
             merged := (previous, Float.max previous_score score) :: rest
         | _ -> merged := (index, score) :: !merged)
      sorted;
    { typ;
      entries =
        Array.of_list
          (List.rev
             (List.filter (fun (_, score) -> score > 0.0) !merged)) }

  let eta typ value =
    { typ;
      entries = [| Finite_domain.encode typ value, 1.0 |] }

  let dist typ entries =
    List.iter
      (fun (_, weight) ->
         Direct_evaluator.check_float_weight
           ~context:"Trace_max_dist.dist" weight)
      entries;
    Direct_evaluator.check_mass
      ~context:("Trace_max_dist.dist " ^ typ.Finite_domain.name)
      (List.fold_left (fun mass (_, weight) -> mass +. weight) 0.0 entries);
    canonical typ
      (List.map
         (fun (value, weight) -> Finite_domain.encode typ value, weight)
         entries)

  let map typ f distribution =
    canonical typ
      (Array.to_list
         (Array.map
            (fun (index, score) ->
               ( Finite_domain.encode typ
                   (f (Finite_domain.decode distribution.typ index)),
                 score ))
            distribution.entries))

  let map2 typ f left right =
    canonical typ
      (List.concat_map
         (fun (left_index, left_score) ->
            let left_value = Finite_domain.decode left.typ left_index in
            Array.to_list
              (Array.map
                 (fun (right_index, right_score) ->
                    ( Finite_domain.encode typ
                        (f left_value
                           (Finite_domain.decode right.typ right_index)),
                      left_score *. right_score ))
                 right.entries))
         (Array.to_list left.entries))

  let cdist (src : 'a typ) (dst : 'b typ) f =
    { src;
      dst;
      rows =
        Array.init src.Finite_domain.cardinal (fun index ->
            let distribution = f (Finite_domain.decode src index) in
            Finite_domain.require_same
              ~context:"Trace_max_dist.cdist" dst distribution.typ;
            distribution.entries) }

  let capp distribution kernel =
    Finite_domain.require_same
      ~context:"Trace_max_dist.capp" distribution.typ kernel.src;
    canonical kernel.dst
      (List.concat_map
         (fun (source, source_score) ->
            Array.to_list
              (Array.map
                 (fun (target, conditional_score) ->
                    target, source_score *. conditional_score)
                 kernel.rows.(source)))
         (Array.to_list distribution.entries))

  let remap_dist ~domain distribution =
    let dense = Array.make domain.Finite_domain.cardinal 0.0 in
    Array.iter
      (fun (index, score) ->
         let value = Finite_domain.decode distribution.typ index in
         dense.(Finite_domain.encode domain value) <- score)
      distribution.entries;
    dense

  (* Map a derived score table into the ordinary kernel's domain identities.
     This does not call Model_kernel.instantiate or stochastic-row validation. *)
  let remap_numeric ~source ~destination kernel =
    let targets =
      Array.init source.Finite_domain.cardinal (fun source_index ->
          let source_value = Finite_domain.decode source source_index in
          let score_source = Finite_domain.encode kernel.src source_value in
          Array.map
            (fun (target_index, _) ->
               let target_value = Finite_domain.decode kernel.dst target_index in
               Finite_domain.encode destination target_value)
            kernel.rows.(score_source))
    and probabilities =
      Array.init source.Finite_domain.cardinal (fun source_index ->
          let source_value = Finite_domain.decode source source_index in
          let score_source = Finite_domain.encode kernel.src source_value in
          Array.map snd kernel.rows.(score_source))
    in
    { Model_kernel.source;
      destination;
      targets;
      probabilities }
end

include Make (Parameter.No_values)
