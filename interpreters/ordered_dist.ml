(* Independent ordered-table interpretation, used to cross-check the
   finite-index interpretation.  It stores canonical host values, while
   Finite_domain performs the shared checked admission from host values to
   finite indices. *)

module Make (V : Parameter.VALUES) : Direct_evaluator.PROBABILITY_EVALUATOR = struct
  let () = Parameter.check_values V.values

  type 'a typ = 'a Finite_domain.t
  type weight = float
  type 'a dist = { typ : 'a typ; table : ('a * float) array }
  type ('a, 'b) cdist = { src : 'a typ; dst : 'b typ; rows : 'b dist array }

  let make_typ = Finite_domain.make
  let tint_range ~name n = Finite_domain.int_range ~name n
  let tpair left right = Finite_domain.product left right
  let tiso = Finite_domain.iso

  let const x =
    Direct_evaluator.check_float_weight ~context:"Ordered_dist.const" x;
    x

  let parameter p = Parameter.value V.values p
  let complement p = 1.0 -. Parameter.value V.values p

  let rank = Finite_domain.encode

  let merge typ entries =
    let indexed =
      Array.of_list
        (List.map
           (fun (value, probability) ->
              rank typ value, probability)
           entries)
    in
    Array.stable_sort
      (fun (left, _) (right, _) -> Int.compare left right)
      indexed;
    let merged = ref [] in
    Array.iter
      (fun (index, probability) ->
         match !merged with
         | (previous, total) :: rest when index = previous ->
             merged := (previous, total +. probability) :: rest
         | _ -> merged := (index, probability) :: !merged)
      indexed;
    { typ;
      table =
        Array.of_list
          (List.rev
             (List.filter_map
                (fun (index, probability) ->
                   if probability > 0.0 then
                     Some (Finite_domain.decode typ index, probability)
                   else
                     None)
                !merged)) }

  let eta typ value =
    let index = rank typ value in
    { typ; table = [| Finite_domain.decode typ index, 1.0 |] }

  let dist typ entries =
    List.iter
      (fun (_, weight) ->
         Direct_evaluator.check_float_weight
           ~context:"Ordered_dist.dist" weight)
      entries;
    Direct_evaluator.check_mass
      ~context:("Ordered_dist.dist " ^ Finite_domain.name typ)
      (List.fold_left (fun total (_, weight) -> total +. weight) 0.0 entries);
    merge typ entries

  let map typ f distribution =
    merge typ
      (Array.to_list
         (Array.map
            (fun (value, probability) -> f value, probability)
            distribution.table))

  let map2 typ f left right =
    merge typ
      (List.concat_map
         (fun (left_value, left_probability) ->
            Array.to_list
              (Array.map
                 (fun (right_value, right_probability) ->
                    ( f left_value right_value,
                      left_probability *. right_probability ))
                 right.table))
         (Array.to_list left.table))

  let cdist src dst f =
    { src;
      dst;
      rows =
        Array.init (Finite_domain.cardinal src) (fun index ->
            let distribution = f (Finite_domain.decode src index) in
            Finite_domain.require_same
              ~context:"Ordered_dist.cdist" dst distribution.typ;
            distribution) }

  let row kernel value =
    kernel.rows.(rank kernel.src value)

  let capp distribution kernel =
    Finite_domain.require_same
      ~context:"Ordered_dist.capp" distribution.typ kernel.src;
    merge kernel.dst
      (List.concat_map
         (fun (value, probability) ->
            Array.to_list
              (Array.map
                 (fun (target, conditional_probability) ->
                    target, probability *. conditional_probability)
                 (row kernel value).table))
         (Array.to_list distribution.table))

  let lookup_index distribution index =
    let result = ref 0.0 in
    Array.iter
      (fun (value, probability) ->
         if rank distribution.typ value = index then result := probability)
      distribution.table;
    !result

  let score_observation kernel observation distribution =
    Finite_domain.require_same
      ~context:"Ordered_dist.score_observation"
      distribution.typ kernel.src;
    let observation_index = rank kernel.dst observation in
    { distribution with
      table =
        Array.of_list
          (List.filter_map
             (fun (value, probability) ->
                let likelihood =
                  lookup_index (row kernel value) observation_index
                in
                if probability *. likelihood > 0.0 then
                  Some (value, probability *. likelihood)
                else
                  None)
             (Array.to_list distribution.table)) }

  let normalize_with_mass distribution =
    let mass =
      Array.fold_left
        (fun total (_, probability) -> total +. probability)
        0.0 distribution.table
    in
    if mass = 0.0 then
      ({ distribution with table = [||] }, 0.0)
    else
      ( { distribution with
          table =
            Array.map
              (fun (value, probability) -> value, probability /. mass)
              distribution.table },
        mass )

  let cardinal = Finite_domain.cardinal
  let index = rank
  let value_of_index = Finite_domain.decode
  let support distribution =
    Array.to_list (Array.map fst distribution.table)
  let to_table distribution = Array.to_list distribution.table
end

include Make (Parameter.No_values)
