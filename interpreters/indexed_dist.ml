(* Finite-index interpretation. Distributions are sparse tables ordered by
   integer index. In this interpretation cdist evaluates its row function once
   for every represented source value and stores the resulting rows. *)

module Make (V : Parameter.VALUES) : Direct_evaluator.PROBABILITY_EVALUATOR = struct
  let () = Parameter.check_values V.values

  type 'a typ = 'a Finite_domain.t
  type weight = float
  type 'a dist = { typ : 'a typ; entries : (int * float) array }        (* sorted, merged, > 0 *)
  type ('a, 'b) cdist = { src : 'a typ; dst : 'b typ; rows : (int * float) array array }

  let make_typ = Finite_domain.make
  let tint_range ~name n = Finite_domain.int_range ~name n
  let tpair a b = Finite_domain.product a b
  let tiso = Finite_domain.iso

  let const x = Direct_evaluator.check_float_weight ~context:"Indexed_dist.const" x; x
  let parameter p = Parameter.value V.values p
  let complement p = 1.0 -. Parameter.value V.values p

  let canonical typ (entries : (int * float) list) =
    let a = Array.of_list entries in
    Array.stable_sort (fun (i, _) (j, _) -> Int.compare i j) a;
    let out = ref [] in
    Array.iter (fun (i, p) ->
        match !out with
        | (j, q) :: rest when i = j -> out := (j, q +. p) :: rest
        | _ -> out := (i, p) :: !out) a;
    { typ; entries = Array.of_list (List.rev (List.filter (fun (_, p) -> p > 0.0) !out)) }

  let eta typ x = { typ; entries = [| (Finite_domain.encode typ x, 1.0) |] }

  let dist typ entries =
    List.iter (fun (_, w) -> Direct_evaluator.check_float_weight ~context:"Indexed_dist.dist" w) entries;
    Direct_evaluator.check_mass
      ~context:("Indexed_dist.dist " ^ Finite_domain.name typ)
      (List.fold_left (fun s (_, w) -> s +. w) 0.0 entries);
    canonical typ (List.map (fun (x, w) -> (Finite_domain.encode typ x, w)) entries)

  let map typ f d =
    canonical typ (Array.to_list (Array.map (fun (i, p) ->
        (Finite_domain.encode typ (f (Finite_domain.decode d.typ i)), p)) d.entries))

  let map2 typ f d1 d2 =
    canonical typ (List.concat_map (fun (i, p) ->
        let x = Finite_domain.decode d1.typ i in
        Array.to_list (Array.map (fun (j, q) ->
            (Finite_domain.encode typ (f x (Finite_domain.decode d2.typ j)), p *. q)) d2.entries))
        (Array.to_list d1.entries))

  let cdist (src : 'a typ) (dst : 'b typ) f =
    { src; dst;
      rows = Array.init (Finite_domain.cardinal src) (fun i ->
          let d = f (Finite_domain.decode src i) in
          Finite_domain.require_same ~context:"Indexed_dist.cdist" dst d.typ;
          d.entries) }

  let row k x = { typ = k.dst; entries = k.rows.(Finite_domain.encode k.src x) }

  let capp d k =
    Finite_domain.require_same ~context:"Indexed_dist.capp" d.typ k.src;
    canonical k.dst (List.concat_map (fun (i, p) ->
        Array.to_list (Array.map (fun (j, q) -> (j, p *. q)) k.rows.(i))) (Array.to_list d.entries))

  let lookup (row : (int * float) array) j =
    let rec search lo hi =
      if lo >= hi then 0.0
      else
        let mid = (lo + hi) / 2 in
        let i, p = row.(mid) in
        if i = j then p else if j < i then search lo mid else search (mid + 1) hi
    in
    search 0 (Array.length row)

  let score_observation k y d =
    Finite_domain.require_same ~context:"Indexed_dist.score_observation" d.typ k.src;
    let jy = Finite_domain.encode k.dst y in
    { d with entries = Array.of_list (List.filter_map (fun (i, p) ->
          let q = lookup k.rows.(i) jy in
          if p *. q > 0.0 then Some (i, p *. q) else None) (Array.to_list d.entries)) }

  let normalize_with_mass d =
    let mass = Array.fold_left (fun s (_, p) -> s +. p) 0.0 d.entries in
    if mass = 0.0 then ({ d with entries = [||] }, 0.0)
    else ({ d with entries = Array.map (fun (i, p) -> (i, p /. mass)) d.entries }, mass)

  let cardinal = Finite_domain.cardinal
  let index t x = Finite_domain.encode t x
  let value_of_index t i = Finite_domain.decode t i
  let support d = Array.to_list (Array.map (fun (i, _) -> Finite_domain.decode d.typ i) d.entries)
  let to_table d = Array.to_list (Array.map (fun (i, p) -> (Finite_domain.decode d.typ i, p)) d.entries)
end

include Make (Parameter.No_values)
