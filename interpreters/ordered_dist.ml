(* Independent ordered-table interpretation, used to cross-check the
   finite-index interpretation. Ordering is built structurally from integers,
   products and finite isomorphisms; make_typ uses the supplied encoding. *)

module Make (V : Parameter.VALUES) : Direct_evaluator.PROBABILITY_EVALUATOR = struct
  let () = Parameter.check_values V.values

  type 'a typ = {
    uid : int;
    name : string;
    compare : 'a -> 'a -> int;
    elements : 'a array;
  }

  type weight = float
  type 'a dist = { typ : 'a typ; table : ('a * float) array }
  type ('a, 'b) cdist = { src : 'a typ; dst : 'b typ; rows : 'b dist array }

  let next_uid = ref 0
  let fresh name compare elements =
    let uid = !next_uid in
    incr next_uid;
    { uid; name; compare; elements }

  let require_same ~context left right =
    if left.uid <> right.uid then
      invalid_arg (Printf.sprintf "%s: finite-domain mismatch (%s vs %s)" context left.name right.name)

  let make_typ ~name ~cardinal ~encode ~decode =
    if cardinal < 0 then invalid_arg "Ordered_dist.make_typ: negative cardinality";
    for i = 0 to cardinal - 1 do
      if encode (decode i) <> i then
        invalid_arg (Printf.sprintf "Ordered_dist.make_typ(%s): encode(decode(%d)) <> %d" name i i)
    done;
    fresh name (fun x y -> Int.compare (encode x) (encode y)) (Array.init cardinal decode)

  let tint_range ~name n = fresh name Int.compare (Array.init n (fun i -> i))

  let tpair a b =
    fresh (Printf.sprintf "(%s * %s)" a.name b.name)
      (fun (x1, y1) (x2, y2) -> let c = a.compare x1 x2 in if c <> 0 then c else b.compare y1 y2)
      (Array.concat (Array.to_list (Array.map (fun x -> Array.map (fun y -> (x, y)) b.elements) a.elements)))

  let tiso ~name ~rep ~to_rep ~of_rep =
    let elements = Array.map of_rep rep.elements in
    Array.iteri (fun i x -> if rep.compare (to_rep x) rep.elements.(i) <> 0 then
                    invalid_arg ("Ordered_dist.tiso: not an isomorphism: " ^ name)) elements;
    fresh name (fun x y -> rep.compare (to_rep x) (to_rep y)) elements

  let const x = Direct_evaluator.check_float_weight ~context:"Ordered_dist.const" x; x
  let parameter p = Parameter.value V.values p
  let complement p = 1.0 -. Parameter.value V.values p

  let rank t x =
    let rec search lo hi =
      if lo >= hi then invalid_arg ("Ordered_dist: value outside finite type " ^ t.name)
      else
        let mid = (lo + hi) / 2 in
        let c = t.compare x t.elements.(mid) in
        if c = 0 then mid else if c < 0 then search lo mid else search (mid + 1) hi
    in
    search 0 (Array.length t.elements)

  let merge t entries =
    List.iter (fun (x, _) -> ignore (rank t x)) entries;
    let a = Array.of_list entries in
    Array.stable_sort (fun (x, _) (y, _) -> t.compare x y) a;
    let out = ref [] in
    Array.iter (fun (x, p) ->
        match !out with
        | (y, q) :: rest when t.compare x y = 0 -> out := (y, q +. p) :: rest
        | _ -> out := (x, p) :: !out) a;
    { typ = t; table = Array.of_list (List.rev (List.filter (fun (_, p) -> p > 0.0) !out)) }

  let eta t x = ignore (rank t x); { typ = t; table = [| (x, 1.0) |] }

  let dist t entries =
    List.iter (fun (_, w) -> Direct_evaluator.check_float_weight ~context:"Ordered_dist.dist" w) entries;
    Direct_evaluator.check_mass ~context:("Ordered_dist.dist " ^ t.name)
      (List.fold_left (fun s (_, w) -> s +. w) 0.0 entries);
    merge t entries

  let map t f d = merge t (Array.to_list (Array.map (fun (x, p) -> (f x, p)) d.table))

  let map2 t f d1 d2 =
    merge t (List.concat_map (fun (x, p) ->
        Array.to_list (Array.map (fun (y, q) -> (f x y, p *. q)) d2.table)) (Array.to_list d1.table))

  let cdist src dst f =
    { src; dst;
      rows = Array.map (fun x ->
          let d = f x in require_same ~context:"Ordered_dist.cdist" dst d.typ; d) src.elements }

  let row k x = k.rows.(rank k.src x)

  let capp d k =
    require_same ~context:"Ordered_dist.capp" d.typ k.src;
    merge k.dst (List.concat_map (fun (x, p) ->
        Array.to_list (Array.map (fun (y, q) -> (y, p *. q)) (row k x).table)) (Array.to_list d.table))

  let lookup d y =
    let result = ref 0.0 in
    Array.iter (fun (z, p) -> if d.typ.compare y z = 0 then result := p) d.table;
    !result

  let score_observation k y d =
    require_same ~context:"Ordered_dist.score_observation" d.typ k.src;
    { d with table = Array.of_list (List.filter_map (fun (x, p) ->
          let q = lookup (row k x) y in
          if p *. q > 0.0 then Some (x, p *. q) else None) (Array.to_list d.table)) }

  let normalize_with_mass d =
    let mass = Array.fold_left (fun s (_, p) -> s +. p) 0.0 d.table in
    if mass = 0.0 then ({ d with table = [||] }, 0.0)
    else ({ d with table = Array.map (fun (x, p) -> (x, p /. mass)) d.table }, mass)

  let cardinal t = Array.length t.elements
  let index = rank
  let value_of_index t i = t.elements.(i)
  let support d = Array.to_list (Array.map fst d.table)
  let to_table d = Array.to_list d.table
end

include Make (Parameter.No_values)
