(* Sparse polynomials for symbolic model weights. Constants, parameters,
   complements, addition and multiplication are normalized by merging equal
   monomials. Coefficients are floats, so constant checks use a tolerance and
   are validation checks rather than exact symbolic proofs. *)

type monomial = (int * int) list
type t = (monomial * float) list

let rec compare_monomial (left : monomial) (right : monomial) =
  match left, right with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | (i, e) :: left', (j, f) :: right' ->
      let c = Int.compare i j in
      if c <> 0 then c
      else
        let c = Int.compare e f in
        if c <> 0 then c else compare_monomial left' right'

let rec multiply_monomial (left : monomial) (right : monomial) : monomial =
  match left, right with
  | [], m | m, [] -> m
  | (i, e) :: left', (j, f) :: right' ->
      if i = j then (i, e + f) :: multiply_monomial left' right'
      else if i < j then (i, e) :: multiply_monomial left' right
      else (j, f) :: multiply_monomial left right'

let canonical (terms : (monomial * float) list) : t =
  let sorted = List.stable_sort (fun (m, _) (n, _) -> compare_monomial m n) terms in
  let rec merge = function
    | (m, a) :: (n, b) :: rest when compare_monomial m n = 0 -> merge ((m, a +. b) :: rest)
    | (m, a) :: rest -> if a = 0.0 then merge rest else (m, a) :: merge rest
    | [] -> []
  in
  merge sorted

let zero : t = []
let const c : t = if c = 0.0 then [] else [ ([], c) ]
let variable index : t = [ ([ (index, 1) ], 1.0) ]
let one_minus_variable index : t = canonical [ ([], 1.0); ([ (index, 1) ], -1.0) ]

let add (p : t) (q : t) : t = canonical (p @ q)

let mul (p : t) (q : t) : t =
  canonical
    (List.concat_map (fun (m, a) -> List.map (fun (n, b) -> (multiply_monomial m n, a *. b)) q) p)

let eval (values : float array) (p : t) =
  List.fold_left
    (fun acc (m, c) ->
       acc
       +. List.fold_left
            (fun term (i, e) ->
               if i >= Array.length values then
                 invalid_arg (Printf.sprintf "Polynomial.eval: no value for parameter %d" i);
               term *. (values.(i) ** float_of_int e))
            c m)
    0.0 p

let is_unit (m : monomial) = match m with [] -> true | _ :: _ -> false
let is_constant (p : t) = List.for_all (fun (m, _) -> is_unit m) p

(* Test whether a polynomial is the constant c within the given tolerance. *)
let approx_constant ~tolerance (p : t) c =
  let constant = match p with ([], a) :: _ -> a | _ -> 0.0 in
  Float.abs (constant -. c) <= tolerance
  && List.for_all (fun (m, a) -> is_unit m || Float.abs a <= tolerance) p

let to_string (p : t) =
  match p with
  | [] -> "0"
  | _ ->
    String.concat " + "
      (List.map
         (fun (m, c) ->
            Printf.sprintf "%.17g%s" c
              (String.concat ""
                 (List.map (fun (i, e) -> if e = 1 then Printf.sprintf "*p%d" i
                              else Printf.sprintf "*p%d^%d" i e) m)))
         p)
