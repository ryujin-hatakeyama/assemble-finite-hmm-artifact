(* A finite domain stores stable canonical representatives indexed by
   0,...,cardinal-1.  A host value is admitted only when its candidate index is
   in range and the supplied semantic equality relates it to the canonical
   representative at that index.  The callbacks and equality are trusted to be
   pure and stable; construction cannot prove those properties. *)

type 'a t = {
  identity : unit ref;
  name : string;
  cardinal : int;
  equal : 'a -> 'a -> bool;
  candidate_index : 'a -> int;
  representatives : 'a array;
}

let name domain = domain.name
let cardinal domain = domain.cardinal

let check_index domain index =
  if index < 0 || index >= domain.cardinal then
    invalid_arg
      (Printf.sprintf
         "Finite_domain(%s): index %d outside [0,%d)"
         domain.name index domain.cardinal)

let encode domain value =
  let index = domain.candidate_index value in
  check_index domain index;
  if not (domain.equal value domain.representatives.(index)) then
    invalid_arg
      (Printf.sprintf
         "Finite_domain(%s): value is not equivalent to representative %d"
         domain.name index);
  index

let decode domain index =
  check_index domain index;
  domain.representatives.(index)

let elements domain = Array.copy domain.representatives

let make ~name ~cardinal ~equal ~encode:candidate_index ~decode =
  if cardinal < 0 then
    invalid_arg "Finite_domain.make: negative cardinality";
  let representatives = Array.init cardinal decode in
  let domain =
    { identity = ref ();
      name;
      cardinal;
      equal;
      candidate_index;
      representatives }
  in
  Array.iteri
    (fun index value ->
       let encoded = candidate_index value in
       if encoded <> index then
         invalid_arg
           (Printf.sprintf
              "Finite_domain.make(%s): encode(decode(%d)) = %d"
              name index encoded);
       if not (equal value value) then
         invalid_arg
           (Printf.sprintf
              "Finite_domain.make(%s): representative %d is not equal to itself"
              name index))
    representatives;
  domain

let int_range ?(name = "int_range") cardinal =
  make
    ~name
    ~cardinal
    ~equal:Int.equal
    ~encode:(fun value ->
      if value < 0 || value >= cardinal then
        invalid_arg
          (Printf.sprintf
             "Finite_domain.int_range(%s): value %d outside [0,%d)"
             name value cardinal);
      value)
    ~decode:(fun index -> index)

let product ?name left right =
  let name =
    match name with
    | Some name -> name
    | None -> Printf.sprintf "(%s * %s)" left.name right.name
  in
  let cardinal =
    if left.cardinal <> 0 && right.cardinal > max_int / left.cardinal then
      invalid_arg "Finite_domain.product: cardinality overflow";
    left.cardinal * right.cardinal
  in
  make
    ~name
    ~cardinal
    ~equal:(fun (left_x, right_x) (left_y, right_y) ->
      left.equal left_x left_y && right.equal right_x right_y)
    ~encode:(fun (x, y) ->
      let left_index = encode left x in
      let right_index = encode right y in
      left_index * right.cardinal + right_index)
    ~decode:(fun index ->
      if right.cardinal = 0 then
        invalid_arg "Finite_domain.product: empty right factor";
      ( decode left (index / right.cardinal),
        decode right (index mod right.cardinal) ))

let same left right = left.identity == right.identity

let require_same ~context left right =
  if not (same left right) then
    invalid_arg
      (Printf.sprintf
         "%s: finite-domain mismatch (%s vs %s)"
         context left.name right.name)

(* Each call creates a fresh domain identity.  Equality for the represented
   host type is explicit: deriving it solely through [to_rep] would admit an
   invalid value whenever [to_rep] mapped that value to a valid representation. *)
let iso ~name ~equal ~rep ~to_rep ~of_rep =
  make
    ~name
    ~cardinal:rep.cardinal
    ~equal
    ~encode:(fun value -> encode rep (to_rep value))
    ~decode:(fun index -> of_rep (decode rep index))
