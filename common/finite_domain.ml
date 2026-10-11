(* Runtime description of a finite domain. The intended correspondence is
   between represented values and the integers 0,...,cardinal-1. make checks
   encode (decode i) = i for every index; decode (encode x) = x remains a
   requirement on the supplied functions. *)

type 'a t = {
  uid : int;
  name : string;
  cardinal : int;
  encode : 'a -> int;
  decode : int -> 'a;
}

let next_uid = ref 0

let fresh_uid () =
  let uid = !next_uid in
  incr next_uid;
  uid

let make ~name ~cardinal ~encode ~decode =
  if cardinal < 0 then
    invalid_arg "Finite_domain.make: negative cardinality";
  let domain =
    { uid = fresh_uid (); name; cardinal; encode; decode }
  in
  for index = 0 to cardinal - 1 do
    let value = decode index in
    let encoded = encode value in
    if encoded <> index then
      invalid_arg
        (Printf.sprintf
           "Finite_domain.make(%s): encode(decode(%d)) = %d"
           name index encoded)
  done;
  domain

let check_index domain index =
  if index < 0 || index >= domain.cardinal then
    invalid_arg
      (Printf.sprintf
         "Finite_domain(%s): index %d outside [0,%d)"
         domain.name index domain.cardinal)

let encode domain value =
  let index = domain.encode value in
  check_index domain index;
  index

let decode domain index =
  check_index domain index;
  domain.decode index

let elements domain =
  Array.init domain.cardinal domain.decode

let int_range ?(name = "int_range") cardinal =
  make
    ~name
    ~cardinal
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
    ~encode:(fun (x, y) ->
      let left_index = encode left x in
      let right_index = encode right y in
      left_index * right.cardinal + right_index)
    ~decode:(fun index ->
      if right.cardinal = 0 then
        invalid_arg "Finite_domain.product: empty right factor";
      ( decode left (index / right.cardinal),
        decode right (index mod right.cardinal) ))

let same left right =
  left.uid = right.uid

let require_same ~context left right =
  if not (same left right) then
    invalid_arg
      (Printf.sprintf
         "%s: finite-domain mismatch (%s vs %s)"
         context left.name right.name)

(* Each call creates a fresh domain identity, even if the arguments describe
   the same set. *)
let iso ~name ~rep ~to_rep ~of_rep =
  make
    ~name
    ~cardinal:rep.cardinal
    ~encode:(fun value -> encode rep (to_rep value))
    ~decode:(fun index -> of_rep (decode rep index))
