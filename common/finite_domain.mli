(** Validated finite domains with stable canonical representatives. *)

type 'a t

(** [make ~name ~cardinal ~equal ~encode ~decode] snapshots [decode i] for
    every index in [[0, cardinal)].  It checks [encode (decode i) = i].

    Later, a value [v] is admitted at candidate index [i = encode v] only when
    [i] is in range and [equal v (decode i)] holds for the snapshotted
    representative.  Thus an out-of-carrier value must be rejected by the
    encoder or distinguished from the representative by [equal].

    [equal], [encode], and [decode] are a trusted boundary: they must be total
    on their documented inputs, pure, and stable, and [equal] must faithfully
    express the intended semantic equality.  These properties cannot be proved
    by finite runtime validation.  Canonical representatives must be immutable,
    or clients must treat values returned by [decode] as immutable. *)
val make :
  name:string ->
  cardinal:int ->
  equal:('a -> 'a -> bool) ->
  encode:('a -> int) ->
  decode:(int -> 'a) ->
  'a t

val name : 'a t -> string
val cardinal : 'a t -> int

(** Checked admission of a host value. *)
val encode : 'a t -> 'a -> int

(** Checked access to a canonical representative. *)
val decode : 'a t -> int -> 'a

(** A defensive copy of the representative array. *)
val elements : 'a t -> 'a array

val int_range : ?name:string -> int -> int t
val product : ?name:string -> 'a t -> 'b t -> ('a * 'b) t

(** Domain compatibility is nominal, not merely equality of cardinalities. *)
val same : 'a t -> 'b t -> bool
val require_same : context:string -> 'a t -> 'b t -> unit

(** [equal] is equality on the represented host values, not equality derived
    from [to_rep].  Admission checks the original value against the canonical
    [of_rep] representative at the selected index. *)
val iso :
  name:string ->
  equal:('b -> 'b -> bool) ->
  rep:'a t ->
  to_rep:('b -> 'a) ->
  of_rep:('a -> 'b) ->
  'b t
