(* Two finite Grid Sensors models with different movement and internal-choice
   structures.

   Original follows the three-mode transition and observation rules of Lall's
   Grid Sensors example. The grid size, sensor limit and initial distribution
   below are fixed by this artifact and are not claimed to reproduce a complete
   historical experimental setup.

   Variant is our four-heading variant. A Keep/Turn command and a
   command-dependent rotation are sampled before moving, so distinct internal
   choice sequences may reach the same HMM successor.

   The state field m is a movement mode in Original and a heading in Variant. *)

type state = { x1 : int; x2 : int; m : int }
type obs = { y1 : int; y2 : int }
type command = Keep | Turn

let equal_state left right =
  Int.equal left.x1 right.x1
  && Int.equal left.x2 right.x2
  && Int.equal left.m right.m

let equal_obs left right =
  Int.equal left.y1 right.y1 && Int.equal left.y2 right.y2

let equal_command left right =
  match left, right with
  | Keep, Keep | Turn, Turn -> true
  | (Keep, Turn) | (Turn, Keep) -> false

let grid_size = 40
let sensor_limit = 21
let keep_parameter = Parameter.make ~index:0 ~name:"keep"

module Types (M : Model_language.MODEL) = struct
  let one_based ~name n =
    M.tiso ~name ~equal:Int.equal
      ~rep:(M.tint_range ~name:(name ^ "-index") n)
      ~to_rep:(fun value -> value - 1) ~of_rep:(fun index -> index + 1)

  let coordinate_typ = one_based ~name:"coordinate" grid_size
  let sensor_typ = one_based ~name:"sensor" sensor_limit

  let state_typ ~modes =
    M.tiso ~name:"state" ~equal:equal_state
      ~rep:(M.tpair (M.tpair coordinate_typ coordinate_typ) modes)
      ~to_rep:(fun s -> ((s.x1, s.x2), s.m))
      ~of_rep:(fun ((x1, x2), m) -> { x1; x2; m })

  let obs_typ =
    M.tiso ~name:"obs" ~equal:equal_obs
      ~rep:(M.tpair sensor_typ sensor_typ)
      ~to_rep:(fun o -> (o.y1, o.y2)) ~of_rep:(fun (y1, y2) -> { y1; y2 })

  let clamp_position x =
    min grid_size (max 1 x)

  let sensor_channel coordinate =
    if coordinate mod 2 = 0 then
      M.eta sensor_typ 21
    else
      let detected = (coordinate + 1) / 2 in
      M.dist sensor_typ
        [ detected, M.const 0.3; 21, M.const 0.7 ]

  let emission state_typ =
    M.cdist state_typ obs_typ (fun s ->
      M.map2 obs_typ
        (fun y1 y2 -> { y1; y2 })
        (sensor_channel s.x1)
        (sensor_channel s.x2))

  let initial state_typ n_modes =
    M.dist state_typ
      (List.init n_modes (fun m ->
         ({ x1 = 1; x2 = 1; m }, M.const (1.0 /. float_of_int n_modes))))
end

module Original (M : Model_language.MODEL) = struct
  include Types (M)

  let n_modes = 3
  let mode_typ = M.tint_range ~name:"mode" n_modes
  let state_typ = state_typ ~modes:mode_typ

  let displacement = function
    | 0 -> 0, 1
    | 1 -> 1, 0
    | 2 -> 1, 1
    | _ -> invalid_arg "Grid_sensor.Original.displacement"

  let initial = initial state_typ n_modes

  let transition =
    M.cdist state_typ state_typ (fun s ->
      let dx1, dx2 = displacement s.m in
      let next_x1 = clamp_position (s.x1 + dx1) in
      let next_x2 = clamp_position (s.x2 + dx2) in
      let next_mode =
        M.dist mode_typ
          [ s.m, M.const 0.8; (s.m + 1) mod n_modes, M.const 0.2 ]
      in
      M.map state_typ
        (fun m -> { x1 = next_x1; x2 = next_x2; m })
        next_mode)

  (* The parameter and its complement replace the fixed mode weights. *)
  let transition_parameterized () =
    M.cdist state_typ state_typ (fun s ->
      let dx1, dx2 = displacement s.m in
      let next_x1 = clamp_position (s.x1 + dx1) in
      let next_x2 = clamp_position (s.x2 + dx2) in
      let next_mode =
        M.dist mode_typ
          [ s.m, M.parameter keep_parameter;
            (s.m + 1) mod n_modes, M.complement keep_parameter ]
      in
      M.map state_typ
        (fun m -> { x1 = next_x1; x2 = next_x2; m })
        next_mode)

  let emission = emission state_typ
end

module Variant (M : Model_language.MODEL) = struct
  include Types (M)

  let n_modes = 4
  let mode_typ = M.tint_range ~name:"heading" n_modes
  let state_typ = state_typ ~modes:mode_typ
  let command_typ =
    M.tiso ~name:"command" ~equal:equal_command
      ~rep:(M.tint_range ~name:"command-index" 2)
      ~to_rep:(function Keep -> 0 | Turn -> 1)
      ~of_rep:(function 0 -> Keep | 1 -> Turn | _ -> invalid_arg "Grid_sensor.Variant.command")

  let right m =
    (m + 1) mod n_modes

  let displacement = function
    | 0 -> 0, 1
    | 1 -> 1, 0
    | 2 -> 0, -1
    | 3 -> -1, 0
    | _ -> invalid_arg "Grid_sensor.Variant.displacement"

  let step s next_mode =
    let dx1, dx2 = displacement next_mode in
    {
      x1 = clamp_position (s.x1 + dx1);
      x2 = clamp_position (s.x2 + dx2);
      m = next_mode;
    }

  let initial = initial state_typ n_modes

  let command =
    M.dist command_typ
      [ Keep, M.const 0.8; Turn, M.const 0.2 ]

  let rotate s = function
    | Keep ->
        M.dist mode_typ
          [ s.m, M.const 0.9; right s.m, M.const 0.1 ]
    | Turn ->
        M.dist mode_typ
          [ s.m, M.const 0.2;
            right s.m, M.const 0.6;
            right (right s.m), M.const 0.2 ]

  (* capp combines the command distribution with conditional rotation. *)
  let transition =
    M.cdist state_typ state_typ (fun s ->
      M.capp command (M.cdist command_typ mode_typ (rotate s))
      |> M.map state_typ (step s))

  let command_parameterized () =
    M.dist command_typ
      [ Keep, M.parameter keep_parameter;
        Turn, M.complement keep_parameter ]

  let transition_parameterized () =
    M.cdist state_typ state_typ (fun s ->
      M.capp (command_parameterized ()) (M.cdist command_typ mode_typ (rotate s))
      |> M.map state_typ (step s))

  let emission = emission state_typ
end
