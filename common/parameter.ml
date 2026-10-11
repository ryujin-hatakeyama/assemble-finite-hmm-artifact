(* Model parameters not fixed by the high-level description. Parameters are
   restricted to 0 < p < 1, so both p and 1-p are strictly positive. Direct
   interpretations receive values before evaluation; kernels may keep them
   symbolic until instantiation. *)

type t = {
  index : int;
  name : string;
}

let make ~index ~name =
  if index < 0 then invalid_arg "Parameter.make: negative index";
  { index; name }

type values = float array

let check_values values =
  Array.iteri
    (fun index value ->
       if not (value > 0.0 && value < 1.0) then
         invalid_arg
           (Printf.sprintf
              "Parameter.check_values: parameter %d = %.17g is not in (0,1)"
              index value))
    values

let value values parameter =
  if parameter.index >= Array.length values then
    invalid_arg
      (Printf.sprintf "Parameter.value: no value for %s (index %d)"
         parameter.name parameter.index);
  values.(parameter.index)

module type VALUES = sig
  val values : values
end

module No_values : VALUES = struct
  let values = [||]
end
