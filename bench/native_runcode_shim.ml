(* Native BER MetaOCaml compatibility shim.
   The bytecode API Runcode.run has no native implementation in N153.
   Native builds compile this file as runcode.ml, so existing calls to
   Runcode.run are redirected to Runnative.run. *)

let run = Runnative.run
