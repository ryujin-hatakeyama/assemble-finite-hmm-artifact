(* Small test harness shared by the artifact checks. *)

let failures = ref 0

let report name ok detail =
  Printf.printf "%-66s %s  %s\n%!" name (if ok then "PASS" else "FAIL") detail;
  if not ok then incr failures

let expect_invalid name f =
  match f () with
  | _ -> report name false "no exception"
  | exception Invalid_argument msg -> report name true msg

let finish () =
  if !failures > 0 then begin
    Printf.printf "\n%d FAILURE(S)\n" !failures;
    exit 1
  end else print_endline "\nALL CHECKS PASSED"

let time f =
  let t0 = Sys.time () in
  let r = f () in
  (r, Sys.time () -. t0)

let max_row_diff (a : (int * float) list array) (b : (int * float) list array) =
  if Array.length a <> Array.length b then infinity
  else begin
    let m = ref 0.0 in
    Array.iteri (fun i ra ->
        let rb = b.(i) in
        if List.length ra <> List.length rb || not (List.for_all2 (fun (x, _) (y, _) -> x = y) ra rb)
        then m := infinity
        else List.iter2 (fun (_, p) (_, q) -> m := Float.max !m (Float.abs (p -. q))) ra rb) a;
    !m
  end

let same_keys (a : int list array) (b : int list array) =
  Array.length a = Array.length b
  && Array.for_all2 (fun x y -> List.length x = List.length y && List.for_all2 ( = ) x y) a b
