(* Log-domain reference for numerical cross-checks. It uses the same source
   traversal as the probability-domain implementation and applies emission
   weights once per destination after transition aggregation. *)

let logaddexp a b =
  if a = neg_infinity then b
  else if b = neg_infinity then a
  else if a > b then a +. Float.log1p (exp (b -. a))
  else b +. Float.log1p (exp (a -. b))

let run ~combine ~(initial : float array) ~(transition : ('s, 's) Model_kernel.numeric)
    ~(emission : ('s, 'o) Model_kernel.numeric) (observations : 'o array) =
  let length = Array.length observations in
  if length = 0 then invalid_arg "Kernel_inference_log: empty observation sequence";
  let n = Array.length initial in
  let columns = Model_kernel.columns emission in
  let encode = Finite_domain.encode emission.destination in
  let log_transition = Array.map (Array.map log) transition.probabilities in
  let score observation predicted =
    let v = Array.make n neg_infinity in
    Array.iter (fun (x, b) -> if predicted.(x) > neg_infinity then v.(x) <- predicted.(x) +. log b)
      columns.(observation);
    v
  in
  let current = ref (score (encode observations.(0)) (Array.map log initial)) in
  let backpointers = Array.make length [||] in
  for time = 1 to length - 1 do
    (* Keep traceback in range when every candidate score is negative infinity. *)
    let next = Array.make n neg_infinity and bp = Array.make n 0 in
    Array.iteri (fun source a ->
        if a > neg_infinity then
          Array.iteri (fun c target -> combine next bp target source (a +. log_transition.(source).(c)))
            transition.targets.(source)) !current;
    current := score (encode observations.(time)) next;
    backpointers.(time) <- bp
  done;
  (!current, backpointers)

module Forward = struct
  let run ~initial ~transition ~emission observations =
    let last, _ = run ~combine:(fun v _ t _ c -> v.(t) <- logaddexp v.(t) c) ~initial ~transition ~emission observations in
    (last, Array.fold_left logaddexp neg_infinity last)
end

module Viterbi = struct
  let run ~initial ~transition ~emission observations =
    let last, bps =
      run ~combine:(fun v bp t s c -> if c > v.(t) then (v.(t) <- c; bp.(t) <- s))
        ~initial ~transition ~emission observations in
    let best = ref 0 in
    Array.iteri (fun i a -> if a > last.(!best) then best := i) last;
    let length = Array.length observations in
    let path = Array.make length !best in
    for time = length - 1 downto 1 do path.(time - 1) <- bps.(time).(path.(time)) done;
    (path, last.(!best))
end
