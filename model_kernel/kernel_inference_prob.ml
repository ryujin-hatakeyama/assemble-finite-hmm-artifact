(* Scaled probability-domain inference over an instantiated model kernel.
   Forward and Viterbi share the same push traversal; only the reducer and
   scaling rule differ. Emission weights are applied after transition
   aggregation. *)

module type REDUCER = sig
  (* Combine one predecessor contribution into a destination value. *)
  val update : values:float array -> backpointers:int array -> target:int -> source:int -> candidate:float -> unit
  (* Return the common positive scaling factor for one time step. *)
  val scale : float array -> float
end

module Sum_reducer : REDUCER = struct
  let update ~values ~backpointers:_ ~target ~source:_ ~candidate =
    values.(target) <- values.(target) +. candidate
  let scale v = Array.fold_left ( +. ) 0.0 v
end

module Argmax_reducer : REDUCER = struct
  (* Strict comparison preserves first-predecessor tie-breaking. State 0 is
     only an in-range traceback fallback when every candidate score is zero. *)
  let update ~values ~backpointers ~target ~source ~candidate =
    if candidate > values.(target) then begin
      values.(target) <- candidate;
      backpointers.(target) <- source
    end
  let scale v = Array.fold_left Float.max 0.0 v
end

module Traverse (R : REDUCER) = struct
  let push (transition : ('s, 's) Model_kernel.numeric) current next backpointers =
    Array.fill next 0 (Array.length next) 0.0;
    for source = 0 to Array.length current - 1 do
      let p = current.(source) in
      if p > 0.0 then begin
        let targets = transition.targets.(source) and probs = transition.probabilities.(source) in
        for c = 0 to Array.length targets - 1 do
          R.update ~values:next ~backpointers ~target:targets.(c) ~source ~candidate:(p *. probs.(c))
        done
      end
    done

  let score columns observation predicted scored =
    Array.fill scored 0 (Array.length scored) 0.0;
    Array.iter (fun (x, b) -> scored.(x) <- predicted.(x) *. b) columns.(observation)

  let rescale v =
    let s = R.scale v in
    if s > 0.0 then begin
      let r = 1.0 /. s in
      Array.iteri (fun i p -> v.(i) <- p *. r) v
    end;
    s

  let run ~(initial : float array) ~(transition : ('s, 's) Model_kernel.numeric)
      ~(emission : ('s, 'o) Model_kernel.numeric) (observations : 'o array) =
    let length = Array.length observations in
    if length = 0 then invalid_arg "Kernel_inference_prob: empty observation sequence";
    let n = Array.length initial in
    if n <> transition.source.Finite_domain.cardinal then invalid_arg "Kernel_inference_prob: size mismatch";
    Finite_domain.require_same ~context:"Kernel_inference_prob" transition.destination emission.source;
    let columns = Model_kernel.columns emission in
    let encode = Finite_domain.encode emission.destination in
    let current = Array.make n 0.0 and predicted = Array.make n 0.0 in
    score columns (encode observations.(0)) initial current;
    let log_score = ref (log (rescale current)) in
    let backpointers = Array.make length [||] in
    for time = 1 to length - 1 do
      let bp = Array.make n 0 in
      push transition current predicted bp;
      score columns (encode observations.(time)) predicted current;
      log_score := !log_score +. log (rescale current);
      backpointers.(time) <- bp
    done;
    (!log_score, current, backpointers)
end

module Forward = struct
  module T = Traverse (Sum_reducer)
  let run ~initial ~transition ~emission observations =
    let ll, posterior, _ = T.run ~initial ~transition ~emission observations in
    (posterior, ll)
end

module Viterbi = struct
  module T = Traverse (Argmax_reducer)
  let run ~initial ~transition ~emission observations =
    let score, last, backpointers = T.run ~initial ~transition ~emission observations in
    let length = Array.length observations in
    let best = ref 0 in
    Array.iteri (fun i p -> if p > last.(!best) then best := i) last;
    let path = Array.make length !best in
    for time = length - 1 downto 1 do path.(time - 1) <- backpointers.(time).(path.(time)) done;
    (path, score)
end
