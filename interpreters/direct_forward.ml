(* Scaled probability-domain Forward filtering. Each step propagates the
   current distribution, applies the observation likelihood, normalizes, and
   adds the logarithm of the normalization mass to the log-likelihood. *)

module Make (P : Direct_evaluator.PROBABILITY_EVALUATOR) = struct
  let run ~initial ~transition ~emission observations =
    if Array.length observations = 0 then invalid_arg "Direct_forward.run: empty observation sequence";
    let posterior, mass = P.normalize_with_mass (P.score_observation emission observations.(0) initial) in
    let posterior = ref posterior and log_likelihood = ref (log mass) in
    for time = 1 to Array.length observations - 1 do
      let scored = P.score_observation emission observations.(time) (P.capp !posterior transition) in
      let normalized, mass = P.normalize_with_mass scored in
      posterior := normalized;
      log_likelihood := !log_likelihood +. log mass
    done;
    (!posterior, !log_likelihood)
end
