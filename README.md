# Assemblé

Assemblé is a compiler for finite hidden Markov models (HMMs), implemented in OCaml and its extension BER MetaOCaml. It accepts initial and emission distributions together with a compositional description of transitions, in which finite probabilistic choices can be combined with deterministic state updates. From the transition description, Assemblé constructs a sparse symbolic HMM transition kernel and generates specialized Forward and Viterbi inference programs. Forward returns the log-likelihood of an observation sequence; Viterbi finds a most probable hidden-state path when the sequence has positive likelihood.

This repository contains the compiler, an unstaged reference implementation, semantic and numerical tests, and archived experimental results. The version provided here accompanies the POPL 2027 SRC submission.

## From a model description to an HMM kernel

In the four-heading Grid Sensors model, a state records the agent's position and heading on a 40 × 40 grid. A transition chooses a command, then a rotation according to that command, before moving the agent in the resulting direction. The description in `common/grid_sensor.ml` includes:

```ocaml
let transition =
  M.cdist state_typ state_typ (fun s ->
    M.capp command (M.cdist command_typ mode_typ (rotate s))
    |> M.map state_typ (step s))
```

Here `M` implements the model-description interface in `common/model_language.ml`. The operation `capp` combines the distribution over commands with the conditional distribution over rotations; `map` then applies the deterministic state update.

These operations have a standard interpretation in terms of finite weighted distributions. Let $U$ and $V$ be finite sets and $R$ a commutative semiring. Write $\mathcal W_R(U)=\{\mu:U\to R\}$ for the $R$-weighted functions on $U$ (the free $R$-semimodule on $U$; see [Bonchi and Santamaria, 2021](https://doi.org/10.1007/978-3-030-71995-1_6)). Given $f:U\to V$, $\mu\in\mathcal W_R(U)$, and a conditional weighted distribution $K:U\to\mathcal W_R(V)$, the two operations are

$$
\begin{aligned}
\operatorname{map}(f,\mu)(v)
  &= \bigoplus_{\substack{u\in U\\f(u)=v}}\mu(u),\\
\operatorname{capp}(\mu,K)(v)
  &= \bigoplus_{u\in U}\mu(u)\otimes K(u)(v).
\end{aligned}
$$

Thus `map` is the pushforward along a function, while `capp` extends a conditional distribution linearly. In the probabilistic interpretation, the weights are nonnegative real numbers and the distributions have total mass one.

For example, `Keep` is chosen with probability 0.8 and retains the heading with conditional probability 0.9, while `Turn` has probabilities 0.2 and 0.2 for the corresponding choices. Both lead to the same successor, contributing $0.8\times0.9=0.72$ and $0.2\times0.2=0.04$. Their sum gives the HMM transition probability $0.76$; the other two successors receive $0.20$ and $0.04$.

More generally, let $\Gamma_x$ be the finite set of internal choice sequences from state $x$. Each $\gamma\in\Gamma_x$ has weight $w_x(\gamma)$ and successor $\tau_x(\gamma)$. The resulting HMM transition kernel is

$$
A(x,x') =
  \sum_{\substack{\gamma\in\Gamma_x\\\tau_x(\gamma)=x'}}
  w_x(\gamma).
$$

The internal choices belong to individual transitions: subsequent transitions and emissions depend on the resulting HMM state. The weights are therefore combined within each transition, after which Forward and Viterbi operate on the HMM kernel $A$. Assemblé represents these combined weights symbolically and records their finite source and target indices when constructing the kernel.

## Generated inference programs

The sparse symbolic kernel serves as the common input to reference inference and two staged code generators:

- **Packed** stores the sparse transition structure in arrays and generates loops over those arrays.
- **Embedded** incorporates the statically known transition indices into the generated program.

Both generators support constant weights and the run-time evaluation of supported parameterized weight expressions. BER MetaOCaml checks the types and scopes of generated code; regression tests compare its Forward and Viterbi results with unstaged reference inference. The artifact includes unstaged, scaled Forward and Viterbi implementations for this purpose.

## Finite domains

State and observation domains are represented by finite indices and canonical OCaml values. For a domain of cardinality $n$, construction fixes a representative $d(i)$ for each $0\leq i<n$ and checks that its candidate encoder $e$ satisfies $e(d(i))=i$. When a value $v$ enters the domain, Assemblé checks that $e(v)$ is in range and that $v$ equals the representative $d(e(v))$ according to the domain's equality test.

User-defined domains supply an equality test through `make_typ` or `tiso`, together with their representation functions. The equality must be an equivalence relation on the intended domain and distinguish canonical representatives at different indices. The representation functions and model callbacks must be total on their declared inputs, pure, and stable, and must respect that equality; canonical representatives must remain unchanged after construction. The implementation of finite domains is in `common/finite_domain.ml` and `common/finite_domain.mli`.

## Getting started

This snapshot was tested on macOS with OCaml 5.3.0 and BER-N153 MetaOCaml, using the opam switch `5.3.0+BER`. The build requires `ocamlfind`, `make`, a POSIX shell, and the OCaml `unix` and `dynlink` libraries. The archived-data checks use Python 3.10 or later; plotting additionally uses NumPy, pandas, and Matplotlib.

From the repository root, run the ordinary tests:

```sh
opam exec --switch=5.3.0+BER -- make check
```

To run the full regression suite, including generated code and archived-result checks, use:

```sh
opam exec --switch=5.3.0+BER -- make \
  check staged-check aggregation-scope-check description-invariance-check \
  verify-model-generated-aggregation-scope verify-archived-performance
```

The suite compares the numerical implementations and generated programs, checks the treatment of internal choices, and verifies the archived results. Tests build locally or use temporary directories; `make clean` removes local build outputs.

## Grid Sensors models and transition semantics

`common/grid_sensor.ml` provides two models on a 40 × 40 grid with the same sensor observation model:

- **Original** (`Grid_sensor.Original`) follows the three-mode transition and observation rules of [Sanjay Lall's Grid Sensors example](https://web.stanford.edu/class/ee365/lectures/hmm.pdf). It moves according to the current mode before updating that mode. This artifact specifies the starting position and a uniform distribution over initial modes.
- **Variant** (`Grid_sensor.Variant`) has four headings. It chooses a `Keep` or `Turn` command, rotates according to that command, and moves according to the resulting heading.

A regression test compares the nested command-and-rotation description with a flat description of the same Variant HMM, whose successor probabilities are `0.76 / 0.20 / 0.04`. Both descriptions produce the same transition kernel at all 6,400 source states, up to floating-point roundoff.

For a separate comparison, `tests/trace_max_dist.ml` interprets internal choices using a maximum instead of a sum. The nested description gives `0.72 / 0.12 / 0.04`, while the flat description gives `0.76 / 0.20 / 0.04`. The first row sums to `0.88`: its entries are scores for a different path-optimization problem, rather than HMM transition probabilities. The test compares the resulting optimization problem with ordinary HMM-state Viterbi.

## Archived evaluation

The repository contains archived results for two experiments, each with a command for verifying the recorded evidence.

### Internal choices and decoding

The experiment in `results/aggregation_scope/model_generated_20261006/` compares HMM-state Viterbi with maximization over internal choices. Each model is tested on 500 model-generated observation sequences of length 20, using seed 2027:

- **Original:** the returned state paths differ in **0/500** sequences.
- **Variant:** the returned paths differ in **124/500** sequences. For **110/500**, the two returned paths are each strictly suboptimal under the other objective, beyond an absolute tolerance of `1e-12`. The remaining 14 differences do not satisfy that stronger criterion.

`make verify-model-generated-aggregation-scope` regenerates the four archived files in a temporary directory and compares them byte for byte.

### Native performance

The benchmark in `results/performance/cp5e-native-final-20260929/` covers both models, Forward and Viterbi, and five transition-weight settings (one constant setting and four parameter values). Each observation sequence has length 300; each condition has eight processes with five warm-up and 30 timed calls per process. The archive's [README](results/performance/cp5e-native-final-20260929/README.md) records the machine, timing boundaries, and full statistics. These measurements were collected from source revision `a552089427fbcbbeaf82b09fcd5ecd674989587c`, which precedes the finite-domain changes in this snapshot.

Across the measured conditions, the median paired **Handwritten-Packed / Packed** runtime ratio is **1.062**. For Variant, the **Packed / Embedded** ratio of condition-median runtimes ranges from **1.119 to 1.421** for Forward and **0.843 to 0.922** for Viterbi: Embedded is faster in the former case, while Packed is faster in the latter. Compared with Packed, Embedded requires **6.11–9.60×** the specialization time, **539–1016×** the printed residual-code size, and **24.0–46.3×** the compile-only peak RSS.

The Handwritten-Packed comparison uses paired process-level ratios; the Packed-Embedded comparison divides each implementation's condition median, computed from eight process medians. Specialization includes preparation, code construction, and `Runcode.run`, while timed inference is measured separately. In comparisons with unstaged kernel-reference inference, the preparation of emission columns also falls on different sides of the timed-call boundary. The archive records these boundaries for interpreting the results.

The verifier checks the archive's SHA-256 manifest and recomputes the summaries and analyses from the recorded rows. To regenerate figures in a separate directory, install NumPy, pandas, and Matplotlib, then run:

```sh
python3 bench/plot_kernel_evaluation.py \
  --result-dir results/performance/cp5e-native-final-20260929 \
  --out-dir /tmp/assemble-figures --model variant
```

The full native benchmark is run by `bench/run_native_final_kernel_evaluation.sh`. It requires substantially more time than the checks above; its compile-only memory measurement uses the macOS `/usr/bin/time -l` interface.

The archived files retain their historical labels: `Grid3`/`grid3` denotes Original, `Grid4`/`grid4` denotes Variant, and `path_max`/`pathmax` denotes the test-only maximization over internal choices. Current benchmark commands use `original` and `variant`.

## Repository structure

```text
common/         finite domains, model descriptions, parameters, symbolic weights
interpreters/   direct finite interpretations (numeric and Boolean) and Forward
model_kernel/   sparse kernel construction, reference inference, code generation
tests/          numerical and semantic checks, archived-result verification
bench/          handwritten baseline, benchmarks, analysis and plotting
results/        preserved semantic and native-performance evidence
```

The same model-description interface has several interpretations under `interpreters/`; the symbolic HMM kernel and its staged lowerings are implemented under `model_kernel/`.

## Scope and license

Assemblé constructs finite HMM kernels whose possible successor states can be enumerated at generation time. Transition weights are represented symbolically using supported constants, parameters, complements, sums, and products; parameter values are supplied at run time while the transition structure stays fixed.

The artifact is released under the [MIT License](LICENSE).
