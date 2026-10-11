# Assemblé

Assemblé is a compiler for finite hidden Markov models (HMMs), written in OCaml
and BER MetaOCaml. It takes compositional descriptions of an HMM's initial,
transition, and emission distributions, constructs a sparse symbolic transition
kernel, and generates Forward and Viterbi inference code. Different internal
choices can lead to the same HMM successor. Assemblé adds their
probabilities while forming the kernel, before inference uses it.

The repository contains the compiler, an unstaged reference implementation,
tests of the transition semantics, and archived performance measurements.

*Artifact snapshot accompanying the POPL 2027 SRC submission.*

## From a description to inference code

The four-heading Grid Sensors model describes one transition by first choosing
a command, then choosing a rotation according to that command, and finally
moving in the resulting direction. Its OCaml description contains:

```ocaml
let transition =
  M.cdist state_typ state_typ (fun s ->
    M.capp command (M.cdist command_typ mode_typ (rotate s))
    |> M.map state_typ (step s))
```

Here `M` implements the model-description interface in
`common/model_language.ml`, and `s` is the current HMM state. `capp` combines
the command and conditional rotation distributions; `map` applies the
deterministic state update. The full model is in `common/grid_sensor.ml`.

For example, `Keep` has probability 0.8 and `Turn` has probability 0.2. Each
command has its own rotation probabilities. The two internal choices that
retain the current heading contribute $0.8\times0.9=0.72$ and
$0.2\times0.2=0.04$ to the same successor. Adding their contributions gives
$0.76$; the other two successors receive $0.20$ and $0.04$.

More generally, if an internal choice sequence $\gamma$ has weight
$w_x(\gamma)$ and produces successor $\tau_x(\gamma)$, the HMM transition
probability is

$$
A(x,x') = \sum_{\gamma:\,\tau_x(\gamma)=x'} w_x(\gamma).
$$

The resulting kernel is shared by ordinary Forward and Viterbi inference.
Assemblé also generates two forms of specialized code from it:

- **Packed** stores the sparse transition structure in arrays and emits loops
  over those arrays.
- **Embedded** emits the statically known transitions directly into the
  generated code.

Either form can use constant transition weights or evaluate supported symbolic
weights from a parameter vector at run time. The implementation also provides
unstaged, scaled Forward and Viterbi for reference and testing.

## Getting started

The artifact was verified on macOS using OCaml 5.3.0 and BER-N153 MetaOCaml in
the opam switch `5.3.0+BER`. It also requires `ocamlfind`, `make`, a POSIX shell,
and the OCaml `unix` and `dynlink` libraries. Python 3.10 or later is used by
the archived-data checks. Plotting additionally requires NumPy, pandas, and
Matplotlib.

From the repository root, run the ordinary tests:

```sh
opam exec --switch=5.3.0+BER -- make check
```

To run the full set of existing checks, including generated code and the
archived results:

```sh
opam exec --switch=5.3.0+BER -- make \
  check staged-check aggregation-scope-check description-invariance-check \
  verify-model-generated-aggregation-scope verify-archived-performance
```

The checks verify the numerical implementations, the generated Forward and
Viterbi code, the treatment of internal choices, and the archived evidence.
They build locally or use temporary directories; the archived results are not
rewritten. `make clean` removes the local build outputs. All checks operate
within this repository, without access to the private development history.

## Models and transition semantics

`common/grid_sensor.ml` contains two examples on a 40 × 40 grid with the same
sensor observation model:

- **Original** (`Grid_sensor.Original`) follows the three-mode transition and
  observation rules of
  [Sanjay Lall's Grid Sensors example](https://web.stanford.edu/class/ee365/lectures/hmm.pdf).
  It moves according to the current mode before updating that mode. The
  starting position and uniform distribution over initial modes are specified
  by this artifact.
- **Variant** (`Grid_sensor.Variant`) uses four headings. It chooses a
  `Keep`/`Turn` command, rotates according to the command, and then moves.
  Several internal choice sequences can produce the same successor.

The models differ in their movement rules and internal choices, as well as in
the number of modes.

One regression test compares two descriptions of the *same* Variant HMM: the
nested command-and-rotation description above and a flat description with the
three probabilities `0.76 / 0.20 / 0.04`. They produce the same ordinary
transition kernel at all 6,400 source states, up to floating-point roundoff.

For comparison, `tests/trace_max_dist.ml` is a **test-only** interpretation that
uses a maximum over internal choices reaching a successor instead of their
sum. The nested description then yields scores `0.72 / 0.12 / 0.04`, whereas
the flat description yields `0.76 / 0.20 / 0.04`. The former row sums to
`0.88`, so it is not a probability distribution. Viterbi over these scores
solves a different optimization problem from ordinary Viterbi over the HMM
kernel. This interpretation is used to test the distinction; it is not a
second inference mode provided by the compiler.

## Archived evaluation

The checked-in results belong to two experiments. Both can be verified with
the commands above without repeating the full measurements.

### Internal choices and decoding

The experiment in `results/aggregation_scope/model_generated_20261006/`
compares ordinary Viterbi with Viterbi over the test-only maximum of internal
choices. It uses 500 model-generated observation sequences of length 20 for
each model (seed 2027):

- Original: the returned state paths differ in **0/500** sequences.
- Variant: the returned paths differ in **124/500** sequences. In **110/500**,
  each returned path is strictly suboptimal under the other objective, beyond
  the experiment's absolute tolerance of `1e-12`. The remaining 14 differences
  do not meet that stronger criterion.

`make verify-model-generated-aggregation-scope` regenerates the four archived
files in a temporary directory and compares them byte for byte.

### Native performance

The archived native benchmark is in
`results/performance/cp5e-native-final-20260929/`. It uses both models,
Forward and Viterbi, and five transition-weight settings (constant and four
parameter values). The observation sequence has length 300. Each condition
has eight processes, each with five warm-up and 30 timed calls. The archive's
[README](results/performance/cp5e-native-final-20260929/README.md) records
the machine, original source revision, measurement boundaries, and complete
statistics.

Across these conditions, the median paired
**Handwritten-Packed / Packed** runtime ratio is **1.062**. For the Variant,
the **Packed / Embedded** ratio of condition-median runtimes ranges from
**1.119 to 1.421** for Forward, and from **0.843 to 0.922** for Viterbi.
Thus Embedded is faster for Variant Forward in these measurements, whereas
Packed is faster for Variant Viterbi. Embedded also has higher specialization
costs: **6.11–9.60×** the specialization time, **539–1016×** the printed
residual-code size, and **24.0–46.3×** the compile-only peak RSS.

The two runtime statistics are deliberately different. The
Handwritten-Packed / Packed comparison uses paired process-level ratios;
Packed / Embedded divides each implementation's condition median, computed
from the eight process medians. Specialization time includes preparing the
lowering, constructing the code, and using `Runcode.run` to obtain the
specialized inference function; timed inference is measured separately.
For comparisons with unstaged kernel-reference inference, emission-column
preparation also differs between the timed-call boundaries. Consult the
archive before interpreting those ratios as effects of staging alone.

The archive verifier checks the SHA-256 manifest and recomputes its summaries
and analyses from the recorded rows. To regenerate figures in a separate
directory, install NumPy, pandas, and Matplotlib, then run:

```sh
python3 bench/plot_kernel_evaluation.py \
  --result-dir results/performance/cp5e-native-final-20260929 \
  --out-dir /tmp/assemble-figures --model variant
```

The full benchmark is run by `bench/run_native_final_kernel_evaluation.sh`.
It takes substantially longer than the checks above; its compile-only memory
measurement uses the macOS `/usr/bin/time -l` interface. Reproduction on
other operating systems has not been established.

The archived files retain their historical labels: `Grid3`/`grid3` means
Original, `Grid4`/`grid4` means Variant, and `path_max`/`pathmax` refers to
the test-only maximization over internal choices. Current benchmark commands
use `original` and `variant`.

## Repository structure

```text
common/         finite domains, model descriptions, parameters, symbolic weights
interpreters/   direct finite interpretations (numeric and Boolean) and Forward
model_kernel/   sparse kernel construction, reference inference, code generation
tests/          numerical and semantic checks, archived-result verification
bench/          handwritten baseline, benchmarks, analysis and plotting
results/        preserved semantic and native-performance evidence
```

The `interpreters/` name describes the role of the present implementations:
several interpretations of the same finite model-description interface. The
code for constructing the sparse symbolic kernel remains in `model_kernel/`.

## Scope and license

Assemblé works with finite, indexed state and observation domains and
transitions whose possible successors can be enumerated when the kernel is
constructed. Its symbolic weights are built from supported constants,
parameters, complements, sums and products. Statically known transition
structure is shared by the reference interpreter and generated code; the
supported parameter values supply transition weights at run time.

The artifact is released under the [MIT License](LICENSE).
