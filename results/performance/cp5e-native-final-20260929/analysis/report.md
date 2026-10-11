# Finite-kernel evaluation analysis

The fixed observation workload starts with `(1,1)` and uses `(21,21)` for the remaining `T-1` observations, where `T` is the condition's `t_len`. Each timed Kernel-reference invocation constructs emission columns inside ordinary inference. Handwritten-Packed and staged Packed/Embedded prepare their emission columns before the timed invocation; staged specialization is also outside runtime timing. Kernel-reference/staged ratios are therefore end-to-end implementation comparisons with different preparation boundaries, not like-for-like measurements of transition lowering alone.

Runtime columns are medians of independent process medians. `K/P proc` and `H/P proc` are medians of paired per-process speedup ratios; brackets show the process-level IQR. `wins` is the number of independent process pairs in which staged Packed was faster. Break-even counts use only specialization cost (`lowering_prep + code_construction + stage_run`), excluding model/transition construction. A dash means the staged target is not faster than that baseline.

| model | query | weights | p | H ms | K ms | P ms | E ms | K/P proc [IQR] | H/P proc [IQR] | wins H→P | P stage s | E stage s | E/P code | E/P RSS | BE K→P | BE H→P |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| grid3 | forward | constant | — | 6.616 | 25.546 | 6.590 | 7.776 | 3.877 [3.815, 3.910] | 1.000 [0.993, 1.006] | 4/8 | 0.781 | 3.000 | 293.030 | 16.420 | 42 | 29448 |
| grid3 | forward | parameterized | 0.60 | 6.741 | 25.110 | 6.652 | 7.330 | 3.777 [3.726, 3.808] | 1.016 [0.991, 1.021] | 6/8 | 0.773 | 3.158 | 327.276 | 18.372 | 42 | 8700 |
| grid3 | forward | parameterized | 0.70 | 6.719 | 24.653 | 6.647 | 7.373 | 3.709 [3.671, 3.779] | 1.012 [1.006, 1.014] | 6/8 | 0.772 | 3.153 | 327.276 | 18.348 | 43 | 10644 |
| grid3 | forward | parameterized | 0.80 | 6.772 | 25.231 | 6.645 | 7.358 | 3.818 [3.687, 3.927] | 1.016 [1.008, 1.029] | 7/8 | 0.770 | 3.132 | 327.276 | 18.365 | 42 | 6091 |
| grid3 | forward | parameterized | 0.95 | 6.737 | 25.010 | 6.676 | 7.398 | 3.750 [3.672, 3.844] | 1.010 [1.006, 1.017] | 7/8 | 0.768 | 3.178 | 327.276 | 18.367 | 42 | 12649 |
| grid3 | viterbi | constant | — | 12.665 | 30.965 | 11.844 | 15.608 | 2.585 [2.542, 2.656] | 1.057 [1.039, 1.108] | 8/8 | 0.782 | 4.106 | 498.325 | 27.304 | 41 | 953 |
| grid3 | viterbi | parameterized | 0.60 | 12.961 | 30.840 | 12.021 | 15.489 | 2.539 [2.492, 2.570] | 1.062 [1.052, 1.083] | 8/8 | 0.784 | 4.338 | 531.411 | 29.213 | 42 | 835 |
| grid3 | viterbi | parameterized | 0.70 | 13.022 | 29.665 | 12.202 | 14.841 | 2.449 [2.416, 2.503] | 1.063 [1.055, 1.106] | 8/8 | 0.775 | 4.277 | 531.411 | 29.247 | 45 | 945 |
| grid3 | viterbi | parameterized | 0.80 | 12.810 | 30.190 | 11.965 | 15.005 | 2.536 [2.482, 2.546] | 1.061 [1.056, 1.072] | 8/8 | 0.774 | 4.360 | 531.411 | 29.207 | 43 | 917 |
| grid3 | viterbi | parameterized | 0.95 | 13.049 | 30.075 | 11.868 | 15.031 | 2.526 [2.475, 2.590] | 1.086 [1.066, 1.115] | 8/8 | 0.786 | 4.417 | 531.411 | 29.306 | 44 | 667 |
| grid4 | forward | constant | — | 15.507 | 52.607 | 14.810 | 13.232 | 3.701 [3.591, 3.726] | 1.024 [1.021, 1.055] | 8/8 | 0.774 | 4.733 | 538.741 | 27.899 | 21 | 1111 |
| grid4 | forward | parameterized | 0.60 | 16.732 | 53.075 | 15.873 | 11.769 | 3.413 [3.335, 3.452] | 1.037 [1.021, 1.073] | 6/8 | 0.768 | 5.075 | 597.099 | 24.028 | 21 | 895 |
| grid4 | forward | parameterized | 0.70 | 16.854 | 51.966 | 15.827 | 11.139 | 3.287 [3.244, 3.352] | 1.054 [1.044, 1.078] | 8/8 | 0.769 | 5.058 | 597.099 | 24.008 | 22 | 750 |
| grid4 | forward | parameterized | 0.80 | 16.877 | 53.560 | 15.724 | 11.167 | 3.412 [3.332, 3.446] | 1.062 [1.025, 1.082] | 7/8 | 0.767 | 5.082 | 597.099 | 23.996 | 21 | 665 |
| grid4 | forward | parameterized | 0.95 | 16.822 | 53.521 | 15.550 | 11.182 | 3.451 [3.345, 3.478] | 1.075 [1.044, 1.094] | 7/8 | 0.783 | 5.098 | 597.099 | 24.005 | 21 | 616 |
| grid4 | viterbi | constant | — | 24.572 | 98.717 | 21.677 | 25.719 | 4.635 [4.516, 4.666] | 1.147 [1.115, 1.154] | 8/8 | 0.789 | 6.978 | 964.913 | 46.309 | 11 | 273 |
| grid4 | viterbi | parameterized | 0.60 | 26.177 | 100.110 | 22.764 | 24.799 | 4.374 [4.326, 4.436] | 1.150 [1.122, 1.172] | 8/8 | 0.773 | 7.415 | 1015.783 | 40.097 | 10 | 227 |
| grid4 | viterbi | parameterized | 0.70 | 26.753 | 98.999 | 22.614 | 24.529 | 4.355 [4.243, 4.435] | 1.190 [1.141, 1.206] | 8/8 | 0.782 | 7.360 | 1015.783 | 40.095 | 11 | 189 |
| grid4 | viterbi | parameterized | 0.80 | 26.182 | 98.073 | 22.354 | 24.703 | 4.444 [4.297, 4.540] | 1.176 [1.147, 1.216] | 8/8 | 0.777 | 7.439 | 1015.783 | 40.067 | 11 | 203 |
| grid4 | viterbi | parameterized | 0.95 | 25.831 | 99.760 | 22.504 | 24.497 | 4.397 [4.353, 4.429] | 1.136 [1.101, 1.157] | 8/8 | 0.774 | 7.406 | 1015.783 | 40.078 | 11 | 233 |

## Cross-condition medians

- Paired process median Kernel-reference / staged-Packed runtime ratio: 3.705×.
- Paired process median Handwritten-Packed / staged-Packed runtime ratio: 1.062×.
- Embedded / Packed residual-source-size ratio: 535.076×.
- Embedded / Packed specialization-cost ratio: 5.872×.
- Kernel→Packed finite break-even conditions: 20/20, median ceiling 31.5 runs.
- Handwritten→Packed finite break-even conditions: 20/20, median ceiling 865 runs.
