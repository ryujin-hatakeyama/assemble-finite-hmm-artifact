# Model-generated aggregation-scope experiment

Seed: `2027`; sequences per model: `500`; sequence length: `20`; model order: Grid3 then Grid4.

The data were sampled from the ordinary summed HMM initial, transition, and emission distributions. The alternative decoder uses unnormalised multiplicative scores derived by interpreting the same model with max, rather than addition, when duplicate target indices are combined.

## Interpretation validation

- Grid3: summed and path-max transition scores agree for every source state.
- Grid4 summed same/right/right^2 weights: `0.76 / 0.20 / 0.04`.
- Grid4 path-max same/right/right^2 scores: `0.72 / 0.12 / 0.04`; these do not form a probability row.

## Aggregate results

| Model | Returned decoded paths differ | Wilson 95% CI | Both returned paths strictly suboptimal under the other objective | Wilson 95% CI | Differing path pairs not meeting the stricter criterion | Hamming distance among differing paths |
|---|---:|---:|---:|---:|---:|---|
| Grid3 | 0/500 (0.000000) | [0.000000, 0.007624] | 0/500 (0.000000) | [0.000000, 0.007624] | 0 | NA |
| Grid4 | 124/500 (0.248000) | [0.212164, 0.287678] | 110/500 (0.220000) | [0.185901, 0.258369] | 14 | median 8.00; Q1 5.00; Q3 16.00; IQR 11.00 |

The stricter criterion holds only when the summed-objective path is strictly suboptimal under the path-max objective and the path-max-objective path is strictly suboptimal under the summed objective, in each case by more than the absolute `1e-12` score tolerance. Failing this criterion does not identify the reason for the returned-path difference. Passing it concerns only the two returned paths and does not establish that the complete optimal-path sets are disjoint. Quartiles use linear interpolation with `h=(n-1)p` (type 7). The rates characterize only these fixed Grid3 and Grid4 models and do not establish superiority of either decoder.
