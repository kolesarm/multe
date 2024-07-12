# multe 1.1.0

## New Features

- The function `multe` now also returns a vector of standard deviations of the
  estimated propensity score in each treatment arm, both in the full and in the
  overlap sample

# multe 1.0.1

## Minor improvements and fixes

- Adjust tolerance in unit tests so there are no issues on M1 Mac
- No longer use latex package `bbm` in vignette, which is not available on some
  mac platforms.

# multe 1.0.0

## New Features

- The function `multe` computes contamination bias decomposition and alternative
  estimators using method developed in [Goldsmith-Pinkham, Hull, and Kolesár
  (2024)](https://arxiv.org/abs/2106.05024).
