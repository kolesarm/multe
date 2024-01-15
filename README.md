# multe

[![R-CMD-check](https://github.com/kolesarm/multe/workflows/R-CMD-check/badge.svg)](https://github.com/kolesarm/multe/actions) [![Coverage status](https://codecov.io/gh/kolesarm/multe/branch/master/graph/badge.svg)](https://codecov.io/github/kolesarm/multe?branch=master)

This R package implements contamination bias diagnostics, using procedures from
[Goldsmith-Pinkham, Hull, and Kolesár (2023)](https://arxiv.org/abs/2106.05024).
See [multe-stata](https://github.com/gphk-metrics/stata-multe) for a Stata
version of this package.

See vignette [multe](doc/multe.pdf) for description of the package
(available through `vignette("multe")` once package is installed), and the
package [manual](doc/manual.pdf) for documentation of the package functions.

This software package is based upon work supported by the National Science
Foundation under grant numbers SES-22049356 (Kolesár), and by work supported by
the Alfred P. Sloan Research Fellowship (Kolesár).

## Installation

You can get the current development version from GitHub:
``` r
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("kolesarm/multe")
```

## Example

The packages takes the output of `lm`, and computes alternative estimates of the
treatment effects that are free of contamination bias.

``` r
library(multe)
## Regression of IQ at 24 months on race indicators and baseline controls
r1 <- stats::lm(std_iq_24~race+factor(age_24)+female+SES_quintile, weight=W2C0, data=fl)
## Compute alternatives estimates free of contamination bias
m1 <- multe(r1, "race", cluster=NULL)
print(m1)
```

This returns the following table

|          |      PL|     OWN|     ATE|      EW|      CW|
|:---------|-------:|-------:|-------:|-------:|-------:|
|Black     | -0.2574| -0.2482| -0.2655| -0.2550| -0.2604|
|pop_se    |  0.0281|  0.0291|  0.0298|  0.0289|  0.0292|
|oracle_se |      NA|      NA|  0.0298|  0.0288|  0.0290|
|Hispanic  | -0.2931| -0.2829| -0.2992| -0.2862| -0.2944|
|pop_se    |  0.0260|  0.0267|  0.0299|  0.0268|  0.0279|
|oracle_se |      NA|      NA|  0.0299|  0.0268|  0.0278|
|Asian     | -0.2621| -0.2609| -0.2599| -0.2611| -0.2694|
|pop_se    |  0.0343|  0.0343|  0.0418|  0.0343|  0.0475|
|oracle_se |      NA|      NA|  0.0418|  0.0344|  0.0465|
|Other     | -0.1563| -0.1448| -0.1503| -0.1447| -0.1522|
|pop_se    |  0.0369|  0.0370|  0.0359|  0.0368|  0.0370|
|oracle_se |      NA|      NA|  0.0359|  0.0360|  0.0366|


In particular, the package computes the following estimates

- PL :: The original estimate based on a partly linear model
- OWN :: The same estimate, but debiased (OWN)
- ATE :: An estimate of the average treatment effect, using an interacted specification
- EW :: Estimate of the effect of each treatment using one-treatment-at-a-time-regression.
- CW :: Efficient common weights.
