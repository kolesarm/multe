# multe

[![R-CMD-check](https://github.com/kolesarm/multe/workflows/R-CMD-check/badge.svg)](https://github.com/kolesarm/multe/actions) [![Coverage status](https://codecov.io/gh/kolesarm/multe/branch/master/graph/badge.svg)](https://codecov.io/github/kolesarm/multe?branch=master)

This R package implements contamination bias diagnostics, using procedures from [Goldsmith-Pinkham, Hull, and Kolesár (2023)](https://arxiv.org/abs/2106.05024). See the package
[manual](doc/manual.pdf) for documentation of the package functions. See
[multe-stata](https://github.com/gphk-metrics/stata-multe) for a Stata
version of this package.

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

|          |        PL|       OWN|       ATE|        EW|        CW|
|:---------|---------:|---------:|---------:|---------:|---------:|
|Black     | -0.257415| -0.248221| -0.265524| -0.255010| -0.265109|
|pop_se    |  0.028124|  0.029064|  0.029828|  0.028875|  0.030923|
|oracle_se |        NA|        NA|  0.029802|  0.028792|  0.030432|
|Hispanic  | -0.293147| -0.282927| -0.299243| -0.286168| -0.301426|
|pop_se    |  0.025962|  0.026727|  0.029881|  0.026797|  0.031717|
|oracle_se |        NA|        NA|  0.029868|  0.026775|  0.031533|
|Asian     | -0.262108| -0.260907| -0.259864| -0.261079| -0.269722|
|pop_se    |  0.034262|  0.034323|  0.041769|  0.034326|  0.041925|
|oracle_se |        NA|        NA|  0.041763|  0.034391|  0.041311|
|Other     | -0.156337| -0.144837| -0.150279| -0.144746| -0.151334|
|pop_se    |  0.036913|  0.036964|  0.035941|  0.036841|  0.036018|
|oracle_se |        NA|        NA|  0.035887|  0.036039|  0.035578|


In particular, the package computes the following estimates

- PL :: The original estimate based on a partly linear model
- OWN :: The same estimate, but debiased (OWN)
- ATE :: An estimate of the average treatment effect, using an interacted specification
- EW :: Estimate of the effect of each treatment using one-treatment-at-a-time-regression.
- CW :: Efficient common weights.
