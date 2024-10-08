# multe

[![R-CMD-check](https://github.com/kolesarm/multe/workflows/R-CMD-check/badge.svg)](https://github.com/kolesarm/multe/actions) [![Coverage status](https://codecov.io/gh/kolesarm/multe/branch/master/graph/badge.svg)](https://app.codecov.io/github/kolesarm/multe?branch=master) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/multe)](https://cran.r-project.org/package=multe) [![Download statistics](https://cranlogs.r-pkg.org/badges/multe)](https://cran.r-project.org/package=multe)

This R package implements contamination bias diagnostics, using procedures from
[Goldsmith-Pinkham, Hull, and Kolesár (2024)](https://arxiv.org/abs/2106.05024).
See [multe-stata](https://github.com/gphk-metrics/stata-multe) for a Stata
version of this package.

See vignette [multe](doc/multe.pdf) for description of the package
(available through `vignette("multe")` once package is installed), and the
package [manual](doc/manual.pdf) for documentation of the package functions.

This software package is based upon work supported by the National Science
Foundation under grant numbers SES-22049356 (Kolesár), and by work supported by
the Alfred P. Sloan Research Fellowship (Kolesár).

## Installation

You can install the released version of `multe` from
[CRAN](https://CRAN.R-project.org/package=multe) with:

``` r
install.packages("multe")
```

Alternatively, you can get the current development version from GitHub:
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
library("multe")
## Regression of IQ at 24 months on race indicators and baseline controls
r1 <- stats::lm(std_iq_24~race+factor(age_24)+female+SES_quintile, weight=W2C0, data=fl)
## Compute alternatives estimates free of contamination bias
m1 <- multe(r1, "race", cluster=NULL)
print(m1, digits=3)
```

This returns the following table:

|          |      PL |     OWN |     ATE |      EW |      CW |
|:---------|--------:|--------:|--------:|--------:|--------:|
| Black    | -0.2574 | -0.2482 | -0.2655 | -0.2550 | -0.2604 |
| SE       |  0.0281 |  0.0291 |  0.0298 |  0.0289 |  0.0292 |
| Hispanic | -0.2931 | -0.2829 | -0.2992 | -0.2862 | -0.2944 |
| SE       |  0.0260 |  0.0267 |  0.0299 |  0.0268 |  0.0279 |
| Asian    | -0.2621 | -0.2609 | -0.2599 | -0.2611 | -0.2694 |
| SE       |  0.0343 |  0.0343 |  0.0418 |  0.0343 |  0.0475 |
| Other    | -0.1563 | -0.1448 | -0.1503 | -0.1447 | -0.1522 |
| SE       |  0.0369 |  0.0370 |  0.0359 |  0.0368 |  0.0370 |


In particular, the package computes the following estimates

- PL :: The original estimate based on a partly linear model where covariates
        enter additively
- OWN :: The own treatment effect component of the PL estimator that subtracts
         an estimate of the contamination bias
- ATE :: The unweighted average treatment effect, implemented using regression
         that includes interactions of covariates with the treatment indicators
- EW :: Weighted ATE estimator based on easiest-to-estimate weighting (EW)
        scheme, implemented by running one-treatment-at-a-time regressions.
- CW :: Weighted ATE estimator using easiest-to-estimate common weighting (CW)
        scheme, implemented using weighted regression.
