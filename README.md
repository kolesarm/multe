[![R-CMD-check](https://github.com/kolesarm/multe/workflows/R-CMD-check/badge.svg)](https://github.com/kolesarm/multe/actions) [![Coverage status](https://codecov.io/gh/kolesarm/multe/branch/master/graph/badge.svg)](https://codecov.io/github/kolesarm/multe?branch=master)

# multe

<!-- badges: start -->
<!-- badges: end -->

This R package implements contamination bias diagnostics, using procedures from [Goldsmith-Pinkham, Hull, and Kolesár (2023)](https://arxiv.org/abs/2106.05024). See the package
[manual](doc/manual.pdf) for documentation of the package functions. See
[multe-stata](https://github.com/gphk-metrics/stata-multe) for a Stata
version of this package.

This software package is based upon work supported by the National Science
Foundation under grant numbers SES-22049356 (Kolesár), and by work supported by
the Alfred P. Sloan Research Fellowship (Kolesár).

## Installation

Alternatively, you can get the current development version from GitHub:
``` r
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("kolesarm/multe")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(multe)
## TODO
```

## TODOs

- F test when V singular; What does panel regression do?
- test:
  dim(Cm)=1
  dim(Cm)=0
