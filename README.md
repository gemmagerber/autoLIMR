
<!-- README.md is generated from README.Rmd. Please edit that file -->

# autoLIMR

<!-- badges: start -->

[![R-CMD-check](https://github.com/gemmagerber/autoLIMR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gemmagerber/autoLIMR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/gemmagerber/autoLIMR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gemmagerber/autoLIMR?branch=main)
[![test-coverage](https://github.com/gemmagerber/autoLIMR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/gemmagerber/autoLIMR/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The goal of autoLIMR is to automate the construction, calculation, and
evaluation of multiple plausible networks using linear inverse modelling
coupled with Markov Chain Monte Carlo (LIM_MCMC).

autoLIMR achieves this by: \* 1. Translating network input data into
files directly compatible with R packages
[LIM](https://cran.r-project.org/web/packages/LIM/index.html) and
[limSolve](https://cran.r-project.org/web/packages/limSolve/index.html)
\* 2. Solves multiple plausible networks using LIM-MCMC procedures in
[LIM](https://cran.r-project.org/web/packages/LIM/index.html) and
[limSolve](https://cran.r-project.org/web/packages/limSolve/index.html)
\* 3. Evaluates if the multiple plausible networks are representitave of
the original input data using a suite of visual and statistical MCMC
diagnostics.

For more information please consult the html vignette (in progress)

## Installation

You can install the latest version of autoLIMR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gemmagerber/autoLIMR")
```
