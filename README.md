
<!-- README.md is generated from README.Rmd. Please edit that file -->

# autoLIMR

<!-- badges: start -->

[![R-CMD-check](https://github.com/gemmagerber/autoLIMR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gemmagerber/autoLIMR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/gemmagerber/autoLIMR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gemmagerber/autoLIMR?branch=main)
[![test-coverage](https://github.com/gemmagerber/autoLIMR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/gemmagerber/autoLIMR/actions/workflows/test-coverage.yaml)
[![Style](https://github.com/gemmagerber/autoLIMR/actions/workflows/style.yaml/badge.svg)](https://github.com/gemmagerber/autoLIMR/actions/workflows/style.yaml)
<!-- badges: end -->

![autoLIMR](vignettes/images/autoLIMR_logo.png)

The goal of **autoLIMR** is to facilitate the inclusion of data
uncertainty in ecological models. **autoLIMR** does this by:

1.  Automated construction of input files from network data, directly
    compatible with R packages
    [LIM](https://cran.r-project.org/web/packages/LIM/index.html) and
    [limSolve](https://cran.r-project.org/web/packages/limSolve/index.html)
2.  Calculation of multiple plausible network configuration using linear
    inverse modelling and Markov Chain Monte Carlo (LIM-MCMC), and
3.  Evaluation of model quality using several visual and statistical
    MCMC convergence diagnostics.

For more information please consult the [html
vignette](https://gemmagerber.github.io/autoLIMR)

## Installation

You can install the latest version of **autoLIMR** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gemmagerber/autoLIMR")
```
