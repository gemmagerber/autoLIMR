
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Automated workflow for incorporation and evaluation of data uncertainty in ecological networks

<!-- badges: start -->

[![Published
in](https://img.shields.io/badge/Ecological_Informatics-green?style=plastic&label=Published%20in&link=https%3A%2F%2Fdoi.org%2F10.1016%2Fj.ecoinf.2023.102375)](https://doi.org/10.1016/j.ecoinf.2023.102375)
[![Codecov test
coverage](https://codecov.io/gh/gemmagerber/autoLIMR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gemmagerber/autoLIMR?branch=main)
[![test-coverage](https://github.com/gemmagerber/autoLIMR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/gemmagerber/autoLIMR/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

![](vignettes/images/autoLIMR_logo.png)

The goal of **autoLIMR** is to facilitate the inclusion of data
uncertainty in ecological models. **autoLIMR** does this by:

1.  Automated construction of input files from network data, directly
    compatible with R packages
    [LIM](https://CRAN.R-project.org/package=LIM) and
    [limSolve](https://CRAN.R-project.org/package=limSolve)

2.  Calculation of multiple plausible network configuration using linear
    inverse modelling and Markov Chain Monte Carlo (LIM-MCMC), and

3.  Evaluation of model quality using several visual and statistical
    MCMC convergence diagnostics.

## Installation

You can install the latest version of **autoLIMR** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gemmagerber/autoLIMR")
```
