# autoLIMR 3.0.1

# autoLIMR 3.0.0

# autoLIMR 2.2.2

* Changes for CRAN acceptance
* Citation entry changes
* Excluded @dontrun examples in functions `mcmc_diags()` and `raftery_diags()`
* Fixed bugs in function `aevar()`

# autoLIMR 2.2.1

Vignette updates.

# autoLIMR 2.2.0

# autoLIMR 2.1.1

# autoLIMR 2.1.0

# autoLIMR 2.0.0

This is a major release adding a range of substantial new features and fixing
bugs.

## Breaking changes

* Brand new features mean some dependencies not previously included. These
include `coda` (>= 0.19.4), `LIM` (>= 1.4.7), and `network` (>= 1.18.0)
* Roxygen update 7.2.1 -> 7.2.3


## New features

* Added a `NEWS.md` file to track changes to the package.

### Calculate multiple plausible networks

* Added `multi_net()` function and friends to calculate multiple plausible networks. Friend functions include:
  + Starting point functions `centralx0()` and `defaultx0`
  + Network packing function `prepack_fun()` and inner functions `export_fun()`,
  `fmat_fun()`, `input_fun()`, `living_fun()`, `output_fun()`, `resp_fun()`
  
### Markov Chain Monte Carlo (MCMC) Convergence Diagnostics

* Added various functions for visual MCMC convergence diagnostics:
  + `trace_plot()`
  + `dens_plot()`
  + `runmean_plot()`
  + `geweke_plot()`
  + `autocorr_plot()`
  + `mcmc_plots()` function seamlessly combines all plots into
  a single plot output.
* Added various functions for statistical MCMC convergence diagnostics:
  + `summary_diag()`
  + `geweke_diag()`
  + `effsize_diag()`
  + `heidel_diag()`
  + `raftery_diag()`
  + `autocorr_diag()`
  + `mcmc_diags()` function returns all statistical MCMC convergence diagnostics
  in a single list or data frame.
  
### Examples

* Added eight example LIM Declaration files for a toy four-node network in 
`inst/example_limfiles` directory
  + Four weighted network LIM Declaration files (Summer, Spring, Autumn, Winter)
  + Four unweighted network LIM Declaration files (Summer, Spring, Autumn, Winter)

## Bug fixes

* `autoGen()` - changed file path names to simply "unweighted_limfiles" or
"weighted_limfiles"

