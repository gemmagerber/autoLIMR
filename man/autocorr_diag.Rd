% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autocorr_diag.R
\name{autocorr_diag}
\alias{autocorr_diag}
\title{autocorr_diag()}
\usage{
autocorr_diag(x, flow = NULL, ...)
}
\arguments{
\item{x}{Object of class "multi_net_output" generated from
autoLIMR::multi_net() function.}

\item{flow}{Character argument. By default, flow = NULL, and the convergence
diagnostics are returned for each Markov Chain variable. If convergence
diagnostics are required for one Markov chain variable only,
it can be specified as a character string e.g., flow = "Plant_GPP".
We suggest leaving the default flow = NULL.}

\item{...}{Other arguments from \code{coda::autocorr.diag()}. Check defaults
in the \code{coda} package.}
}
\value{
Data frame with each row corresponding to one Markov chain variable,
and columns describing autocorrelation function at specified lags.
}
\description{
Function to calculate the autocorrelation function at specified
lags for autoLIMR Markov chains.
Based on \code{coda::autocorr.diag()} function (Plummer et al., 2020).
}
\examples{
fpath <- system.file("example_limfiles",
  "Winter_Weighted_Network_LIMfile.R",
  package = "autoLIMR"
)
set.seed(1)
x <- multi_net(
  file = fpath,
  iter = 1000,
  jmp = NULL,
  x0 = NULL
)
autocorr_diag(x = x)
autocorr_diag(x = x, flow = "Plant_GPP")

}
