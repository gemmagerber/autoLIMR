% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dens_plot.R
\name{dens_plot}
\alias{dens_plot}
\title{dens_plot()}
\usage{
dens_plot(x, flow, addtitle = FALSE, ...)
}
\arguments{
\item{x}{The solved MCMC flow values in a data.frame, accessible
from the multi_net() generated list of model outputs ("output") e.g.,
output[["solved.flow.values"]]. Can also refer to the entire multi_net()
output list (class(output) == "multi_net_model).}

\item{flow}{Character argument. The specified name of the flow plot.}

\item{addtitle}{Logical argument, defaults to \code{FALSE}.
If \code{TRUE},
a title describing the type of plot is included above the plot.}

\item{...}{Other graphical parameters from coda::densplot.}
}
\value{
A kernel density plot showing sample size and bin width
}
\description{
function to plot the Kernel Density of an MCMC object variable
}
\examples{
set.seed(1)
x <- data.frame(rnorm(1000, m = 0, s = 1))
names(x) <- "Value"
dens_plot(x = x, flow = "Value")

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
dens_plot(x = x, flow = "Plant_GPP")

}
