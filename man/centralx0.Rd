% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/centralx0.R
\name{centralx0}
\alias{centralx0}
\title{centralx0()}
\usage{
centralx0(full_lim, iter = NULL, jmp = NULL, x0 = "central", ...)
}
\arguments{
\item{full_lim}{LIM Declaration file built from \code{LIM_Read()}.}

\item{iter}{An integer defining the total number of iterations (samples)
of the Markov Chain. If \code{NULL}, the default, the number of iterations
is set at 3000. In our experience, we recommend setting a larger number of
iterations (> 10,000) to ensure adequate sampling.}

\item{jmp}{An integer defining the jump size (loosely interpreted as the
'distance' between iterative samples in the solution space).
If \code{NULL}, the default, the jump size is internally
calculated. If a value is provided, the value should be within the ranges of
the flow magnitudes.}

\item{x0}{A single string defining the starting solution algorithm.
If \code{NULL}, the default, the first solution of the Markov Chain is
solved with Least Squares with Equalities and Inequalities (LSEI).
If \code{"central"}, the first solution of the Markov Chain is solved with
the central solution as per \code{LIM:Xranges}.}

\item{...}{Further LIM::Xsample arguments.}
}
\value{
A list of multiple plausible network flow values. The first solution
is solved with
the central solution.
}
\description{
Function to solve multiple plausible networks with Markov
Chain Monte Carlo methods, with a 'central' starting solution calculated
from \code{LIM::Xranges}.

If the 'central' solution is not valid, the starting solution defaults
to the Least Squares with Equalities and Inequalities (LSEI), suitable
for under determined LIM problems.
}
