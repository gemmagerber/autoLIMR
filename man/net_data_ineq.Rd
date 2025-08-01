% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/net_data_ineq.R
\name{net_data_ineq}
\alias{net_data_ineq}
\title{function: net_data_ineq()}
\usage{
net_data_ineq(x, primary_producer, NLNode = NULL)
}
\arguments{
\item{x}{network input data matrix}

\item{primary_producer}{Character vector defining primary producer compartments}

\item{NLNode}{Character vector defining one or multiple non-living compartments}
}
\value{
Vector of inequalities from network input data matrix for LIM declaration file
}
\description{
Inequalities definition (with headings)
}
\keyword{internal}
