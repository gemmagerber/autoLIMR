% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_limfile.R
\name{write_limfile}
\alias{write_limfile}
\title{write_limfile}
\usage{
write_limfile(data, weighted = NULL, overwrite = overwrite)
}
\arguments{
\item{data}{limfile objects in environment}

\item{weighted}{Logical. Should both weighted and unweighted files be written?}

\item{overwrite}{Logical. Should existing files be overwritten? Default is TRUE.}
}
\description{
Helper internal function of force_write_test() in autoGen().
  A function to write LIMfiles into the working directory.
}
\keyword{internal}
