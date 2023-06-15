#' @title effsize_diag(): effective sample size for estimating the mean
#'
#' @description A function to calculate effective sample size for autoLIMR
#' Markov chains.
#' Based on \code{coda::effectiveSize()} function (Plummer et al., 2020).
#' The example LIM Declaration files can be found in the package folder
#' "example_limfiles". If using a custom LIM Declaration file, users can
#' save the LIM Declaration file in the working directory and refer directly
#' within the function e.g., \code{file = "mylimfile.R"}.
#' If the LIM Declaration file is not saved in the
#' working directory, the function will accept a valid file path.
#'
#' @importFrom coda as.mcmc effectiveSize
#'
#' @param x Object of class "multi_net_output" generated from
#' autoLIMR::multi_net() function.
#'
#' @param flow Character argument. By default, flow = NULL, and the convergence
#' diagnostics are returned for each Markov Chain variable. If convergence
#' diagnostics are required for one Markov chain variable only,
#' it can be specified as a character string e.g., flow = "Plant_GPP".
#' We suggest leaving the default flow = NULL.
#'
#' @param ... Other arguments from coda::effectiveSize. Check defaults in
#' the coda package.
#'
#' @return Data frame with each row corresponding to one Markov chain variable,
#' and one column describing Effective Sample Size
#' "effective.sample.size".
#' @export
#' @examples
#' fpath <- system.file("example_limfiles",
#'   "Winter_Weighted_Network_LIMfile.R",
#'   package = "autoLIMR"
#' )
#' set.seed(1)
#' x <- multi_net(
#'   file = fpath,
#'   iter = 1000,
#'   jmp = NULL,
#'   x0 = NULL
#' )
#' effsize_diag(x = x)
#' effsize_diag(x = x, flow = "Plant_GPP")
#'
effsize_diag <- function(x, flow = NULL, ...) {
  ### Errors
  # Error: MCMC object must be provided
  if (is.null(x)) {
    stop('Please provide the MCMC object with class "multi_net_output".')
  }
  # Error: Stop, load coda
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package \"coda\" must be installed to use this function.",
      call. = FALSE
    )
  }
  # Error: Stop, load LIM
  if (!requireNamespace("LIM", quietly = TRUE)) {
    stop("Package \"LIM\" must be installed to use this function.",
      call. = FALSE
    )
  }
  # Error: if MCMC object is not class "multi_net_output"
  if (!inherits(x, "multi_net_output")) {
    stop('Please ensure MCMC object "x" type is of class "multi_net_output".')
  } else {
    # Convert multi_net_output object into coda::as.mcmc object
    y <- as.data.frame(x[["solved.flow.values"]])
    z <- coda::as.mcmc(y)

    eff.all <- as.data.frame(coda::effectiveSize(z))
    names(eff.all)[names(eff.all) == "coda::effectiveSize(z)"] <-
      "effective.sample.size"

    if (is.null(flow)) {
      return(eff.all)
    } else {
      flow1 <- as.data.frame(eff.all[flow, , drop = FALSE])
      return(flow1)
    }
  }
}
