#' @title mcmc_diags(): Suite of convergence diagnostics for Markov chains.
#'
#' @description Function to return a suite of Markov Chain diagnostics
#' at once. Includes 1) Summary, 2) Geweke (1992) Diagnostics,
#' 3) Effective Sample Size, 4) Heidelberger-Welch Diagnostics,
#' 5) Raftery Lewis Diagnostics, and 6) Autocorrelation.
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
#' @param ... Other arguments inherited from Markov chain convergence
#' diagnostic functions. Check defaults in
#' the coda package.
#'
#' @return A list containing data.tables of various Markov Chain
#' diagnostics included in package autoLIMR: 1) summary,
#' 2) Geweke (1992) Diagnostics, 3) Effective Sample Size,
#' 4) Heidelberger-Welch Diagnostics, 5) Raftery Lewis Diagnostics, and 6)
#' Autocorrelation
#' @export
#'
#' @examples
#' \donttest{
#' fpath <- system.file("example_limfiles",
#'   "Winter_Weighted_Network_LIMfile.R",
#'   package = "autoLIMR"
#' )
#' set.seed(1)
#' x <- multi_net(
#'   file = fpath,
#'   iter = 4000,
#'   jmp = 0.0001,
#'   x0 = NULL
#' )
#' # mcmc_diags(x = x)
#' mcmc_diags(x = x, flow = "Plant_GPP")
#' }
#'
mcmc_diags <- function(x, flow = NULL, ...) {
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
  }

  # Combines all MCMC diagnostics into one list

  summary <- summary_diag(x = x, flow = flow, ...)
  geweke <- geweke_diag(x = x, flow = flow, ...)
  effsize <- effsize_diag(x = x, flow = flow, ...)
  heidel <- heidel_diag(x = x, flow = flow, ...)
  raftery <- raftery_diag(x = x, flow = flow, ...)
  autocorr <- autocorr_diag(x = x, flow = flow, ...)

  returnlist <- list(summary, geweke, effsize, heidel, raftery, autocorr)
  names(returnlist) <- c(
    "Summary",
    "Geweke",
    "Effective.Sample.Size",
    "Heidelberger-Welch",
    "Raftery-Lewis",
    "Autocorrelation"
  )
  return(returnlist)
}
