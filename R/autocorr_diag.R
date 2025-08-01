#' @title autocorr_diag()
#'
#' @description Function to calculate the autocorrelation function at specified
#' lags for autoLIMR Markov chains.
#' Based on \code{coda::autocorr.diag()} function (Plummer et al., 2020).
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
#' @param ... Other arguments from \code{coda::autocorr.diag()}. Check defaults
#' in the \code{coda} package.
#'
#' @return Data frame with each row corresponding to one Markov chain variable,
#' and columns describing autocorrelation function at specified lags.

#' @export
#' @importFrom coda as.mcmc autocorr.diag
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
#' autocorr_diag(x = x)
#' autocorr_diag(x = x, flow = "Plant_GPP")
#'
autocorr_diag <- function(x, flow = NULL, ...) {
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

    autocorr <- coda::autocorr.diag(mcmc.obj = z, ...)
    ac <- as.data.frame(t(autocorr))

    if (is.null(flow)) {
      return(ac)
    } else {
      flow1 <- as.data.frame(ac[flow, , drop = FALSE])
      return(flow1)
    }
  }
}
