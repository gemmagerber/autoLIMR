#' @title geweke_diag(): Geweke (1992) convergence diagnostic.
#'
#' @description A function to calculate Geweke (1992) convergence diagnostic
#' for autoLIMR Markov chains. Based on \code{coda::geweke.diag()} function
#' (Plummer et al., 2020).
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
#' @param ... Other arguments from \code{coda::geweke.diag}. Check defaults in
#' the coda package.
#'
#' @importFrom coda as.mcmc geweke.diag
#'
#' @return Data frame with each row corresponding to one Markov chain variable
#' object, and three columns describing:
#' 1) Geweke Diagnostic "geweke.diag":
#' Z-scores for a test of equality of means between first and least part of
#' chain,
#' 2) fraction to use from beginning of chain, and
#' 3) fraction to use from end of chain.
#' For more information, see coda package.
#' @export
#'
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
#' geweke_diag(x = x)
#' geweke_diag(x = x, flow = "Plant_GPP")
geweke_diag <- function(x, flow = NULL, ...) {
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

    geweke <- coda::geweke.diag(z, ...)
    geweke.diag <- geweke[["z"]]
    geweke.diag <- as.data.frame(geweke.diag)
    geweke.frac <- as.data.frame(t(geweke[["frac"]]))
    names(geweke.frac)[names(geweke.frac) == "V1"] <- "geweke.frac1"
    names(geweke.frac)[names(geweke.frac) == "V2"] <- "geweke.frac2"
    geweke.all <- cbind(geweke.diag, geweke.frac)

    if (is.null(flow)) {
      return(geweke.all)
    } else {
      flow1 <- as.data.frame(geweke.all[flow, , drop = FALSE])
      return(flow1)
    }
  }
}
