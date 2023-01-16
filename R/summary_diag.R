#' @title summary_diag(): basic summary of autoLIMR Markov chain outputs.
#'
#' @description Function to calculate summary statistics
#' for autoLIMR Markov chains.
#' Based on coda::summary() function (Plummer et al., 2020).
#'
#' @param x Object of class "multi_net_output" generated from
#' autoLIMR::multi_net() function.
#'
#' @param flow Character argument. By default, flow = NULL, and the convergence
#' diagnostics are returned for each Markov chain variable. If convergence
#' diagnostics are required for one Markov chain variable only,
#' it can be specified as a character string e.g., flow = "Plant_GPP".
#' We suggest leaving the default flow = NULL.
#'
#' @param xranges Logical argument, default = TRUE. Includes the LIM:Xranges
#' calculated flow ranges for each flow.
#'
#' @param ... Other arguments
#'
#' @return Data frame with each row corresponding to one Markov chain variable,
#' and columns describing various summary statistics.
#'
#' @export
#'
#' @examples
#' # Summary of object "multi_net_output" generated from autoLIMR::multi_net().
#' # The example LIM Declaration files can be found in the package folder
#' # "example_limfiles". If using a custom LIM Declaration file, users can
#' # save the LIM Declaration file in the working directory and refer directly
#' # within the function e.g., file = "mylimfile.R".
#' # If the LIM Declaration file is not saved in the
#' # working directory, the function will accept a valid file path.
#'
#' fpath <- system.file("example_limfiles",
#' "Winter_Weighted_Network_LIMfile.R",
#' package = "autoLIMR")
#' set.seed(1)
#' x <- multi_net(
#'   file = fpath,
#'   iter = 1000,
#'   jmp = NULL,
#'   x0 = NULL)
#' summary_diag(x = x) # MCMC Summary for all flows
#' summary_diag(x = x, flow = "Plant_GPP") # MCMC summary for flow "Plant_GPP"
#'
summary_diag <- function (x, flow = NULL, xranges = TRUE, ...) {
  ### Errors
  # Error: MCMC object must be provided
  if (is.null(x)) {
    stop(
      'Please provide the MCMC object with class "multi_net_output".'
    )
  }
  # Error: Stop, load coda
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package \"coda\" must be installed to use this function.",
         call. = FALSE)
  }
  # Error: Stop, load LIM
  if (!requireNamespace("LIM", quietly = TRUE)) {
    stop("Package \"LIM\" must be installed to use this function.",
         call. = FALSE)
  }
  # Error: if MCMC object is not class "multi_net_output"
  if (!inherits(x, "multi_net_output")) {
    stop(
      'Please ensure MCMC object "x" type is of class "multi_net_output".'
    )
  } else {
    # Convert multi_net_output object into coda::as.mcmc object
    y <- as.data.frame(x[["solved.flow.values"]])
    z <- coda::as.mcmc(y)

    # Fetch Summary Stats for every flow
    sumlst <- summary(object = z)
    sumdat <- as.data.frame(cbind(sumlst[["statistics"]],
                                  sumlst[["quantiles"]]))

    # Add LIM::Xranges
    if (is.null(xranges) | xranges == TRUE) {
      xr <- LIM::Xranges(x[["full_limfile"]])
      colnames(xr) <- c("Xranges_min", "Xranges_max")
      sumdat <- cbind(sumdat, xr)
    }

    # Add MCMC number of variables, iterations, chain, and thinning value
    sumdat$nvar <- coda::nvar(z)
    sumdat$niter <- coda::niter(z)
    sumdat$nchain <- coda::nchain(z)
    sumdat$thin <- sumlst$thin

    if (is.null(flow)) {
      return(sumdat)
    } else {
      flow1 <- as.data.frame(sumdat[flow, , drop = FALSE])
      return(flow1)
    }
  }
}
