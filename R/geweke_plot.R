#' geweke_plot(): function to create Geweke (Date) plots for varibles of
#' MCMC objects
#' Used to decide...
#' Inspired by coda package
#' See (references) for more information
#' @inheritParams trace_plot
#' @inheritParams coda::geweke.plot
#' @param ... Other graphical parameters from coda::geweke.plot.
#' @importFrom coda geweke.plot as.mcmc
#' @return Geweke plot of variable
#' @export
#'
#' @examples
#' # Example 1: Geweke Plot from a data frame
#' # or coda::as.mcmc() object.
#' set.seed(1)
#' x <- data.frame(rnorm(1000, m = 0, s = 1))
#' names(x) <- "Value"
#' geweke_plot(x = x, flow = "Value")
#'
#' # Example 2: Geweke Plot from multi_net() function output
#' # (class "model_class_output").
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
#' geweke_plot(x = x, flow = "Plant_GPP")
#'
geweke_plot <- function(x, flow, nbins = 20,
                        pvalue = 0.05, frac1 = 0.1,
                        frac2 = 0.5, addtitle = FALSE, ...) {
  ### Errors
  # Error: MCMC object must be provided
  if (is.null(x)) {
    stop(
      'Please provide the name of the MCMC object as a "data.frame",
         "mcmc", or "multi_net_output"'
    )
  }
  # Error: Flow name must be provided
  if (is.null(flow)) {
    stop(
      'Please provide the chracter string name of the flow in the MCMC object
      to plot, e.g., flow = "Plant_GPP".'
    )
  }

  # Error: Stop, load coda
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package \"coda\" must be installed to use this function.",
      call. = FALSE
    )
  }


  ### Four input types accepted (mcmc, matrix, data.frame, multi_net_output)

  if (is.data.frame(x) | inherits(x, "mcmc")) {
    z <- coda::as.mcmc(x)

    par(col.main = "white") # Sets all titles to white
    coda::geweke.plot(
      x = z,
      frac1 = frac1,
      frac2 = frac2,
      nbins = nbins,
      pvalue = pvalue,
      auto.layout = FALSE,
      ask = FALSE
    )

    if (addtitle == TRUE) {
      title(main = "Geweke Plot", col.main = "black")
    }
  } else if (inherits(x, "multi_net_output")) {
    x <- as.data.frame(x[["solved.flow.values"]])
    y <- as.data.frame(x[[paste0(flow)]])
    names(y) <- paste0(flow)
    z <- coda::as.mcmc(y)

    par(col.main = "white") # Sets all titles to white
    coda::geweke.plot(
      x = z,
      frac1 = frac1,
      frac2 = frac2,
      nbins = nbins,
      pvalue = pvalue,
      auto.layout = FALSE,
      ask = FALSE
    )
    if (addtitle == TRUE) {
      title(main = "Geweke Plot", col.main = "black")
    }
  } else {
    stop(
      paste0('Please ensure the object "x" type is one of "mcmc", "data.frame",
             or "multi_net_output"')
    )
  }
}
