#' @title autocorr_plot()
#' @description a function to plot the Autocorrelation of an MCMC object
#' variable over time.
#'
#' @inheritParams trace_plot
#' @inheritParams coda::autocorr.plot
#' @param ... Other graphical parameters from \code{coda::autocorr_plot}.
#' @return An autocorrelation plot over time (number of iterations).
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- data.frame(rnorm(1000, m = 0, s = 1))
#' colnames(x) <- "Value"
#' autocorr_plot(x = x, flow = "Value")
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
#' autocorr_plot(x = x, flow = "Plant_GPP")
#'

autocorr_plot <- function(x, flow, lag.max = 50,
                          addtitle = FALSE,
                          ...) {
  # Three input types accepted (mcmc, data.frame, multi_net_output).

  # MCMC object must be provided
  if (is.null(x)) {
    stop(
      'Please provide the name of the MCMC object as a "data.frame",
         "mcmc", or "multi_net_output"'
    )
  }

  # Flow name must be provided
  if (is.null(flow)) {
    stop(
      'Please provide the character string name of the flow in the MCMC object
      to plot, e.g., flow = "Plant_GPP".'
    )
  }

  if (is.data.frame(x) | inherits(x, "mcmc")) {
    z <- coda::as.mcmc(x)
    # par(col.main = 'white')# Sets all titles to white
    coda::autocorr.plot(
      x = z,
      lag.max = lag.max,
      main = NULL,
      auto.layout = FALSE,
      ask = FALSE
    )
    par(col.main = "white") # Sets all titles to white
    if (addtitle == TRUE) {
      title(main = "Autocorrelation Plot", col.main = "black")
    }
  } else if (inherits(x, "multi_net_output")) {
    all <- as.data.frame(x[["solved.flow.values"]])
    z <- as.data.frame(all[[paste0(flow)]])
    colnames(z) <- paste0(flow)
    z <- coda::as.mcmc(z)

    par(col.main = "white") # Sets all titles to white
    coda::autocorr.plot(
      x = z,
      lag.max = lag.max,
      main = NULL,
      auto.layout = FALSE,
      ask = FALSE
    )
    par(col.main = "white") # Sets all titles to white

    if (addtitle == TRUE) {
      title(main = "Autocorrelation Plot", col.main = "black")
    }
  }
}
