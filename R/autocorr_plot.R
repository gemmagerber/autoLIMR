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
#' autocorr_plot(x = x, flow = "Plant_GPP")
#'

autocorr_plot <- function(x,
                          flow,
                          lag.max = 50,
                          addtitle = FALSE,
                          ...) {
  # Validate inputs
  if (is.null(x)) {
    stop('Provide the MCMC object as "data.frame", "mcmc", or "multi_net_output".')
  }
  if (is.null(flow)) {
    stop('Provide the flow name in the MCMC object,
         e.g., flow = "Plant_GPP".')
  }

  # Convert input to MCMC format
  z <- if (is.data.frame(x) || inherits(x, "mcmc")) {
    coda::as.mcmc(x)
  } else if (inherits(x, "multi_net_output")) {
    flow_data <- as.data.frame(x[["solved.flow.values"]])
    flow_column <- flow_data[[flow]]
    if (is.null(flow_column))
      stop("Specified flow not found in multi_net_output.")
    coda::as.mcmc(as.data.frame(flow_column))
  } else {
    stop("Unsupported input type for 'x'.
         Must be data.frame, mcmc, or multi_net_output.")
  }

  # Plot autocorrelation
  par(col.main = "white")  # Temporarily hide titles
  coda::autocorr.plot(
    z,
    lag.max = lag.max,
    main = NULL,
    auto.layout = FALSE,
    ask = FALSE
  )

  # Optionally add a title
  if (addtitle) {
    title(main = "Autocorrelation Plot", col.main = "black")
  }
}
