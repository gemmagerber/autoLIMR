#' @title mcmc_plots(): visual Markov chian doagnostics
#'
#' @description Function to return five visual Markov chain convergence
#' diagnostics at once.
#' For any variable of MCMC object, this function returns the following plots:
#' 1) trace,
#' 2) kernel density,
#' 3) running mean,
#' 4) Geweke, and
#' 5) Autocorrelation.
#'
#' @inheritParams trace_plot
#'
#' @return A plot containing various Markov chain convergence diagnostic plots
#' for a single Markov chian variable:
#' 1) trace, 2) kernal density, 3) running mena, 4) Geweke, and 5) Autocorrelation
#' @export
#'
#' @importFrom graphics layout par title
#' @examples
#' # Visual diagnostics of variables in
#' # Markov Chain object "multi_net_output"
#' # generated from autoLIMR::multi_net().
#'
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
#' mcmc_plots(x = x, flow = "Plant_GPP", xranges = TRUE)
#' mcmc_plots(x = x, flow = "Plant_GPP", xranges = FALSE)
#'
mcmc_plots <- function(x, flow, xranges = FALSE, ...) {

  ### Errors
  # Error: MCMC object must be provided
  if (is.null(x)) {
    stop(
      'Please provide the name of the MCMC object as a "data.frame",
         "mcmc", "matrix", or "multi_net_output"'
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
         call. = FALSE)
  }


  ### Four input types accepted (mcmc, matrix, data.frame, multi_net_output)
  if (is.data.frame(x) | is.matrix(x) | inherits(x, "mcmc")) {
    z <- coda::as.mcmc(x)

    } else if (inherits(x, "multi_net_output")) {
      all <- as.data.frame(x[["solved.flow.values"]])
      z <- as.data.frame(all[[paste0(flow)]])
      colnames(z) <- paste0(flow)
      z <- coda::as.mcmc(z)

      } else {
        stop(
          paste0(
            'Please ensure the MCMC object "x" type is one of "mcmc", "data.frame",
             "matrix",  or "multi_net_output"'
          )
        )
      }

  reset_par()
  layout(matrix(c(1, 1, 2, 3, 4, 5), 2, 3, byrow = TRUE))

  #par(col.main = 'white')# Sets all titles to white
  trace_plot(x = z, flow = flow, xranges = xranges, ...)
  dens_plot(x = z, flow = flow, ...)
  runmean_plot(x = z, flow = flow, ...)
  geweke_plot(x = z, flow = flow, ...)
  autocorr_plot(x = z, flow = flow, ...)

  par(mfrow=c(1,1)) # # Reset plotting device back to normal


  # trace_plot(x = z, flow = flow, xranges = xranges, ...)
  # dens_plot(x = z, flow = flow, ...)
  # runmean_plot(x = z, flow = flow, ...)

  #par(col.main = 'white')# Sets all titles to white
  # geweke_plot(x = z, flow = flow, ...)
  # title(main = "Geweke Plot", col.main = "black")

  # autocorr_plot(x = z, flow = flow, ...)
  # title(main = "Autocorrelation Plot", col.main = "black")


  #layout(matrix(c(1, 1, 2, 3, 4, 5), 2, 3, byrow = TRUE))

  # # Plot 1: Traceplot
  # trace_plot(x = x,
  #            flow = flow,
  #            xranges = xranges)
  # #title(main = "Trace Plot", col.main = "black")
  #
  # # Plot 2: Kernel Density Plot
  # dens_plot(x = x, flow = flow)
  # #title(main = "Kernel Density Plot", col.main = "black")
  #
  # # Plot 3: Running Mean Plot
  # runmean_plot(x = x, flow = flow)
  # title(main = "Running Mean Plot", col.main = "black")
  #
  # x <- as.data.frame(x[["solved.flow.values"]])
  # y <- as.data.frame(x[[paste0(flow)]])
  # names(y) <- paste0(flow)
  # z <- as.mcmc(y)

  # par(col.main = 'white')

  # Plot 4: Geweke Plot

  # coda::geweke.plot(
  #   x = z,
  #   frac1 = 0.1,
  #   frac2 = 0.5,
  #   nbins = 20,
  #   pvalue = 0.05,
  #   auto.layout = FALSE,
  #   ask = FALSE
  # )
  #par(col.main = 'white')
  #title(main = "Geweke Plot", col.main = "black")

# Plot 5: Autocorrelation Plot
  # coda::autocorr.plot(
  #   x = z,
  #   lag.max = 50,
  #   auto.layout = FALSE,
  #   ask = FALSE,
  #   main = NULL
  # )
  # title(main = "Autocorrelation Plot", col.main = "black")
}
