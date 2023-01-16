#' runmean_plot(): a function to plot the running mean of an MCMC object
#' variable
#' Shows time average estimates against the number of iterations.
#' Used to decide iteration stopping times.
#'
#' @inheritParams trace_plot
#' @param ... Other base R graphical parameters. See ?plot for more details.
#' @return A running mean plot over time (number of iterations).
#' @importFrom coda as.mcmc
#' @export
#'
#' @examples
#' # Example 1: Running mean plot from a data frame, matrix,
#' # or coda::as.mcmc() object.
#' set.seed(1)
#' x <- matrix(rnorm(1000, m = 0, s = 1))
#' colnames(x) <- "Value"
#' runmean_plot(x = x, flow = "Value")
#'
#' #Example 2: Running mean plot from multi_net() function output
#' # (class "model_class_output").
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
#' runmean_plot(x = x, flow = "Plant_GPP")

runmean_plot <- function(x, flow, ...) {
  # Four input types accepted (mcmc, matrix, data.frame, multi_net_output).

  # MCMC object must be provided
  if (is.null(x)) {
    stop(
      'Please provide the name of the MCMC object as a "data.frame",
         "mcmc", "matrix", or "multi_net_output"'
    )
  }

  # Flow name must be provided
  if (is.null(flow)) {
    stop(
      'Please provide the chracter string name of the flow in the MCMC object
      to plot, e.g., flow = "Plant_GPP".'
    )
  }

  if (is.data.frame(x) | is.matrix(x) | inherits(x, "mcmc")) {
    z <- coda::as.mcmc(x)

    plot(
      y = cumsum({
        {
          z[, paste0(flow)]
        }
      }) / seq_along({
        {
          z[, paste0(flow)]
        }
      }),
      x = 1:nrow(z),
      type = "l",
      xlab = "Iteration",
      ylab = paste0(flow),
      main = "Running Mean Plot"
    )
  } else if (inherits(x, "multi_net_output")) {
    all <- as.data.frame(x[["solved.flow.values"]])
    z <- as.data.frame(all[[paste0(flow)]])
    colnames(z) <- paste0(flow)
    z <- coda::as.mcmc(z)

    plot(
      y = cumsum({
        {
          z[, paste0(flow)]
        }
      }) / seq_along({
        {
          z[, paste0(flow)]
        }
      }),
      x = 1:nrow(z),
      type = "l",
      xlab = "Iteration",
      ylab = paste0(flow),
      main = "Running Mean Plot"
    )

  }


}
