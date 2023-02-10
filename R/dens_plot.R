#' dens_plot(): a function to plot the Kernel Density of an MCMC object variable
#'
#' @inheritParams trace_plot
#' @inheritParams coda::densplot
#' @param ... Other graphical parameters from coda::densplot.
#' @return A kernel density plot showing sample size and bin width
#' @export
#' @importFrom coda densplot as.mcmc
#' @examples
#' # Example 1: Kernel density plot from a data frame
#' # or coda::as.mcmc() object.
#'
#' set.seed(1)
#' x <- data.frame(rnorm(1000, m = 0, s = 1))
#' names(x) <- "Value"
#' dens_plot(x = x, flow = "Value")
#'
#' # Example 2: Kernel Density plot from multi_net() function output
#' # (class "model_class_output").
#' # The example LIM Declaration files can be found in the package folder
#' # "example_limfiles". If using a custom LIM Declaration file, users can
#' # save the LIM Declaration file in the working directory and refer directly
#' # within the function e.g., file = "mylimfile.R".
#' # If the LIM Declaration file is not saved in the
#' # working directory, the function will accept a valid file path.
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
#' dens_plot(x = x, flow = "Plant_GPP")
#'
dens_plot <- function(x, flow, addtitle = FALSE, ...) {
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

    coda::densplot(
      z,
      show.obs = TRUE,
      ylab = "",
      type = "l",
      main = "",
      right = TRUE
    )

    if (addtitle == TRUE) {
      title(main = "Kernel Density Plot", col.main = "black")
    }
  } else if (inherits(x, "multi_net_output")) {
    all <- as.data.frame(x[["solved.flow.values"]])
    z <- as.data.frame(all[[paste0(flow)]])
    colnames(z) <- paste0(flow)
    z <- coda::as.mcmc(z)

    coda::densplot(
      z,
      show.obs = TRUE,
      ylab = "",
      main = "",
      type = "l",
      right = TRUE
    )

    if (addtitle == TRUE) {
      title(main = "Kernel Density Plot", col.main = "black")
    }
  }
}
