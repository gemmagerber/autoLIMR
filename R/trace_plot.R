#' trace_plot(): a function to plot an MCMC object variable over time (iterations)
#' Trace plots can be used to assess the mixing of the MCMC chain over time
#'
#' @param x The solved MCMC flow values in a data.frame, accessible
#' from the multi_net() generated list of model outputs ("output") e.g.,
#' output[["solved.flow.values"]]. Can also refer to the entire multi_net()
#' output list (class(output) == "multi_net_model).
#' @param flow Character argument. The specified name of the flow plot.
#'
#' @param xranges Logical argument, default FALSE. If xranges = TRUE, the plot
#' includes horizontal lines in the trace plot indicating absolute maximum and
#' minimum values of the flow range calculated by LIM::Xranges.
#' If xranges = FALSE, the minimum and maximum lines are omitted from the plot.
#'
#' @param addtitle Logical argument, defaults to FALSE. If 'addtitles = TRUE',
#' a title describing the type of plot is included above the plot.
#'
#' @param ... Other base R graphical parameters. See ?plot for more details.
#' @importFrom graphics abline legend
#' @importFrom utils data
#' @importFrom coda as.mcmc
#' @return A trace plot of MCMC variable over time (or number of iterations)
#' @export
#'
#' @examples
#' # Example 1: Traceplot from a data.frame or coda::as.mcmc() object.
#' # Since this example is not directly from a "multi_net_object" class, the
#' # flow ranges from the LIM Declaration file cannot be included.
#'
#' set.seed(1)
#' x <- data.frame(rnorm(1000, m = 0, s = 1))
#' colnames(x) <- "Value"
#' trace_plot(x = x, flow = "Value", xranges = FALSE)
#'
#' # Example 2: Traceplot from multi_net() function output
#' # (class "model_class_output"). If so, flow ranges calculated from the
#' # LIM Declaration file using LIM::Xranges() can be included in the
#' # trace_plot() function argument.
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
#' trace_plot(x = x, flow = "Plant_GPP", xranges = TRUE)
#' trace_plot(x = x, flow = "Plant_GPP", xranges = FALSE)


trace_plot <- function(x, flow, xranges = FALSE,
                       addtitle = FALSE, ...) {

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
      'Please provide the character string name of the flow in the MCMC object
      to plot, e.g., flow = "Plant_GPP".'
    )
  }

  ### Four input types accepted (mcmc, data.frame, multi_net_output)
  if (is.data.frame(x) | inherits(x, "mcmc")) {
    z <- coda::as.mcmc(x)

    plot(
      x = 1:nrow(z),
      y = z[, paste0(flow)],
      type = "l",
      xlab = "Iteration",
      ylab = "Value"
    )

    if (addtitle == TRUE) {
      title(main = "Trace Plot", col.main = "black")
    }


  } else if (inherits(x, "multi_net_output")) {
    all <- as.data.frame(x[["solved.flow.values"]])
    z <- as.data.frame(all[[paste0(flow)]])
    colnames(z) <- paste0(flow)
    z <- coda::as.mcmc(z)

    if (xranges == FALSE | is.null(xranges)) {
      plot(
        x = 1:nrow(z),
        y = z[, paste0(flow)],
        type = "l",
        xlab = "Iteration",
        ylab = "Value"
      ) # ylab = paste0(flow)
      if (addtitle == TRUE) {
        title(main = "Trace Plot", col.main = "black")
      }
    }
    if (xranges == TRUE) {
      xr <- t(LIM::Xranges(x[["full_limfile"]]))
      min <-
        xr[grep(pattern = "min", x = rownames(xr)),
           grep(pattern = paste0(flow), x = colnames(xr))]
      max <-
        xr[grep(pattern = "max", x = rownames(xr)),
           grep(pattern = paste0(flow), x = colnames(xr))]
      plot(
        x = 1:length(z),
        y = z[, paste0(flow)],
        ylim = c(min, max),
        type = "l",
        xlab = "Iteration",
        ylab = "Value"
      )
      abline(h = min, col = "blue", lty = 2)
      abline(h = max, col = "red", lty = 2)
      legend(
        length(z) * 0.79,
        max,
        c("Minimum", "Maximum"),
        lwd = c(1, 1),
        lty = c(2, 2),
        col = c("blue", "red")
      )
      if (addtitle == TRUE) {
        title(main = "Trace Plot", col.main = "black")
      }

    }
  } else {
    stop(
      paste0(
        'Please ensure the MCMC object "x" type is one of "mcmc", "data.frame",
             or "multi_net_output"'
      )
    )
  }

}
