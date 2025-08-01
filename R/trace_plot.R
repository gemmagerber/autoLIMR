#' @title trace_plot()
#' @description Function to plot an MCMC object variable over time (iterations)
#' Trace plots can be used to assess the mixing of the MCMC chain over time
#'
#' @param x The solved MCMC flow values in a data.frame, accessible
#' from the multi_net() generated list of model outputs ("output") e.g.,
#' output[["solved.flow.values"]]. Can also refer to the entire multi_net()
#' output list (class(output) == "multi_net_model).
#' @param flow Character argument. The specified name of the flow plot.
#'
#' @param xranges Logical. If \code{xranges = FALSE}, the default, the lines
#' indicaitng minimum and maximum are excluded from the plot.
#' If \code{xranges = TRUE}, the plot
#' includes horizontal lines in the trace plot indicating absolute maximum and
#' minimum values of the flow range calculated by \code{LIM::Xranges}.
#'
#' @param addtitle Logical argument, defaults to \code{FALSE}.
#' If \code{TRUE},
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
#' set.seed(1)
#' x <- data.frame(rnorm(1000, m = 0, s = 1))
#' colnames(x) <- "Value"
#' trace_plot(x = x, flow = "Value", xranges = FALSE)
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
#' trace_plot(x = x, flow = "Plant_GPP", xranges = TRUE)
#' trace_plot(x = x, flow = "Plant_GPP", xranges = FALSE)
trace_plot <- function(x, flow, xranges = FALSE, addtitle = FALSE, ...) {
  ### Errors
  # Error: MCMC object must be provided
  if (is.null(x)) {
    stop('Please provide the name of the MCMC object as a "data.frame", "mcmc", or "multi_net_output"')
  }

  # Error: Flow name must be provided
  if (is.null(flow)) {
    stop('Please provide the character string name of the flow in the MCMC object to plot, e.g., flow = "Plant_GPP".')
  }

  ### Handle different input types
  if (is.data.frame(x) || inherits(x, "mcmc")) {
    # Handle data.frame or mcmc objects
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
    # Handle multi_net_output objects
    all <- as.data.frame(x[["solved.flow.values"]])
    z <- as.data.frame(all[[paste0(flow)]])
    colnames(z) <- paste0(flow)
    z <- coda::as.mcmc(z)

    if (xranges == FALSE || is.null(xranges)) {
      # Simple plot without Xranges
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
    } else {
      # Plot with Xranges - with error handling
      min_val <- min(z[, paste0(flow)])
      max_val <- max(z[, paste0(flow)])

      # Try to get Xranges with error handling
      xr <- tryCatch({
        if (is.null(x[["full_limfile"]])) {
          stop("Missing 'full_limfile' element in input")
        }
        t(LIM::Xranges(x[["full_limfile"]]))
      }, error = function(e) {
        warning("Could not calculate Xranges: ", conditionMessage(e),
                ". Using min/max of data instead.")
        return(NULL)
      })

      # Use Xranges if available, otherwise use data min/max
      if (!is.null(xr)) {
        # Try to extract min/max values safely
        tryCatch({
          flow_min_idx <- grep(pattern = "min", x = rownames(xr))
          flow_col_idx <- grep(pattern = paste0(flow), x = colnames(xr))

          if (length(flow_min_idx) > 0 && length(flow_col_idx) > 0) {
            min_val <- xr[flow_min_idx, flow_col_idx]
            max_val <- xr[grep(pattern = "max", x = rownames(xr)), flow_col_idx]
          }
        }, error = function(e) {
          warning("Error extracting min/max from Xranges: ", conditionMessage(e))
        })
      }

      # Create the plot with calculated min/max
      plot(
        x = 1:length(z),
        y = z[, paste0(flow)],
        ylim = c(min_val, max_val),
        type = "l",
        xlab = "Iteration",
        ylab = "Value"
      )

      # Add reference lines and legend
      abline(h = min_val, col = "blue", lty = 2)
      abline(h = max_val, col = "red", lty = 2)

      # Safe legend placement
      legend_x <- max(1, length(z) * 0.79)
      legend_y <- max_val

      legend(
        legend_x,
        legend_y,
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
    stop('Please ensure the MCMC object "x" type is one of "mcmc", "data.frame", or "multi_net_output"')
  }
}
