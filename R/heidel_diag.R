#' @title heidel_diag(): Heidelberger and Welch’s convergence diagnostic
#'
#' @description Function to calculate Heidelberger and Welch’s
#' convergence diagnostic for autoLIMR Markov chains.
#' Based on coda::heidel.diag() function (Plummer et al., 2020).
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
#' @param ... Other arguments from coda::heidel.diag()
#' @inheritParams coda::heidel.diag
#'
#' @return Data frame with each row corresponding to one Markov chain variable,
#' and columns describing Heidelberger-Welch diagnostics. Includes
#' 1) if the variable passes the Stationarity test ("HW.Stationarity.Test"),
#' 2) starting iteration ("HW.Start.Iteration"),
#' 3) p-value ("HW.p.value"),
#' 4) halfwidth test evaluation ("HW.Halfwidth.Test"),
#' 5) halfwidth mean ("HW.Mean"), and
#' 6) halfwidth value ("HW.Halfwidth.Value").
#'
#' @export
#' @importFrom coda as.mcmc heidel.diag
#' @examples
#' # Heidelberger and Welch’s convergence diagnostic of
#' # variables in object "multi_net_output"
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
#' heidel_diag(x = x)
#' heidel_diag(x = x, flow = "Plant_GPP")
#'
heidel_diag <- function(x, flow = NULL, eps = 0.1, pvalue = 0.05, ...) {
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

    heidel <- coda::heidel.diag(x = z, eps = eps, pvalue = pvalue)
    HW.mat <- as.data.frame.matrix(heidel)
    HW.mat$stest[HW.mat$stest == 1] <-
      "Passed" # Rename stest "1" as "Passed"
    HW.mat$stest[HW.mat$stest == 0] <-
      "Failed" # Rename stest "0" as "Failed"
    HW.mat$htest[HW.mat$htest == 1] <-
      "Passed" # Rename htest "1" as "Passed"
    HW.mat$htest[HW.mat$htest == 0] <-
      "Failed" # Rename htest "0" as "Failed"
    colnames(HW.mat) <- c(
      "HW.Stationarity.Test",
      "HW.Start.Iteration",
      "HW.p.value",
      "HW.Halfwidth.Test",
      "HW.Mean",
      "HW.Halfwidth.Value"
    ) # Rename columns for easier identification

    if (is.null(flow)) {
      return(HW.mat)
    } else {
      flow1 <- as.data.frame(HW.mat[flow, , drop = FALSE])
      return(flow1)
    }
  }
}
