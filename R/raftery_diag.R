#' @title raftery_diag()
#'
#' @description Function to calculate Raftery and Lewis’s
#' convergence diagnostic for autoLIMR Markov chains.
#' Based on \code{coda::raftery.diag()} function (Plummer et al., 2020).
#'
#' @details If the sample size (i.e., iterations) are too small, the
#' function returns an error. We suggest keeping the sample size > 4000.
#'
#' @param x Object of class "multi_net_output" generated from
#' \code{autoLIMR::multi_net()} function.
#'
#' @param flow Character argument. By default, flow = NULL, and the convergence
#' diagnostics are returned for each Markov Chain variable. If convergence
#' diagnostics are required for one Markov chain variable only,
#' it can be specified as a character string e.g., \code{flow = "Plant_GPP"}.
#' We suggest leaving the default \code{flow = NULL}.
#'
#' @inheritParams coda::raftery.diag
#'
#' @param ... Other arguments from \code{coda::raftery.diag()}. Check defaults in
#' the coda package.
#'
#' @importFrom coda as.mcmc raftery.diag
#'
#' @return Data frame with each row corresponding to one Markov chain variable,
#' and columns describing various Raftery Lewis convergence diagnostics:
#' 1) M,
#' 2) N,
#' 3) Nmin,
#' 4) I.
#' See coda package for further details.
#'
#' @export
#'
#' @examples
#' \donttest{
#' fpath <- system.file(
#' "example_limfiles",
#' "Winter_Weighted_Network_LIMfile.R",
#' package = "autoLIMR"
#' )
#'
#' set.seed(1)
#' x <- multi_net(
#'   file = fpath,
#'   iter = 4000,
#'   jmp = 0.0001,
#'   x0 = NULL
#' )
#' # raftery_diag(x = x)
#' raftery_diag(x = x, flow = "Plant_GPP")
#' }
#'
raftery_diag <- function(x,
                         flow = NULL,
                         q = 0.025,
                         r = 0.005,
                         s = 0.95,
                         converge.eps = 0.001,
                         ...) {
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

    # raftery-Lewis Diagnostic
    raft <- coda::raftery.diag(
      data = y,
      q = q,
      r = r,
      s = s,
      converge.eps = converge.eps
    )

    if ("Error" %in% raft[["resmatrix"]] == TRUE) {
      stop("Sample size too small . Please adjust > 4000")
    } else {
      RL.mat <- as.data.frame.matrix(raft[["resmatrix"]])
      # Rename columns for easier identification
      colnames(RL.mat) <- c(
        "RL.Burn.in.Length",
        "RL.Required.Sample.Size",
        "RL.Min.Sample.Size",
        "RL.Dependence.Factor"
      )
      RL.params <- as.data.frame(as.list(raft[["params"]]))
      colnames(RL.params) <-
        c(
          "RL.Margin.of.error.r",
          "RL.Probablity.s",
          "RL.Quantile.q"
        )
      RL.all <- cbind(RL.mat, RL.params)

      if (is.null(flow)) {
        return(RL.all)
      } else {
        flow1 <- as.data.frame(RL.all[flow, , drop = FALSE])
        return(flow1)
      }
    }
  }
}
