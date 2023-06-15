#' @title multi_net(): function for calculating multiple plausible networks
#'
#' @description This function calculates multiple plausible network solutions
#' from a LIM declaration file.
#' Based on LIM and limSolve packages with novel extras!
#'
#' Includes options to change 1) starting points (\code{x0}),
#' 2) jump sizes (\code{jmp}), and 3)
#' number of iterations (\code{iter}).
#'
#' A further argument, \code{pack} is to pack the solved values
#' into network objects for network visualistion and analysis with additional
#' network manipulation packages (see igraph, enaR, etc.)
#'
#' @param file Either an object from the environment, with no quotation (e.g.,
#' \code{file = limfile)}, or a file from the working directory (
#' e.g., \code{file = 'my_limfile.R'}).
#' @param pack Logical. If \code{FALSE}, the default, solved flow values are
#' not packed into network objects. If \code{TRUE}, the flow values are packed
#' into network objects compatible with most network visualisation and
#' analyses packages.
#'
#' @param iter An integer defining the total number of iterations (samples)
#' of the Markov Chain. If \code{NULL}, the default, the number of iterations
#' is set at 3000. In our experience, we recommend setting a larger number of
#' iterations (> 10,000) to ensure adequate sampling.
#'
#' @param jmp An integer defining the jump size (loosely interpreted as the
#' 'distance' between iterative samples in the solution space).
#' If \code{NULL}, the default, the jump size is internally
#' calculated. If a value is provided, the value should be within the ranges of
#' the flow magnitudes.
#'
#' @param x0 A single string defining the starting solution algorithm.
#' If \code{NULL}, the default, the first solution of the Markov Chain is
#' solved with Least Squares with Equalities and Inequalities (LSEI).
#' If \code{"central"}, the first solution of the Markov Chain is solved with
#' the central solution as per \code{LIM:Xranges}.
#'
#' @return A list of model outputs of class 'multi_net_output' containing
#' 1) LIM Declaration File, 2)
#' a table of starting solution values, 3) table of solved flow values, 4)
#' solved flow values packed into matrices, and if pack = TRUE, 5) list of
#' packed network objects, and 6) list of balanced network objects.
#' @export
#'
#' @examples
#' \dontrun{
#' multi_net(file = file, iter = NULL, jmp = NULL, x0 = NULL, pack = FALSE)
#' }
#'
multi_net <-
  function(file = NULL,
           iter = NULL,
           jmp = NULL,
           x0 = NULL,
           pack = TRUE) {
    ############################### Check and build LIM Declaration File

    if (is.null(file)) {
      stop("No LIM Declaration File provided. Please check.")
    }
    if (!is.null(file)) {
      full_limfile <- LIM::Setup(LIM_Read(file))
    }

    ############################### Starting point choices
    # Choice 1: Default (x0 = NULL), or Choice 2: Central (x0 = "central")
    if (is.null(x0) |
      missing(x0)) {
      x0 <- NULL
      solved <-
        defaultx0(
          full_limfile = full_limfile,
          iter = iter,
          jmp = jmp,
          x0 = NULL
        )
    } else if (x0 == "central") {
      solved <-
        centralx0(
          full_limfile = full_limfile,
          iter = iter,
          jmp = jmp,
          x0 = "central"
        )
    }

    ############################### Packing function
    if (pack == FALSE) {
      class(solved) <- "multi_net_output"
      return(solved)
    } else {
      if (pack == TRUE) {
        starting.solution.x0 <- x0

        message(
          strwrap(
            prefix = " \n",
            initial = "",
            "Packing multiple plausible values into network objects..."
          )
        )

        packed.nets <-
          lapply(
            X = solved$solved.flow.matrices,
            FUN = prepack_fun,
            full_limfile = full_limfile
          )
        balanced <- lapply(X = packed.nets, FUN = ssCheck)
        solved.2 <- list(
          packed.nets = packed.nets,
          balanced = balanced
        )
        out <- append(solved, solved.2)

        message(
          strwrap(
            prefix = " \n",
            initial = "",
            "Multiple plausible values solved and packed into network objects."
          )
        )

        message("\n")
        class(out) <- "multi_net_output"
        return(out)
      }
    }
  }
