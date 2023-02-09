#' @title multi_net(): function for calculating multiple plausible networks
#'
#' @description This function calculates multiple plasuible network solutions
#' from a LIM declaration file.
#' Based on LIM and limSolve packages with novel extras!
#' Includes options to change 1) starting points (x0), 2) jump sizes, and 3)
#' number of iterations. A further option, 'pack' is to pack the solved values
#' into network objects for network visualistion and analysis with additional
#' network manipulation packages (see igraph, enaR, etc.)
#'

#' @param pack Logical. Default = FALSE. If pack = TRUE, the function returns
#' the flow values packed into network objects.
#' @inheritParams check_build
#' @inheritParams prepack_fun
#' @inheritParams defaultx0
#' @inheritParams centralx0
#' @inheritParams as_extended
#' @inheritParams ssCheck
#'
#' @return A list of model outputs containing 1) LIM Declaration File, 2)
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
    full_limfile <- check_build(file = file)

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
        solved.2 = list(packed.nets = packed.nets,
                        balanced = balanced)
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
