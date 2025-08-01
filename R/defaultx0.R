#' @title function defaultx0()
#' @description Function to solve multiple plausible networks with Markov
#' Chain Monte Carlo methods, with starting solution calculated by
#' Least Squares with Equalities and
#' Inequalities (LSEI), suitable
#' for under determined LIM problems.
#'
#' @param full_lim LIM Declaration file built from \code{LIM_Read()}.
#' @inheritParams multi_net
#' @param ... Further LIM::Xsample arguments.
#' @return A list of multiple plausible network flow values. The first sample is
#' calculated with Least Squares with Equalities and Inequalities (LSEI).
#' @importFrom LIM Xsample Flowmatrix

defaultx0 <-
  function(full_lim,
           iter = NULL,
           jmp = NULL,
           x0 = NULL,
           ...) {
    if (!requireNamespace("LIM", quietly = TRUE)) {
      stop("Package \"LIM\" must be installed to use this function.",
        call. = FALSE
      )
    }

    message(
      strwrap(
        prefix = " \n",
        initial = "",
        "Initial solution (x0) calculated using default LSEI algorithm (Haskell and Hanson 1981).
    Solving multiple plausible network values (this may take a while)..."
      )
    )

    starting.solution.x0 <- "LSEI"

    print(system.time(
      solved.flow.values <- LIM::Xsample(
        lim = full_lim,
        x0 = x0,
        jmp = jmp,
        iter = iter,
        outputlength = iter
      )
    ))

    solved.flow.matrices <- list(NULL)

    for (i in 1:as.numeric(nrow(solved.flow.values))) {
      solved.flow.matrices[[i]] <-
        LIM::Flowmatrix(lim = full_lim, web = solved.flow.values[i, ])
    }

    solved.networks <- list(
      full_lim = full_lim,
      starting.solution.x0 = starting.solution.x0,
      solved.flow.values = solved.flow.values,
      solved.flow.matrices = solved.flow.matrices
    )

    # message(strwrap(
    #   prefix = " \n",
    #   initial = "",
    #   "Multiple plausible network values solved."
    # ))

    return(solved.networks)
  }
