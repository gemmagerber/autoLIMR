#' @title function `defaultx0()`
#' @description: Solves MCMC objects using the default MCMC
#' parsimonious solution LSEI (LIM::Xsample) as a starting point.
#'
#' @param full_limfile LIM Declaration file built from check_build()
#' @param iter Default = NULL. Number of iterations. Default = NULL returns 3000
#' iterations of the MCMC samples.
#' @param jmp Default = NULL. If jmp = NULL, jump size is internally calculated.
#' @param x0 Default = NULL. The inital starting point. Defaults to LSEI
#'  (x0 = NULL), or can be changed to the Central solutions calcualted with
#'  LIM::Xranges (x0 = "central").
#' @param ... Further LIM::Xsample arguments.
#'
#' @return Multiple plausible network flow values. The first sample is
#' calculated with Least Squares with Equalities and Inequalities (LSEI).
#' @importFrom LIM Xsample Flowmatrix

defaultx0 <-
  function(full_limfile,
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
        lim = full_limfile,
        x0 = x0,
        jmp = jmp,
        iter = iter,
        outputlength = iter
      )
    ))

    solved.flow.matrices <- list(NULL)

    for (i in 1:as.numeric(nrow(solved.flow.values))) {
      solved.flow.matrices[[i]] <-
        LIM::Flowmatrix(lim = full_limfile, web = solved.flow.values[i, ])
    }

    solved.networks <- list(
      full_limfile = full_limfile,
      starting.solution.x0 = starting.solution.x0,
      solved.flow.values = solved.flow.values,
      solved.flow.matrices = solved.flow.matrices
    )

    message(strwrap(
      prefix = " \n",
      initial = "",
      "Multiple plausible network values solved."
    ))

    return(solved.networks)
  }
