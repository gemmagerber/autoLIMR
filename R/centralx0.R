#' @title centralx0(): function to solve MCMC objects using the central MCMC
#' @description solution (LIM::Xsample) as a starting point.
#' If central solution is not valid, defaults to LSEI
#'
#' @inheritParams defaultx0
#'
#' @return Multiple plausible networks, starting point CENTRAL SOLUTION

#' @importFrom LIM Xsample Flowmatrix
#'

centralx0 <-
  function(full_limfile,
           iter = NULL,
           jmp = NULL,
           x0 = "central",
           ...) {
    if (!requireNamespace("LIM", quietly = TRUE)) {
      stop("Package \"LIM\" must be installed to use this function.",
        call. = FALSE
      )
    }


    x0 <-
      LIM::Xranges(lim = full_limfile, central = TRUE, ispos = TRUE)[, "central"]

    starting.solution.x0 <- x0
    message(
      strwrap(
        prefix = " \n",
        initial = "",
        "Initial solution (x0) calculated using LIM::Xranges central solution."
      )
    )

    message(strwrap(
      prefix = " \n",
      initial = "",
      "Checking that the central solution is valid..."
    ))


    cen1 <- max(abs(full_limfile$A %*% x0 - full_limfile$B))
    cen2 <- min(full_limfile$G %*% x0 - full_limfile$H)

    if (!is.na(cen1) & !is.na(cen2) == TRUE) {
      message(
        strwrap(
          prefix = " \n",
          initial = "",
          "Success: Central solution is valid.
          Now solving multiple plausible network values (this may take a while)..."
        )
      )

      print(system.time(
        solved.flow.values <- LIM::Xsample(
          lim = full_limfile,
          x0 = x0,
          jmp = jmp,
          iter = iter,
          outputlength = iter,
          ...
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
    } else {
      if (is.na(cen1 | cen2) == TRUE) {
        message(
          strwrap(
            prefix = " \n",
            initial = "",
            "Central solution is not valid. Default starting algorithm initialised."
          )
        )

        solved.networks <- defaultx0(
          full_limfile = full_limfile,
          x0 = x0,
          jmp = jmp,
          iter = iter,
          outputlength = iter,
          ...
        )
      }
    }
  }
