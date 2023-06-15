#' @title ssCheck
#' @description A brief description goes here
#'
#' @param x network object
#' @param tol tolerance
#' @param more not sure
#' @param zero.na convert NA to zero
#' @return Are network objects balanced? Logical.

#' @importFrom network network.size
#'
ssCheck <- function(x,
                    tol = 5,
                    more = FALSE,
                    zero.na = TRUE) {
  if (!requireNamespace("network", quietly = TRUE)) {
    stop(
      "Package \"network\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Check for network class object
  if (inherits(x, "network") == FALSE) {
    warning("x is not a network class object")
  }
  T. <- as_extended(x) # convert to extended format
  if (zero.na) {
    T.[is.na(T.)] <- 0
  }
  n <- network::network.size(x) # get the number of nodes
  Tin <- apply(T.[, 1:n], 2, sum) # in throughflow
  Tout <- apply(T.[1:n, ], 1, sum) # out throughflow
  d <- abs(Tin - Tout) # SSerror difference
  pe <- (d / Tout) * 100 # SSerror as percent of total throughflow

  if (more == FALSE) {
    return(all(pe < tol)) # returns a logical indicating that all node differences are less than tolerance (==TRUE)
  } else {
    return(list(
      "ss" = all(pe < tol),
      "Tin" = Tin,
      "Tout" = Tout,
      "perror" = pe
    ))
  }
}
