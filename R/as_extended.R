#' @title as_extended()
#' @description directly from package enaR
#' @param x the something to be defined
#' @param zero_na Change NA's to zero's
#'
#' @return Extended flow matrices

#' @importFrom network get.vertex.attribute
#'
as_extended <- function(x, zero_na = TRUE) {
  # Check for network class object
  if (inherits(x, "network") == FALSE) {
    warning("x is not a network class object")
  }

  "%v%" <- function(x, attrname) {
    network::get.vertex.attribute(x, attrname = attrname)
  }

  # unpack the data from the network object
  flow <- as.matrix(x, attrname = "flow")
  input <- x %v% "input"
  respiration <- x %v% "respiration"
  export <- x %v% "export"
  # recombine into the extended format
  import <- c(input, 0, 0, 0)
  x <- cbind(flow, export, respiration, rep(0, nrow(flow)))
  x <- rbind(x, rep(0, length(import)), rep(0, length(import)), import)
  # make NA values zero if zero_na == TRUE
  if (zero_na) {
    x[is.na(x)] <- 0
  }
  return(x)
}
