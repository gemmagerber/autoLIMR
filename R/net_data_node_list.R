#' @title net_data_node_list()
#' @description Defines the final compartment list with standing stock equalities
#' to include in the LIM declaration file. Orders living compartments in
#' alphabetical order, then moves non-living compartments (if applicable) to
#' the end of the list
#' @param x Tidy network data input matrix
#' @return A vector of compartments together with standing stock equalities.
#' Living compartments arranged in alphabetical order.
#' Non-living compartments (if applicable) are moved to the end of the list.
#'
net_data_node_list <- function(x) {
  x2 <- as.matrix(x[, "Biomass"])
  x2 <- as.vector(paste(rownames(x2), "=", x2))
  LN <- grep(x2,
    pattern = "NLNode",
    invert = TRUE,
    value = TRUE
  ) # grab living nodes
  NLN <- grep(x2,
    pattern = "NLNode",
    invert = FALSE,
    value = TRUE
  ) # grab nonliving nodes
  combined <-
    c(
      sort(LN),
      sort(NLN)
    ) # paste together so that NLN are at bottom of list
  return(combined)
}
