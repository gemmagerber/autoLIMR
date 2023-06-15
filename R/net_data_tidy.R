#' @title net_data_tidy()
#' @description Tidies up network data input sheet
#' @param x Network input data sheet
#' @inheritParams autoGen
#' @inheritParams sci_notation_off
#' @return Data frame of tidy input data. Tidy data includes adding 'NLNode'
#' strings to non-living nodes, replacing spaces " " with a period "."
#' in compartment names, switching off scientific notation, removing all
#' compartments with a biomass of  <= 0
#'
net_data_tidy <- function(x, NLNode) {
  x <- as.matrix(x)
  rownames(x) <- x[, 1] # Make Compartment Name the Row Name
  x <- x[, -1] # Make Compartment Name the Row Name
  x <- NLNode_mat(x, NLNode = NLNode)

  x <-
    apply(
      X = x,
      MARGIN = 2,
      FUN = gsub,
      pattern = ",",
      replace = "."
    ) # Substitute commas for periods
  x <-
    x[!x[, "Biomass"] == "0" |
      !x[, "Biomass"] == 0, ] # Drop any rows that have biomass of zero or NA
  x <-
    x[!is.na(x[, "Biomass"]), ]

  # x <- sci_notation_off(x)
  x <-
    apply(
      X = x,
      MARGIN = 2,
      FUN = gsub,
      pattern = " ",
      replace = ""
    ) # Substitute spaces for nothing
  as.data.frame(x)
  return(x)
}
