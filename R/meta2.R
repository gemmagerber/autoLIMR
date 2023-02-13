#' @title Function: meta2()
#' @description Defines metadata from adjacency matrices with abbreviations
#' @param x Adjacency matrix
#'
meta2 <- function(x) {
  internals <- paste0("! Internal flows: ", sum(!is.na(x)))
  metadata2 <- c(
    internals,
    "",
    "! Abbreviations",
    "! GPP = Gross Primary Production (Primary Producers only)",
    "! Q = Consumption",
    "! NPP = Net Primary Production (Primary Producers only)",
    "! P = Production",
    "! R = respiration",
    "! U = Passive flows to non-living compartments/Unassimilated material",
    "! AE = Assimilation Efficiency",
    "! IM = Import flow",
    "! EX = Export Flow",
    "! NLNode = Non-living compartment",
    ""
  )
  return(metadata2)
}
