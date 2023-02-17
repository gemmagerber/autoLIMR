#' @title living_fun() part of prepack_fun() function
#' @description Grabs vector of living nodes
#'
#' @param x Solved full flow matrix
#'
living_fun <- function(x) {
  row.i <-
    grep(
      rownames(x),
      pattern = "Import|Export|CO2",
      value = TRUE,
      invert = TRUE
    )
  living.vector <-
    !grepl(row.i, pattern = "NLNode", ignore.case = TRUE)
}
