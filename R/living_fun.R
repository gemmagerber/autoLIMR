#' @title living_fun()
#' @description Part of \code{prepack_fun()} function. Grabs living compartments.
#' @return A vector of living compartments
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
