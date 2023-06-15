#' @title output_fun()
#' @description part of \code{prepack_fun()} function. Grabs output
#' @param x the flow matrix
#' @return A vector of outputs (imports, exports, plus respiration)
#'
output_fun <- function(x) {
  # Create output vector function
  row.i <-
    grep(
      rownames(x),
      pattern = "Import|Input|Export|CO2",
      value = TRUE,
      invert = TRUE
    )
  col.j <-
    grep(
      colnames(x),
      pattern = "Input|Import|Export|CO2",
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
  output.vector <-
    as.vector(colSums(t(x[c(row.i), c(col.j)]), na.rm = FALSE))
}
