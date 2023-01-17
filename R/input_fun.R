#' input_fun: part of multinets function
#' Grabs input nodes (only boundary flows in)
#'
#' @param x an object?
#'
input_fun <- function(x) {
  input.vector <-
    as.vector(colSums(x[c(grep(
      rownames(x),
      pattern = "Import|CO2",
      value = TRUE,
      invert = FALSE
    )), c(
      grep(
        colnames(x),
        pattern = "Import|Export|CO2",
        value = TRUE,
        invert = TRUE,
        ignore.case = TRUE
      )
    )], na.rm = FALSE))
}