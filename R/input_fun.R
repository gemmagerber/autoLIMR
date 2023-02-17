#' @title input_fun() function, part of prepack_fun() function
#' @description Grabs input nodes values (only boundary flows in)
#'
#' @param x the input matrix
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
