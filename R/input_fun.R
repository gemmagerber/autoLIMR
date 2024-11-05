#' @title input_fun()
#' @description Part of \code{prepack_fun()} function.
#' Grabs input nodes values (only boundary flows in) from solved flow matrices.
#' @return A matrix of imports
#' @param x the input matrix
input_fun <- function(x) {

  col.j <-
    grep(
      colnames(x),
      pattern = "Input|Import|Export|CO2",
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )

  # Make data.frame in case R drop a rowname somewhere
  x <- as.data.frame(x)
  # Subset input compartments from row.i (Imports + CO2 for primary producer GPP)
  input.vec <- x[rownames(x) %in% c("*Import*", "CO2"), ]

  # Subset all internal compartments from col.j
  input.vec <- input.vec[, !colnames(input.vec) %in% col.j]

  # Sum all inputs (in case of multiple rows)
  input.vec <- as.vector(colSums(input.vec, na.rm = FALSE))

return(input.vec)
}
