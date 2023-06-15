#' @title resp_fun()
#' @description Part of \code{pre_pack()} function in \code{multi_net} function.
#' Grabs values of internal nodes that respire.
#'
#' @param x The flow matrix
#' @return Vector of living compartments that respire
#'
resp_fun <- function(x) {
  resp.vector <-
    as.vector(x[c(grep(
      rownames(x),
      pattern = "Import|Export|CO2",
      value = TRUE,
      invert = TRUE
    )), c(grep(
      colnames(x),
      pattern = "CO2",
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    ))])
}
