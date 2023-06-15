#' @title function fmat_fun()
#' @description Part of \code{pre_pack()} function in \code{multi_net()} function
#' Extracts flow matrices (only internal matrices excluding boundary flows)
#' @inheritParams living_fun
#' @return A matrix of internal flows
#'
fmat_fun <- function(x) {
  flow.matrix <-
    x[c(
      grep(
        rownames(x),
        pattern = "Import|Export|CO2",
        value = TRUE,
        invert = TRUE,
        ignore.case = TRUE
      )
    ), c(
      grep(
        colnames(x),
        pattern = "Import|Export|CO2",
        value = TRUE,
        invert = TRUE,
        ignore.case = TRUE
      )
    )]
}
