#' Function uvar_w_ex()
#' Unused material/energy variable definition without exports
#'
#' @param x matrices defined from Network Data Input workbook
#' @param respiration If respiration = TRUE in main autoLIMR argument
#' @export
#'
uvar_w_ex <- function(x, respiration) {
  if (!is.null(respiration) | respiration == FALSE) {
    w.ex_uvar <- paste0(
      x,
      "_U = ",
      "Flowto(",
      x,
      ") - ",
      x,
      "_P - ",
      x,
      "_EX"
    )
  }

  if (respiration == TRUE) {
    w.ex_uvar <- paste0(
      x,
      "_U = ",
      "Flowto(",
      x,
      ") - ",
      x,
      "_P - ",
      x,
      "_R - ",
      x,
      "_EX"
    )
  }
  return(w.ex_uvar)
}
