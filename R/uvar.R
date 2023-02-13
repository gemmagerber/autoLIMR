#' @title Function uvar()
#' @description Defines unused material/energy variable for living compartments.
#' Depends on exports and arguments NLNode and respiration.
#' @inheritParams autoGen
#' @param x tidy network input data matrix


uvar <- function(x, respiration, NLNode) {
  if (length(NLNode) > 0) {
    # Define all living nodes
    living <-
      x[grep("NLNode", rownames(x), invert = TRUE), , drop = FALSE]
    ln <- rownames(living)

    # Define all living things with exports
    ex.mat <- matrix_def(x, mat.type = "Export")
    ln.ex <-
      ex.mat[grep("NLNode", rownames(ex.mat), invert = TRUE), , drop = FALSE]
    ln.ex <- rownames(ln.ex)

    # Define all U variables
    if (respiration == TRUE) {
      variable <- ifelse(
        ln %in% ln.ex,
        paste0(ln, "_U = Flowto(", ln, ") - ", ln, "_P - ", ln, "_R - ", ln, "_EX"),
        paste0(ln, "_U = Flowto(", ln, ") - ", ln, "_P - ", ln, "_R")
      )

    }

    if (respiration == FALSE) {
      variable <- ifelse(
        ln %in% ln.ex,
        paste0(ln, "_U = Flowto(", ln, ") - ", ln, "_P - ", ln, "_EX"),
        paste0(ln, "_U = Flowto(", ln, ") - ", ln, "_P")
      )

    }


  }  else {
    variable <- c("! No Unused Energy/Material Variables defined", "")
  }

  if (length(variable) > 1) {
    u_var <-
      c("! Unused Energy/Material (U) Variables",
        "",
        sort(variable),
        "")
  } else {
    u_var <- variable
  }
  return(u_var)

}
