#' @title pvar()
#' @description Production variable definition.
#' Defines production variables for living compartments only.
#' Depends on exports and arguments NLNode and respiration.
#' @inheritParams autoGen
#' @param x tidy network input data matrix


pvar <- function(x, respiration, NLNode) {
  # Define all living nodes
  living <-
    x[grep("NLNode", rownames(x), invert = TRUE), , drop = FALSE]
  ln <- rownames(living)

  # Define all living things with exports
  ex.mat <- matrix_def(x, mat.type = "Export")
  ln.ex <-
    ex.mat[grep("NLNode", rownames(ex.mat), invert = TRUE), , drop = FALSE]
  ln.ex <- rownames(ln.ex)

  # Define all P variables
  if (respiration == TRUE & length(NLNode) > 0) {
    variable <- ifelse(
      ln %in% ln.ex,
      paste0(ln, "_P = Flowfrom(", ln, ") - ", ln, "_R - ", ln, "_U - ", ln, "_EX"),
      paste0(ln, "_P = Flowfrom(", ln, ") - ", ln, "_R - ", ln, "_U")
    )
  }

  if (respiration == TRUE & length(NLNode) == 0) {
    variable <- ifelse(
      ln %in% ln.ex,
      paste0(ln, "_P = Flowfrom(", ln, ") - ", ln, "_R - ", ln, "_EX"),
      paste0(ln, "_P = Flowfrom(", ln, ") - ", ln, "_R - ")
    )
  }

  if (respiration == FALSE & length(NLNode) > 0) {
    variable <- ifelse(
      ln %in% ln.ex,
      paste0(ln, "_P = Flowfrom(", ln, ") - ", ln, "_U - ", ln, "_EX"),
      paste0(ln, "_P = Flowfrom(", ln, ") - ", ln, "_U")
    )
  }

  if (respiration == FALSE & length(NLNode) == 0) {
    variable <- ifelse(
      ln %in% ln.ex,
      paste0(ln, "_P = Flowfrom(", ln, ") - ", ln, "_EX"),
      paste0(ln, "_P = Flowfrom(", ln, ")")
    )
  }

  if (length(variable) > 1) {
    p_var <- c("! Production (P/NPP) Variables", "", sort(variable), "")
  } else {
    p_var <- c("", "! No Production Variables (U) defined", "")
  }
  return(p_var)
}
