#' @title Function qvar()
#' @description Defines consumption variables for living nodes only.
#' Depends on imports. For living compartments without imports, consumption
#' is defined as `compartment_Q = Flowto(compartment)`. For compartments with
#' imports, consumption is defined as `compartment_Q = Flowto(compartment) -
#' compartment_IM`.
#'
#' @param x tidy network input data matrix

qvar <- function(x) {
  # Define all living nodes
  living <-
    x[grep("NLNode", rownames(x), invert = TRUE), , drop = FALSE]
  ln <- rownames(living)

  # Define all living things with Imports
  import.mat <- matrix_def(x, mat.type = "Import")
  ln.im <-
    import.mat[grep("NLNode", rownames(import.mat), invert = TRUE), , drop = FALSE]
  ln.im <- rownames(ln.im)

  # Define all Q variables
  variables <- ifelse(
    ln %in% ln.im,
    paste0(ln, "_Q = Flowto(", ln, ") - ", ln, "_IM"),
    paste0(ln, "_Q = Flowto(", ln, ")")
  )

  qvar <-
    c(
      "! Consumption (Q) / Gross Primary Production (GPP) Variables",
      "",
      sort(variables),
      ""
    )
  return(qvar)
}
