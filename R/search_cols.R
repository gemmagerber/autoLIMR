#' @title search_cols()
#' @description Search and return columns that match type (as character vector)
#' "Imports", "Exports", "Assimilation Efficiency"
#' @param x Tidy network data input matrix
#' @param col_match the compartment type to match
#' @return A vector of column names that match one of "Imports", "Exports",
#'  "Assimilation Efficiency", "Custom" or "Parameters" from the original
#'  network input data.
search_cols <- function(x, col_match) {
  if (col_match == "Import") {
    x <- grep(
      as.vector(colnames(x)),
      pattern = paste0(
        c(
          "*Import*", "Input*", "^+In+$", "^+IN+$",
          "IN_", "IM_", "^+IM+$", "^+Im+$"
        ),
        collapse = "|"
      ),
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
    return(x)
  }

  if (col_match == "Export") {
    x <- grep(
      colnames(x),
      pattern = paste0(c("Export*", "Ex", "EX", "EX_", "_ex"),
        collapse = "|"
      ),
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
    return(x)
  }

  if (col_match == "AE") {
    x <- grep(
      as.vector(colnames(x)),
      pattern = paste0(
        c(
          "AE", "Assimilation", "efficiency", "AssEm", "ae_"
        ),
        collapse = "|"
      ),
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
    return(x)
  }

  if (col_match == "Custom") {
    x <- grep(
      colnames(x),
      pattern = paste0(c("Custom"),
        collapse = "|"
      ),
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
    return(x)
  }

  if (col_match == "Parameters") {
    x <- grep(
      colnames(x),
      pattern = paste0(c("Parameters|Parameters"),
        collapse = "|"
      ),
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
    return(x)
  }
}
