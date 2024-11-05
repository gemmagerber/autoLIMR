#' @title search_cols()
#' @description Search and return columns that match type (as character vector)
#' "Imports", "Exports", "Assimilation Efficiency"
#' @param x Tidy network data input matrix
#' @param col.match the compartment type to match
#' @return A vector of column names that match one of "Imports", "Exports",
#'  "Assimilation Efficiencies", "Custom" or "Parameters" from the original
#'  network input data.
search_cols <- function(x, col.match) {
  if (col.match == "Import") {
    x <- grep(
      as.vector(colnames(x)),
      pattern = paste0(
        c(
          "*Import*", "Input*", "^+In+$", "^+IN+$", "IN_", "IM_", "^+IM+$", "^+Im+$"
        ),
        collapse = "|"
      ),
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
    return(x)
  }

  if (col.match == "Export") {
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

  if (col.match == "AE") {
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

  if (col.match == "Custom") {
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

  if (col.match == "Parameters") {
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
