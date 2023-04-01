#' @title Function: search_cols()
#' @Description Search and return columns that match type (as character vector)
#' "Imports", "Exports", "Assimilation Efficieny"
#' @param x Tidy network data input matrix
#' @param col.match the compartment type to match
#' @return
search_cols <- function(x, col.match) {
  if (col.match == "Import") {
    x <- grep(
      as.vector(colnames(x)),
      pattern = paste0(
        c(
          "Import", "Input", "^+In+$", "^+IN+$", "IN_", "IM_", "^+IM+$", "^+Im+$"
        ),
        collapse = "|"
      ),
      value = TRUE,
      invert = FALSE,
      ignore.case = FALSE
    )
    return(x)
  }

  if (col.match == "Export") {
    x <- grep(
      colnames(x),
      pattern = paste0(c("Export", "Exports", "Ex", "EX", "EX_"),
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
          "AE", "Assimilation", "efficiency", "AssEm"
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
