#' Function: search_cols()
#' search and return columns that match type (as character vector) "Inputs", "Exports", "Assimilation Efficieny"
#' @param x the matrix
#' @param col.match the node type to match
#' @export
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
