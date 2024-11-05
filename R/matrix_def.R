#' @title matrix_def()
#' @description Return matrix based on \code{search_cols()} function
#' @param x input matrix
#' @param mat.type the matrix type to export
#' @return a matrix of type 'Import', 'Export'
#'
matrix_def <- function(x, mat.type) {
  if (mat.type == "Import" | mat.type == "Export") {
    match <- search_cols(x, col.match = mat.type)
    mat <-
      x[c(rownames(x)), c(match)] # create input matrix based on imports/exports only
    mat <-
      mat[rowSums(is.na(mat)) != ncol(mat), ] # Drop rows where all are NA

    if (length(match) == 1) {
      mat2 <- mat[!mat[, 1] %in% c(0, "0", NA), , drop = FALSE]
    }

    if (length(match) == 2) {
      mat2 <- mat[!mat[, 1] %in% c(0, "0", NA) |
        !mat[, 2] %in% c(0, "0", NA), , drop = FALSE]
    }
    return(mat)
  }
}
