#' @title NLNode_mat()
#' @description Append NLNode to non-living nodes in rows and columns of matrices or data frames
#' @keywords internal
#' @param x the matrix/matrices or data frame
#' @param NLNode the defined NLNodes from the main autoLIMR argument
#' @return a matrix/data frame with 'NLNode' string attached to non-living compartments
#'
NLNode_mat <- function(x, NLNode) {
  # Return original input if NLNode is NULL or empty
  if (is.null(NLNode) || length(NLNode) == 0) {
    return(x)
  }

  # Convert data frame to matrix if needed
  was_data_frame <- is.data.frame(x)
  if (was_data_frame) {
    x <- as.matrix(x)
  }

  # Only modify if x has valid dimensions
  if (!is.null(x) && ncol(x) > 0 && nrow(x) > 0) {
    # Check if colnames and rownames exist
    if (!is.null(colnames(x))) {
      colnames(x) <- ifelse(colnames(x) %in% NLNode,
                            paste0(
                              NLNode[match(colnames(x), NLNode)],
                              "NLNode"
                            ),
                            colnames(x)
      )
    }

    if (!is.null(rownames(x))) {
      rownames(x) <- ifelse(rownames(x) %in% NLNode,
                            paste0(
                              NLNode[match(rownames(x), NLNode)],
                              "NLNode"
                            ),
                            rownames(x)
      )
    }
  }

  # Convert back to data frame if input was a data frame
  if (was_data_frame) {
    x <- as.data.frame(x)
  }

  return(x)
}
