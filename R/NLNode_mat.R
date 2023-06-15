#' @title NLNode_mat()
#' @description Append NLNode to non-living nodes in rows and columns of matrices
#' @param x the matrix/matrices
#' @param NLNode the defined NLNodes from the main autoLIMR argument
#' @return a matrix with 'NLNode' string attached to non-living compartments

NLNode_mat <- function(x, NLNode) {
  if (length(NLNode) > 0) {
    colnames(x) <- ifelse(colnames(x) %in% NLNode,
      paste0(
        NLNode[match(colnames(x), NLNode)],
        "NLNode"
      ),
      colnames(x)
    )
    rownames(x) <- ifelse(rownames(x) %in% NLNode,
      paste0(
        NLNode[match(rownames(x), NLNode)],
        "NLNode"
      ),
      rownames(x)
    )

    return(x)
  }
}
