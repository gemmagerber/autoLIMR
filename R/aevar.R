#' @title aevar()
#' @description Defines assimilation efficiency variables if included
#' in network input data files
#' @param x network data input matrix
#' @inheritParams autoGen
#' @return Vector containing assimilation efficiencies

aevar <- function(x, respiration) {
  x <- as.matrix(x)
  ae.search <- search_cols(x, col.match = "AE")

  if (length(ae.search) > 0) {
    # Define all living nodes
    living <- x[grep("NLNode", rownames(x), invert = TRUE), , drop = FALSE]
    ln <- rownames(living)
    ln.pattern <- paste0(ln, collapse = "|")
    living.mat <- x[grep(pattern = ln.pattern, rownames(x), invert = FALSE), , drop = FALSE]

    # Search and extract living node names that also have AE
    aecols <- paste0(ae.search, collapse = "|") # Find columns that match AE
    aemat <- living.mat[, grep(
      pattern = aecols,
      colnames(living.mat), invert = FALSE
    ),
    drop = FALSE
    ] # Drop all columns that are not AE columns
    with.ae <- names(which(rowSums(is.na(aemat)) != ncol(aemat))) # Extract names of compartments with AE

    # Define AE variables only for compartments with AE
    if (length(with.ae) > 0) {
      if (respiration == TRUE | is.null(respiration)) {
        aevars <- paste0(with.ae, "_AE = ", with.ae, "_P + ", with.ae, "_R")
        sort(aevars)
      } else {
        aevars <- paste0(with.ae, "_AE = ", with.ae, "_P")
        sort(aevars)
      }

      aevars <- c("! Assimilation Efficiency (AE) variables", "", aevars)
    } else if (length(with.ae) == 0) {
      aevars <- "! No Assimilation Efficiency (AE) Variables defined"
    }

    return(aevars)
  }
}
