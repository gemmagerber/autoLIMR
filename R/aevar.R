#' @title aevar()
#' @description Defines assimilation efficiency variables if included
#' in network input data files
#' @param x network data input matrix
#' @inheritParams autoGen
#' @return Vector containing assimilation efficiencies

aevar <- function(x, respiration) {
  x <- as.matrix(x)
  ae_search <- search_cols(x, col_match = "AE")

  if (length(ae_search) > 0) {
    # Define all living nodes
    living <- x[grep("NLNode", rownames(x), invert = TRUE), , drop = FALSE]
    ln <- rownames(living)
    ln_pattern <- paste0(ln, collapse = "|")
    living_mat <- x[grep(pattern = ln_pattern, rownames(x),
                         invert = FALSE), , drop = FALSE]

    # Search and extract living node names that also have AE
    # Find columns that match AE
    aecols <- paste0(ae_search, collapse = "|")
    # Drop all columns that are not AE columns
    aemat <- living_mat[, grep(pattern = aecols,
                               colnames(living_mat),
                               invert = FALSE), drop = FALSE]
    # Extract names of compartments with AE
    with_ae <- names(which(rowSums(is.na(aemat)) != ncol(aemat)))

    # Define AE variables only for compartments with AE
    if (length(with_ae) > 0) {
      if (respiration == TRUE || is.null(respiration)) {
        aevars <- paste0(with_ae, "_AE = ", with_ae, "_P + ", with_ae, "_R")
        sort(aevars)
      } else {
        aevars <- paste0(with_ae, "_AE = ", with_ae, "_P")
        sort(aevars)
      }

      aevars <- c("! Assimilation Efficiency (AE) variables", "", aevars)
    } else if (length(with_ae) == 0) {
      aevars <- "! No Assimilation Efficiency (AE) Variables defined"
    }

    return(aevars)
  }
}
