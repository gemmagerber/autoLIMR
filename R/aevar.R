#' @title Function aevar() : assimilation efficiency definition
#' @description Defines assimilation efficiency variables if included in network input data workbook sheets
#' @param x network data input matrix
#' @param respiration If respiration = TRUE in main autoLIMR argument
#'
#'
# aevar <- function(x, respiration) {
#   # Search for columns with match
#   x <- as.data.frame(x)
#   ae.search <- search_cols(x, col.match = "AE")
#
#   # Extract only columns with AE from input data
#   aemat <- x[, colnames(ae.search), drop = T]
#   ae.mat <-
#     aemat[grep("NLNode", rownames(aemat), invert = TRUE), , drop = FALSE]
#   w.ae <- names(which(rowSums(is.na(ae.mat)) != ncol(ae.mat)))
#
#   if (length(w.ae) > 0) {
#     # Function: Define AE variables only for nodes that have AE inequalities
#     aevar_w_ae <- function(x, respiration) {
#       w.ae_uvar <- paste0(
#         x,
#         "_AE = ",
#         x,
#         "_P"
#       )
#
#       if (respiration == TRUE) {
#         w.ae_uvar <- paste0(
#           x,
#           "_AE = ",
#           x,
#           "_P + ",
#           x,
#           "_R"
#         )
#       }
#       return(w.ae_uvar)
#     }
#
#     ae_var <- aevar_w_ae(w.ae, respiration = respiration)
#     ae_var <-
#       c(
#         "! Assimilation Efficiency (AE) Variables",
#         "",
#         sort(ae_var)
#       )
#   } else {
#     ae_var <- c("! No Assimilation Efficiency (AE) Variables defined")
#   }
# }
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
    aemat <- living.mat[, grep(pattern = aecols, colnames(living.mat), invert = FALSE), drop = FALSE] # Drop all columns that are not AE columns
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
