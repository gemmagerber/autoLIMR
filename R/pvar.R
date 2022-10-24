#' Function pvar(): Production variable definition
#' Defines production variable based on many arguments
#' @inheritParams pvar_wo_ex
#' @inheritParams pvar_w_ex
#'
#' @export
#'
pvar <- function(x, respiration, NLNode) {

  # Search network input data for columns that match "Export" or similar
  search.ex <- search_cols(x, col.match = "Export")

  # Create matrix that only contains export columns and compartment names
  exmat <- x[, grep(pattern =
                      paste0(search.ex, collapse = "|"), colnames(x)), drop = TRUE]

  # Remove rows containing NLNodes
  ex.mat <-
    exmat[grep("NLNode", rownames(exmat), invert = TRUE), , drop = FALSE]

  # Select which compartments have no export (no values in EX_lower or EX_upper)
  wo.ex <- names(which(rowSums(is.na(ex.mat)) == ncol(ex.mat)))

  # Select which compartments have exports (values in EX_lower or EX_upper)
  w.ex <- names(which(rowSums(is.na(ex.mat)) != ncol(ex.mat)))

  # If there are compartments with exports, and some without
  if (length(wo.ex) > 0 & length(w.ex) > 0) {
    wo.ex_pvar <- pvar_wo_ex(wo.ex, respiration = respiration, NLNode = NLNode)
    w.ex_pvar <- pvar_w_ex(w.ex, respiration = respiration, NLNode = NLNode)
    var <- c(wo.ex_pvar, w.ex_pvar)
  }

  if (identical(wo.ex, character(0)) & length(w.ex) > 0) {
    var <- pvar_w_ex(w.ex, respiration = respiration, NLNode = NLNode)
  }

  if (length(wo.ex) > 0 & identical(w.ex, character(0))) {
    var <- pvar_wo_ex(wo.ex, respiration = respiration, NLNode = NLNode)
  }

  if (length(var) > 0) {
    p_var <- c("! Production (P/NPP) Variables", "", sort(var), "")
  } else {
    p_var <- c("", "! No Production Variables (U) defined", "")
  }
}
