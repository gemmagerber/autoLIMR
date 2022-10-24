#' Function uvar()
#' Unused material/energy variable definition
#'
#' @inheritParams uvar_wo_ex
#' @inheritParams uvar_w_ex
#' @export
#'
uvar <- function(x, respiration) {
  # Search network input data for columns that match "Export" or similar
  search.ex <- search_cols(x, col.match = "Export")

  # Create matrix that only contains export columns and compartment names
  exmat <- x[, grep(
    pattern =
      paste0(search.ex, collapse = "|"), colnames(x)
  ), drop = T]

  # Remove rows containing NLNodes
  ex.mat <-
    exmat[grep("NLNode", rownames(exmat), invert = T), , drop = F]

  # Select which compartments have no export (no values in EX_lower or EX_upper)
  wo.ex <- names(which(rowSums(is.na(ex.mat)) == ncol(ex.mat)))

  # Select which compartments have exports (values in EX_lower or EX_upper)
  w.ex <- names(which(rowSums(is.na(ex.mat)) != ncol(ex.mat)))

  if (length(wo.ex) > 0 & length(w.ex) > 0) {
    wo.ex_uvar <- uvar_wo_ex(wo.ex, respiration = respiration)
    w.ex_uvar <- uvar_w_ex(w.ex, respiration = respiration)
    var <- c(wo.ex_uvar, w.ex_uvar)
  }

  if (identical(wo.ex, character(0)) & length(w.ex) > 0) {
    var <- uvar_w_ex(w.ex, respiration = respiration)
  }

  if (length(wo.ex) > 0 & identical(w.ex, character(0))) {
    var <- uvar_wo_ex(wo.ex, respiration = respiration)
  }

  if (length(var) > 0) {
    u_var <-
      c(
        "! Unused Energy/Material (U) Variables",
        "",
        sort(var),
        ""
      )
  } else {
    u_var <-
      c(
        "",
        "! No Unused Energy/Material (U) Variables defined",
        ""
      )
  }
}
