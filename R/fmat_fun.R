#' @title function fmat_fun()
#' @description Part of \code{pre_pack()} function in \code{multi_net()} function
#' Extracts flow matrices (only internal matrices excluding boundary flows)
#' @inheritParams living_fun
#' @return A matrix of internal flows
#' @keywords internal
#'
fmat_fun <- function(x) {

  # Debug input (only if required - for testing)
  # message("fmat_fun input dimensions: ", nrow(x), "x", ncol(x))
  # message("fmat_fun input rownames: ", paste(head(rownames(x)), collapse=", "))
  # message("fmat_fun input colnames: ", paste(head(colnames(x)), collapse=", "))

  # Get indices of rows/columns to keep (not Import/Export/CO2)
  keep_rows <- grep(
    rownames(x),
    pattern = "Import|Export|CO2",
    value = FALSE,  # Return indices, not values
    invert = TRUE,
    ignore.case = TRUE
  )

  keep_cols <- grep(
    colnames(x),
    pattern = "Import|Export|CO2",
    value = FALSE,  # Return indices, not values
    invert = TRUE,
    ignore.case = TRUE
  )

  # Debug message (only if required - for testing)
  # message("Keeping ", length(keep_rows), " rows and ", length(keep_cols), " columns")

  # Debug message (only if required - for testing)
  # Check if we have rows and columns to keep
  # if (length(keep_rows) == 0 || length(keep_cols) == 0) {
  #   warning("No rows or columns match the filtering criteria in fmat_fun")
  #   return(matrix(0, nrow=0, ncol=0))
  # }

  # Extract the flow matrix
  flow_matrix <- x[keep_rows, keep_cols, drop = FALSE]

  # Debug output (only if required - for testing)
  # message("Flow matrix dimensions: ", nrow(flow_matrix), "x", ncol(flow_matrix))
  # message("Flow matrix has ", sum(flow_matrix > 0), " positive values")

  # Return the flow matrix
  return(flow_matrix)
}
