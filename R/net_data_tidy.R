#' @title net_data_tidy()
#' @description Tidies up network data input sheet
#' @keywords internal
#' @param x Network input data sheet
#' @inheritParams autoGen
#' @inheritParams sci_notation_off
#' @return Data frame of tidy input data. Tidy data includes adding 'NLNode'
#' strings to non-living nodes, replacing spaces " " with a period "."
#' in compartment names, switching off scientific notation, removing all
#' compartments with a biomass of  <= 0
#'
net_data_tidy <- function(x, NLNode) {
  # Check for empty input
  if (is.null(x) || (is.data.frame(x) && nrow(x) == 0)) {
    message("Empty input data in net_data_tidy")
    return(data.frame())
  }

  # Convert to matrix safely
  x <- as.matrix(x)

  # Check matrix dimensions
  if (ncol(x) < 1) {
    message("Input matrix has no columns")
    return(data.frame())
  }

  # Set rownames and remove first column
  rownames(x) <- x[, 1]
  x <- x[, -1, drop = FALSE]  # drop=FALSE preserves matrix structure

  # Apply NLNode_mat function
  x <- NLNode_mat(x, NLNode = NLNode)

  # Only proceed if x still has dimensions
  if (is.null(x) || !is.matrix(x) || length(dim(x)) == 0) {
    message("NLNode_mat returned invalid data")
    return(data.frame())
  }

  # Replace commas with periods
  x <- apply(X = x, MARGIN = 2, FUN = gsub, pattern = ",", replace = ".")

  # Check if "Biomass" column exists before filtering
  if ("Biomass" %in% colnames(x)) {
    # Convert biomass to numeric for consistent comparison
    biomass_numeric <- suppressWarnings(as.numeric(x[, "Biomass"]))
    # Keep rows where biomass is not zero and not NA
    keep_rows <- !is.na(biomass_numeric) & biomass_numeric != 0
    x <- x[keep_rows, , drop = FALSE]
  }

  # Remove spaces
  x <- apply(X = x, MARGIN = 2, FUN = gsub, pattern = " ", replace = "")

  # Return as data frame
  return(as.data.frame(x))
}
