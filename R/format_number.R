#' Format numbers without scientific notation
#'
#' @param x Numeric value to format
#' @param digits Number of decimal places to show
#' @return Character string with formatted number
#' @noRd
format_number <- function(x, digits = 6) {
  if (is.numeric(x)) {
    # Format with fixed notation (no scientific)
    return(format(x, scientific = FALSE, digits = digits))
  } else {
    return(as.character(x))
  }
}
