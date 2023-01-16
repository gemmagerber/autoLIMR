#' check_build(): function to check if an appropriate LIM Declaration file has
#' been provided

#' @param file R script (".R" extension) containing the LIM declaration file
#'
#' @importFrom LIM Setup
#'
check_build <- function(file) {
  if (is.null(file)) {
    stop("No LIM Declaration File provided. Please check.")
  }
  if (!is.null(file)) {
    full_limfile <- LIM::Setup(LIM::Read(file))
  }
}
