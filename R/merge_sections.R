#' @title Function: merge_sections()
#' @description Merge lists together, and names sections for LIM declaration file
#' @param type type of section
#' @param ... other arguments
#' @importFrom stats setNames
#' @return list of LIM file sections
merge_sections <- function(type = NULL, ...) {
  l <- list(...)
  keys <- unique(unlist(lapply(l, names)))
  x <- stats::setNames(do.call(mapply, c(FUN = c, lapply(l, `[`, keys))), keys)

  if (!is.null(type)) {
    x <- lapply(
      x,
      function(x) {
        c(
          paste0("### ", toupper(type)),
          "",
          x,
          "",
          paste0("### END ", toupper(type)),
          ""
        )
      }
    )
  }
  return(x)
}
