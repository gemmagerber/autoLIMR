#' Function pp_true()
#' If primary producers are included in main autoLIMR function, this function changes Q to GPP, and P to NPP
#' @param x network input data matrix
#' @param primary_producer Primary producers defined in main autoLIMR function
#' @export
#'
pp_true <- function(x, primary_producer) {
  vec_x <- as.vector(x)

  ppq_pattern <-
    paste0(paste0(primary_producer, "_Q"), collapse = "|")
  ppq <- grep(vec_x, pattern = ppq_pattern)

  ppnpp_pattern <-
    paste0(paste0(primary_producer, "_P"), collapse = "|")
  ppnpp <- grep(vec_x, pattern = ppnpp_pattern)


  if (length(ppq) >= 1) {
    gsub(vec_x,
         pattern = ppq_pattern,
         replacement = paste0(primary_producer, "_GPP"))

  } else {
    if (length(ppq == 0)) {
      x
    } else {
      if (length(ppnpp) >= 1) {
        gsub(
          vec_x,
          pattern = ppnpp_pattern,
          replacement = paste0(primary_producer, "_NPP")
        )
      }
      else {
        if (length(ppnpp) == 0) {
          x
        }
      }
    }
  }
}
