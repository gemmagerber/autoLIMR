#' @title Function: net_data_external_list()
#' @description define externals list for LIM declaration file
#' @param x net data input matrix/matrices
#' @inheritParams autoGen
#' @return Vector of external compartments
#'
net_data_external_list <- function(x, respiration,
                                   respiration_element) {
  # Define all with Imports
  import.mat <- matrix_def(x, mat.type = "Import")
  im <- rownames(import.mat)

  # Define all with exports
  ex.mat <- matrix_def(x, mat.type = "Export")
  ex <- rownames(ex.mat)

  exports <- paste0(ex, "Export")
  imports <- paste0(im, "Import")


  if (respiration == TRUE) {
    if (!is.null(respiration_element)) {
      resp.vec <- as.vector(paste0(toupper(respiration_element)))
    } else {
      resp.vec <- as.vector(paste0("CO2"))
    }
  } else {
    resp.vec <- as.vector(paste0(""))
  }

  returnme <-
    c(
      resp.vec,
      sort(imports),
      sort(exports)
    )
  return(returnme)
}
