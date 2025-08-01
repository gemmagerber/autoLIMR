#' @title Function: net_data_external_list()
#' @description define externals list for LIM declaration file
#' @param x net data input matrix/matrices
#' @inheritParams autoGen
#' @return Vector of external compartments
#'
net_data_external_list <- function(x, respiration,
                                   respiration_element) {
  # Define all with Imports
  import.mat <- matrix_def(x, mat_type = "Import")
  if (nrow(import.mat) > 0) {
    im <- rownames(import.mat)
    imports <- paste0(im, "Import")
  } else {
    imports <- as.vector(paste0(""))
  }

  # Define all compartments with exports
  ex.mat <- matrix_def(x, mat_type = "Export")
  if (nrow(ex.mat) > 0) {
    ex <- rownames(ex.mat)
    exports <- paste0(ex, "Export")
  } else {
    exports <- as.vector(paste0(""))
  }

  # Define Respiration compartment (CO2) if required
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

  returnme <- returnme[nzchar(returnme)] # remove empty strings
  return(returnme)
}
