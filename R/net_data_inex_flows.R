#' @title net_data_inex_flows()
#' @description Defines import and export flows from network input data.
#' @param x network data input matrix
#' @return Vector of boundary flows - import and export flows.

net_data_inex_flows <- function(x) {

  # Define import flows, where applicable
  import.mat <- matrix_def(x, mat.type = "Import")

  # Check whether the import matrix has any data or not before translating
  if(nrow(import.mat) > 0) {
    y <- c(
      "! Import flows",
      "",
        paste0(
          rownames(import.mat),
          "_IM: ",
          rownames(import.mat),
          "Import",
          " -> ",
          rownames(import.mat)
        )
      )

  } else {
    y <- as.vector(paste0("! No import flows")) # If no import data, no flows are defined
  }

  # Define export flows, where applicable
  ex.mat <- matrix_def(x, mat.type = "Export")
  if(nrow(ex.mat) > 0) {
    z <- c("",
           "! Export flows",
           "",
             paste0(
             rownames(ex.mat),
             "_EX: ",
             rownames(ex.mat),
             " -> ",
             rownames(ex.mat),
             "Export"
           ),
           "")
  } else {
    exports <- as.vector(paste0("! No export flows"))
  }

  a <- c(y, z)

  return(a)
}
