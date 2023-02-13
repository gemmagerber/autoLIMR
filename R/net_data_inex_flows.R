#' @title Function: net_data_inex_flows()
#' @description Defines import and export flows from network input data workbook
#' @param x network data input matrix

net_data_inex_flows <- function(x) {
  ex.mat2 <- matrix_def(x, mat.type = "Export")
  in.mat2 <- matrix_def(x, mat.type = "Import")

  x <- c(
    "! Import flows",
    "",
    sort(
      paste0(
        rownames(in.mat2),
        "_IM: ",
        rownames(in.mat2),
        "Import",
        " -> ",
        rownames(in.mat2)
      )
    ),
    "",
    "! Export flows",
    "",
    paste0(
      rownames(ex.mat2),
      "_EX: ",
      rownames(ex.mat2),
      " -> ",
      rownames(ex.mat2),
      "Export"
    ),
    ""
  )
  return(x)
}
