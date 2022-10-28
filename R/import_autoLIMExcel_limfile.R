#' Function: import_autoLIMExcel_limfile()#'
#' Imports autoLIM-Excel LIM declaration file from autoLIM-Excel
#' @param file the autoLIMExcel file
#' @param weighted are you importing the weighted or unweighted limfile? Defaults to weighted = TRUE
#' @param limname the name you would like to give the network. Defaults to "Weighted_LIMfile.R" or " Unweighted_LIMfile.R" depending on weighted argument
#' @param open_script to view the script in R
#' @return
#' LIM Declaration File, saved as an R script

#' @importFrom cellranger cell_cols
#' @importFrom readxl read_excel
#' @importFrom utils write.table file.edit
#' @export
#' @examples
#' \dontrun{
#' import_autoLIMExcel_limfile(
#'   file = "autoLIM_Excel_test2.xlsx",
#'   weighted = TRUE, limname = NULL, open_script = FALSE
#' )
#' }
import_autoLIMExcel_limfile <- function(file,
                                        weighted = TRUE,
                                        limname = NULL,
                                        open_script = FALSE) {
  if (weighted == TRUE | is.null(weighted)) {
    sheetname <- "Weighted LIMfile"
  }

  if (weighted == FALSE) {
    sheetname <- "Unweighted LIMfile"
  }

  if (is.null(limname)) {
    tosave <- paste0(sheetname, ".R")
    tosave <- gsub(
      x = tosave,
      pattern = " ",
      replacement = "_"
    )
  }

  if (!is.null(limname)) {
    tosave <- paste0(limname, ".R")
    tosave <- gsub(
      x = tosave,
      pattern = " ",
      replacement = "_"
    )
  }

  options("scipen" = 99999)
  x <- readxl::read_excel(
    path = file,
    sheet = sheetname,
    col_types = "text",
    .name_repair = "unique",
    range = cellranger::cell_cols(1)
  )
  x[is.na(x)] <- " "
  write.table(
    x,
    file = tosave,
    sep = "/t",
    row.names = FALSE,
    quote = FALSE
  )
  # print(x)
  if (open_script == TRUE) {
    file.edit(tosave)
  }
}
