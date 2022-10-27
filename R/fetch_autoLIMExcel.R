#' Function: fetch_autoLIMExcel()
#' Downloads the latest version of autoLIM-Excel from GitHub
#' @return autoLIM-Excel as .xlsx file in working directory

#' @importFrom utils download.file
#' @export
#'
fetch_autoLIMExcel <- function() {
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("Package \"utils\" must be installed to use this function.",
         call. = FALSE)
  }

  filename <- paste0(getwd(), "/", "autoLIM_Excel.xlsx")
  fileURL <-
    "https://github.com/gemmagerber/autoLIM-Excel/raw/main/autoLIMExcel_Latest.xlsx"

  if (file.exists(filename) == FALSE) {
    utils::download.file(
      url = fileURL,
      destfile = filename,
      mode = "wb",
      method = "auto"
    )

    message(
      strwrap(
        prefix = " \n",
        initial = "",
        "Success! The latest version of autoLIM-Excel has been downloaded and saved in the working directory."
      )
    )
  } else {
    message(
      strwrap(
        prefix = " \n",
        initial = "",
        "autoLIM-Excel already exists. Check the working directory."
      )
    )
  }
}
