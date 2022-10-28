#' Function: fetch_autoLIMExcel()
#' Downloads the latest version of autoLIM-Excel from GitHub
#' @return autoLIM-Excel as .xlsx file in working directory
#' @param force #' If set to \code{TRUE}, files of \code{autoLIM_Excel.xlsx}
#' will be downloaded from GitHub and saved in the users working directory. If
#' this function is used in an interactive session the user will be asked
#' whether or not \code{autoLIM_Excel.xlsx} can be downloaded. The default
#' value is \code{FALSE}. Users will need to explicityl change \code{TRUE} to
#' provide permission for download.
#' @importFrom utils download.file
#' @importFrom utils select.list
#' @export
#'
fetch_autoLIMExcel <- function(force = FALSE) {
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("Package \"utils\" must be installed to use this function.",
         call. = FALSE)
  }

  filename <- paste0(getwd(), "/", "autoLIM_Excel.xlsx")
  fileURL <-
    "https://github.com/gemmagerber/autoLIM-Excel/raw/main/autoLIMExcel_Latest.xlsx"

  # Check if working directory available. It should be.
  if (!dir.exists(file.path(getwd()))) {
    stop("No working directory exists. Please set with setwd().", call. = FALSE)
  } else {

    if (!force && interactive()) {
      title <-
        paste0("May autoLIMR download autoLIM_Excel.xlsx and save to the workign directory?")
      result <- utils::select.list(c("Yes", "No"), title = title)
      if (result == "Yes") {
        # Download and save
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
    }

    else if(force == TRUE) {
    # Check if dir exists. If not, create them.
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
      } else {
  warning("No permission to download autoLIM_Excel.xlsx. Please change by setting force = TRUE..")
      }
  }
}


