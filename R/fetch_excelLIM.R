#' @title Function fetch_excelLIM()
#' @description Downloads the latest version of excelLIM from GitHub
#' @return excelLIM as .xlsx file in working directory
#' @param force #' If set to \code{TRUE}, files of \code{excelLIM.xlsx}
#' will be downloaded from GitHub and saved in the users working directory. If
#' this function is used in an interactive session the user will be asked
#' whether or not \code{excelLEM.xlsx} can be downloaded. The default
#' value is \code{FALSE}. Users will need to explicitly change \code{TRUE} to
#' provide permission for download.
#' @importFrom utils download.file
#' @importFrom utils select.list
#' @export
#'
fetch_excelLIM <- function(force = FALSE) {
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("Package \"utils\" must be installed to use this function.",
      call. = FALSE
    )
  }

  fileURL <-
    "https://github.com/gemmagerber/excelLIM/raw/main/excelLIM_v1.0.0.xlsx"

  file <- gsub(
    "https://github.com/gemmagerber/excelLIM/raw/main/",
    replace = "",
    x = fileURL
  )

  filename <- paste0(getwd(), "/", file)

  # Check if working directory available
  if (!dir.exists(file.path(getwd()))) {
    stop("No working directory exists. Please set with setwd().",
      call. = FALSE
    )
  } else {
    if (!force && interactive()) {
      title <-
        paste0(
          "May autoLIMR download '",
          file,
          "' and save to the working directory?"
        )
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
              "Success! The latest version of 'excelLIM' has been downloaded
              and saved in the working directory."
            )
          )
        } else {
          message(
            strwrap(
              prefix = " \n",
              initial = "",
              "'excelLIM' already exists. Please check the working directory."
            )
          )
        }
      }
    } else if (force == TRUE) {
      # Check if directory exists. If not, create them.
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
            "Success! The latest version of 'excelLIM' has been downloaded
            and saved in the working directory."
          )
        )
      } else {
        message(
          strwrap(
            prefix = " \n",
            initial = "",
            "'excelLIM' already exists. Please check the working directory."
          )
        )
      }
    } else {
      warning(
        "No permission to download 'excelLIM'. Please change by setting force = TRUE"
      )
    }
  }
}
