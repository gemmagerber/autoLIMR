#' @title write_limfile
#' @description Helper internal function of force_write_test() in autoGen().
#'   A function to write LIMfiles into the working directory.
#' @keywords internal
#' @param data limfile objects in environment
#' @param weighted Logical. Should both weighted and unweighted files be written?
#' @param overwrite Logical. Should existing files be overwritten? Default is TRUE.
#'
write_limfile <- function(data, weighted = NULL, overwrite = overwrite) {
  # Always use working directory
  path <- getwd()

  # Create correct directory names matching documentation
  path_w <- file.path(path, "weighted_limfiles")
  path_uw <- file.path(path, "unweighted_limfiles")

  # Create directories with recursive=TRUE for nested paths
  dir.create(path_w, showWarnings = FALSE, recursive = TRUE)
  dir.create(path_uw, showWarnings = FALSE, recursive = TRUE)

  message("Using directories:")
  message("- Weighted: ", path_w)
  message("- Unweighted: ", path_uw)

  # Helper function to write files with better error handling and overwrite control
  write_files <- function(files, path, suffix) {
    if (is.character(files)) {
      filename <- file.path(path, paste0(suffix, "_network_limfile.R"))

      # Check if file exists
      if (file.exists(filename) && !overwrite) {
        message("Skipping existing file: ", filename)
      } else {
        tryCatch({
          write(files, filename)
          if (file.exists(filename)) {
            message("Written: ", filename)
          } else {
            warning("Failed to verify file was written: ", filename)
          }
        }, error = function(e) {
          warning("Failed to write ", filename, ": ", conditionMessage(e))
        })
      }
    } else if (is.list(files)) {
      for (i in seq_along(files)) {
        if (length(files) > 1) {
          filename <- file.path(path, paste0(suffix, "_network_limfile.R"))
        } else {
          filename <- file.path(path, paste0(suffix, "_network_limfile.R"))
        }

        # Check if file exists
        if (file.exists(filename) && !overwrite) {
          message("Skipping existing file: ", filename)
        } else {
          tryCatch({
            write(files[[i]], filename)
            if (file.exists(filename)) {
              message("Written: ", filename)
            } else {
              warning("Failed to verify file was written: ", filename)
            }
          }, error = function(e) {
            warning("Failed to write ", filename, ": ", conditionMessage(e))
          })
        }
      }
    }
  }

  # Retrieve weighted and unweighted data
  w <- data[["Weighted"]]
  uw <- data[["Unweighted"]]

  # Determine which files to write based on 'weighted' parameter
  if (is.null(weighted) || weighted) {
    write_files(w, path_w, "Weighted")
    write_files(uw, path_uw, "Unweighted")
  } else {
    write_files(uw, path_uw, "Unweighted")
  }

  message("LIM Declaration files processing complete.")
}
