#' @title force_write_test
#' @description For use in autoGen function. Generates interactive menu to select if files should be written to the working directory, or not.
#' @keywords internal
#' @param data List of character vectors describing LIM declaration files
#' @param weighted Logical. Whether to include weighted networks
#' @param force Logical. If TRUE, automatically writes files without prompting.
#'   If NULL or not defined, shows interactive prompt.
#'   If FALSE (default), shows interactive prompt.
#' @param overwrite Logical. Should existing files be overwritten? Default is TRUE.
#' @return The data parameter is returned unchanged
#'
force_write_test <- function(data, weighted, force = FALSE, overwrite = TRUE) {
  # message("DEBUG: Inside force_write_test function")
  # message("DEBUG: force parameter is ", force)

  # If force is TRUE, write files automatically
  if (isTRUE(force)) {
    message("Automatically writing files (force=TRUE)")
    write_limfile(data = data, weighted = weighted, overwrite = overwrite)
    return(data)
  }

  # In all other cases (force=FALSE or force=NULL), show interactive prompt
  if (interactive()) {
    title <- "May autoLIMR write LIM declaration files to the working directory?"
    user_choice <- utils::select.list(c("Yes", "No"), title = title)
    if (user_choice == "Yes") {
      # If user selected Yes, ask about overwriting if overwrite is FALSE
      if (!overwrite) {
        title2 <- "Overwrite existing files if they exist?"
        overwrite_choice <- utils::select.list(c("Yes", "No"), title = title2)
        if (overwrite_choice == "Yes") {
          overwrite <- TRUE
        }
      }

      message("User selected Yes - writing files")
      write_limfile(data = data, weighted = weighted, overwrite = overwrite)
    } else {
      message("User selected No - skipping file writing")
    }
  } else {
    message("Non-interactive session with force=FALSE - skipping file writing")
  }

  # Always return the data
  return(data)
}
