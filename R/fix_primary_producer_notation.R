#' @title fix_primary_producer_notation()
#' @description Replace Q with GPP and P with NPP for primary producers
#' @param lim_files List of LIM file contents
#' @param primary_producer Character vector defining primary producer compartments
#' @return List of corrected LIM file contents

fix_primary_producer_notation <- function(lim_files, primary_producer) {
  if (is.null(primary_producer) || length(primary_producer) == 0) {
    return(lim_files)
  }

  message("Applying primary producer notation fixes...")

  # Process each LIM file in the list
  for (i in seq_along(lim_files)) {
    # Get the content as a character vector
    content <- lim_files[[i]]

    # Process each primary producer
    for (pp in primary_producer) {
      # Create patterns to match
      q_pattern <- paste0(pp, "_Q")
      p_pattern <- paste0(pp, "_P")

      # Replace Q with GPP and P with NPP
      content <- gsub(q_pattern, paste0(pp, "_GPP"), content, fixed = TRUE)
      content <- gsub(p_pattern, paste0(pp, "_NPP"), content, fixed = TRUE)

      # Handle cases where the primary producer might be part of a flow name
      flow_q_pattern <- paste0(pp, "_Q_")
      flow_p_pattern <- paste0(pp, "_P_")
      content <- gsub(flow_q_pattern, paste0(pp, "_GPP_"), content, fixed = TRUE)
      content <- gsub(flow_p_pattern, paste0(pp, "_NPP_"), content, fixed = TRUE)
    }

    # Update the content
    lim_files[[i]] <- content
  }

  message("Primary producer notation fixes complete")
  return(lim_files)
}
