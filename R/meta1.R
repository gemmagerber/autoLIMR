#' @title meta1()
#' @description Create metadata for LIM declaration file
#' @keywords internal
#' @param x Network input data
#' @inheritParams autoGen
#' @return Character vector of metadata
meta1 <- function(x, primary_producer = NULL, respiration = TRUE,
                  respiration_element = "CO2", NLNode = NULL,
                  weighted = TRUE, author = NULL, date = NULL) {

  # Set default author if NULL
  if (is.null(author)) {
    author <- Sys.info()[["user"]]
  }

  # Set default date if NULL
  if (is.null(date)) {
    date <- format(Sys.Date(), "%Y-%m-%d")
  }

  # Count living and non-living compartments
  living_count <- sum(!grepl("NLNode", rownames(x)))
  nonliving_count <- sum(grepl("NLNode", rownames(x)))

  # Create metadata with meaningful network information
  network_type <- if(weighted) "Weighted" else "Unweighted"

  metadata <- c(
    paste0("! ", network_type, " Network LIM Declaration File"),
    "! Composed with autoLIMR",
    paste0("! Author: ", author),
    paste0("! Date: ", date),
    paste0("! Living compartments: ", living_count),
    paste0("! Non-living compartments: ", nonliving_count),
    paste0("! Total compartments: ", living_count + nonliving_count),
    ""
  )

  return(metadata)
}
