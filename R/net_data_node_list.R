#' @title net_data_node_list()
#' @description Defines the final compartment list with standing stock equalities
#' to include in the LIM declaration file. Orders living compartments in
#' alphabetical order, then moves non-living compartments (if applicable) to
#' the end of the list
#' @keywords internal
#' @param x Tidy network data input matrix
#' @return A vector of compartments together with standing stock equalities.
#' Living compartments arranged in alphabetical order.
#' Non-living compartments (if applicable) are moved to the end of the list.
#'
net_data_node_list <- function(x) {
  # Check if input is valid
  if (is.null(x) || nrow(x) == 0) {
    warning("Empty or NULL input in net_data_node_list")
    return(character(0))
  }

  # Debug information (only for testing)
  # message("DEBUG: Input matrix dimensions: ", nrow(x), "x", ncol(x))
  # message("DEBUG: First few rownames: ", paste(head(rownames(x)), collapse=", "))

  # Extract biomass column
  if ("Biomass" %in% colnames(x)) {
    biomass_values <- x[, "Biomass"]

    # Create vector with proper formatting - ensure rownames are included
    formatted_strings <- paste(rownames(x), "=", biomass_values)

    # Debug the formatted strings (only for testing)
    # message("DEBUG: First few formatted strings: ", paste(head(formatted_strings), collapse=", "))

    # Separate living and non-living nodes
    LN <- grep(formatted_strings,
               pattern = "NLNode",
               invert = TRUE,
               value = TRUE
    ) # grab living nodes

    NLN <- grep(formatted_strings,
                pattern = "NLNode",
                invert = FALSE,
                value = TRUE
    ) # grab nonliving nodes

    # Combine in order
    combined <- c(sort(LN), sort(NLN))

    return(combined)
  } else {
    warning("No 'Biomass' column found in input data")
    return(character(0))
  }
}
