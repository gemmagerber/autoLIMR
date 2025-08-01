#' @title adj_mat_ineq()
#' @description Defines inequalities from adjacency matrix input workbook
#' @param x Adjacency matrix
#' @param primary_producer Character vector defining primary producer compartments
#' @param NLNode Character vector defining one or multiple non-living compartments
#' @return A vector of adjacency matrix inequalities with vanity headings
#' to be included in the LIM declaration file

adj_mat_ineq <- function(x, primary_producer = NULL, NLNode = NULL) {
  # Helper function for string normalization
  normalize_string <- function(s) {
    trimws(tolower(as.character(s)))
  }

  # Extract table from matrix and convert to data frame
  temp_df <- as.data.frame(as.table(x))

  # Filter out blank, zero, and NA entries BEFORE normalization
  temp_df <- temp_df[!is.na(temp_df$Freq), ]  # Remove NAs

  # Remove zeros (handle both numeric and character representations)
  is_zero <- sapply(temp_df$Freq, function(val) {
    if (is.numeric(val)) {
      return(val == 0 || abs(val) < 1e-10)
    } else {
      # Check common string representations of zero
      val_str <- as.character(val)
      return(val_str == "" ||
               val_str == "0" ||
               val_str == "0.0" ||
               val_str == "0.00" ||
               grepl("^\\s*$", val_str))  # Blank or whitespace
    }
  })

  # Keep only non-zero entries
  temp_df <- temp_df[!is_zero, ]

  # If no valid entries remain, return empty result
  if (nrow(temp_df) == 0) {
    message("No valid inequality entries found in adjacency matrix")
    return(c("! Adjacency Matrix Inequalities", "", ""))
  }

  # Continue with filtered data
  x <- temp_df

  # Debug: Show how many entries remain after filtering
  message("Processing ", nrow(x), " non-zero adjacency matrix entries")

  # Create normalized versions for matching
  x$Var1_norm <- sapply(as.character(x$Var1), normalize_string)
  x$Var2_norm <- sapply(as.character(x$Var2), normalize_string)
  pp_norm <- if(is.null(primary_producer)) character(0) else sapply(primary_producer, normalize_string)
  nl_norm <- if(is.null(NLNode)) character(0) else sapply(NLNode, normalize_string)

  # Debug info
  if(length(primary_producer) > 0) {
    message("Primary producer nodes in adj_mat_ineq: ", paste(primary_producer, collapse=", "))
    message("Any matches in Var1? ", ifelse(any(x$Var1_norm %in% pp_norm), "YES", "NO"))
    message("Any matches in Var2? ", ifelse(any(x$Var2_norm %in% pp_norm), "YES", "NO"))
  }

  # Determine flow type based on node types
  x$flowtype <- apply(x, 1, function(row) {
    var1 <- row["Var1_norm"]
    var2 <- row["Var2_norm"]

    # Check if primary producer
    is_pp1 <- var1 %in% pp_norm
    is_pp2 <- var2 %in% pp_norm

    # Check if non-living
    is_nl1 <- var1 %in% nl_norm || grepl("nlnode", var1, ignore.case = TRUE)
    is_nl2 <- var2 %in% nl_norm || grepl("nlnode", var2, ignore.case = TRUE)

    # Determine flow type
    if(is_nl1 && is_nl2) {
      return("_to_")
    } else if(is_nl1 && !is_nl2) {
      # If destination is a primary producer, use GPP
      if(is_pp2) {
        return("_GPP_")
      } else {
        return("_Q_")
      }
    } else if(!is_nl1 && is_nl2) {
      return("_U_")
    } else if(!is_nl1 && !is_nl2) {
      # If destination is a primary producer, use GPP
      if(is_pp2) {
        return("_GPP_")
      } else {
        return("_Q_")
      }
    } else {
      return("_to_") # Default fallback
    }
  })

  # Create flow names
  x$flownames <- paste0(x$Var1, x$flowtype, x$Var2)

  # Determine variable type
  x$Variable <- apply(x, 1, function(row) {
    var1 <- row["Var1_norm"]
    var2 <- row["Var2_norm"]

    # Check if primary producer
    is_pp1 <- var1 %in% pp_norm
    is_pp2 <- var2 %in% pp_norm

    # Check if non-living
    is_nl1 <- var1 %in% nl_norm || grepl("nlnode", var1, ignore.case = TRUE)
    is_nl2 <- var2 %in% nl_norm || grepl("nlnode", var2, ignore.case = TRUE)

    # Determine variable type
    if(is_nl1 && is_nl2) {
      return("_to")
    } else if(is_nl1 && !is_nl2) {
      # If destination is a primary producer, use GPP
      if(is_pp2) {
        return("_GPP")
      } else {
        return("_Q")
      }
    } else if(!is_nl1 && is_nl2) {
      return("_U")
    } else if(!is_nl1 && !is_nl2) {
      # If destination is a primary producer, use GPP
      if(is_pp2) {
        return("_GPP")
      } else {
        return("_Q")
      }
    } else {
      return("_to") # Default fallback
    }
  })

  # Format frequency values
  x$Freq <- as.character(x$Freq)

  # Create inequality expressions
  x$ineq <- ifelse(
    grepl("^1$|^1.0$|^1.00$", x$Freq) & !grepl(",", x$Freq),
    "none",
    ifelse(
      grepl(",", x$Freq) & (x$Var2 %in% NLNode | grepl("NLNode", x$Var2, ignore.case = TRUE)),
      paste0(
        x$flownames,
        " = ",
        x$Var1,
        x$Variable,
        " * ",
        "[",
        x$Freq,
        "]"
      ),
      ifelse(
        grepl(",", x$Freq) & !(x$Var2 %in% NLNode | grepl("NLNode", x$Var2, ignore.case = TRUE)),
        paste0(
          x$flownames,
          " = ",
          x$Var2,
          x$Variable,
          " * ",
          "[",
          x$Freq,
          "]"
        ),
        ifelse(
          !grepl(",", x$Freq) & (x$Var2 %in% NLNode | grepl("NLNode", x$Var2, ignore.case = TRUE)),
          paste0(
            x$flownames,
            " < ",
            x$Var1,
            x$Variable,
            " * ",
            x$Freq
          ),
          ifelse(
            !grepl(",", x$Freq) & !(x$Var2 %in% NLNode | grepl("NLNode", x$Var2, ignore.case = TRUE)),
            paste0(
              x$flownames,
              " < ",
              x$Var2,
              x$Variable,
              " * ",
              x$Freq
            ),
            "none"
          )
        )
      )
    )
  )

  # Remove "none" entries
  na_remove <- x[!grepl("none", x$ineq), ]
  ineq_vec <- as.vector(na_remove$ineq)

  # Return result with headings
  toreturn <- c(
    "! Adjacency Matrix Inequalities",
    "",
    ineq_vec
  )

  return(toreturn)
}
