#' @title function: net_data_ineq()
#' @description Inequalities definition (with headings)
#' @keywords internal
#' @param x network input data matrix
#' @param primary_producer Character vector defining primary producer compartments
#' @param NLNode Character vector defining one or multiple non-living compartments
#' @return Vector of inequalities from network input data matrix for LIM declaration file
#' @importFrom utils head
net_data_ineq <- function(x, primary_producer, NLNode = NULL) {
  # Helper function to format numbers without scientific notation
  format_number <- function(x, digits = 10) {
    if (is.numeric(x)) {
      return(format(x, scientific = FALSE, digits = digits, trim = TRUE))
    }
    if (is.character(x)) {
      # If it contains operators or letters, don't try to convert
      if (grepl("[*+/\\-]|[a-zA-Z]", x)) {
        return(x)  # Return expressions as is
      }

      x_clean <- gsub("[^0-9.-]", "", x)
      num_val <- suppressWarnings(as.numeric(x_clean))
      if (!is.na(num_val)) {
        return(format(num_val, scientific = FALSE, digits = digits, trim = TRUE))
      }
    }
    return(as.character(x))
  }

  # Helper function to normalize strings for comparison
  normalize_string <- function(s) {
    trimws(tolower(as.character(s)))
  }

  # Add robust input checking
  if (is.null(x) || length(x) == 0) {
    message("Empty input in net_data_ineq")
    return(c("! Network Data Input Inequalities", "", ""))
  }

  # Check if x already has the expected structure
  if (is.data.frame(x) && all(c("Var1", "Var2", "Freq") %in% colnames(x))) {
    # Already in the right format, just remove NAs
    x <- na.omit(x)
  } else {
    # Try to convert the structure safely
    tryCatch({
      # Extract inequality data from the matrix
      result <- data.frame(
        Var1 = character(0),
        Var2 = character(0),
        Freq = character(0),
        stringsAsFactors = FALSE
      )

      # Print all column names for debugging
      message("DEBUG: Column names: ", paste(head(colnames(x)), collapse=", "), "...")

      # IMPROVED LOGIC: Include expressions and filter out blank/zero values
      for (col in colnames(x)) {
        if (grepl("lower|upper|min|max", col, ignore.case = TRUE)) {
          message("DEBUG: Processing column: ", col)
          for (row in rownames(x)) {
            # More strict checking for blank values
            cell_value <- x[row, col]

            # Check if value is actually present (not NA, not "", not just whitespace)
            is_present <- !is.na(cell_value) &&
              !identical(cell_value, "") &&
              !identical(as.character(cell_value), "NA") &&
              !grepl("^\\s*$", as.character(cell_value))

            # Check if it's a valid value to include
            if (is_present) {
              cell_str <- as.character(cell_value)

              # Check if it's an expression or a meaningful number
              is_expression <- grepl("[*+/\\-]|[a-zA-Z]", cell_str)
              is_simple_zero <- !is_expression && (cell_str == "0" || cell_str == "0.0" || cell_str == "0.00")

              # Include expressions and non-zero values
              if (is_expression || !is_simple_zero) {
                result <- rbind(result, data.frame(
                  Var1 = row,
                  Var2 = col,
                  Freq = cell_str,
                  stringsAsFactors = FALSE
                ))
              }
            }
          }
        }
      }

      if (nrow(result) > 0) {
        x <- result
        message("Found ", nrow(x), " inequality entries")
      } else {
        message("No inequality data found in input")
        return(c("! Network Data Input Inequalities", "", ""))
      }
    }, error = function(e) {
      message("Error processing inequality data: ", conditionMessage(e))
      return(c("! Network Data Input Inequalities", "", ""))
    })
  }

  # Debug primary producer and NLNode information
  message("Primary producer node(s): ", paste(primary_producer, collapse=", "))
  if (!is.null(NLNode)) {
    message("Non-living node(s): ", paste(NLNode, collapse=", "))
  }

  # Create normalized versions for matching
  x$Var1_norm <- sapply(x$Var1, normalize_string)
  pp_norm <- if(is.null(primary_producer)) character(0) else sapply(primary_producer, normalize_string)
  nl_norm <- if(is.null(NLNode)) character(0) else sapply(NLNode, normalize_string)

  # Debug column names for checking patterns
  message("DEBUG: Var2 column values: ", paste(head(x$Var2), collapse=", "))

  # Set a default variable value first
  x$Variable <- rep("unknown", nrow(x))

  # FIXED PATTERN MATCHING: Use more precise patterns to avoid false matches

  # Consumption/GPP - More specific pattern with word boundaries
  idx <- grepl("^(Consumption|GPP|Q)$|^(Consumption|GPP|Q)_|_Q$", x$Var2, ignore.case = TRUE)
  idx_pp <- idx & (x$Var1_norm %in% pp_norm)
  idx_non_pp <- idx & !(x$Var1_norm %in% pp_norm)
  x$Variable[idx_pp] <- paste0(x$Var1[idx_pp], "_GPP")
  x$Variable[idx_non_pp] <- paste0(x$Var1[idx_non_pp], "_Q")

  # Production/NPP - More specific pattern with word boundaries
  idx <- grepl("^(Production|NPP|P)$|^(Production|NPP|P)_|_P$", x$Var2, ignore.case = TRUE) &
    !grepl("^GPP", x$Var2, ignore.case = TRUE) # Exclude GPP which is handled above
  idx_pp <- idx & (x$Var1_norm %in% pp_norm)
  idx_non_pp <- idx & !(x$Var1_norm %in% pp_norm)
  x$Variable[idx_pp] <- paste0(x$Var1[idx_pp], "_NPP")
  x$Variable[idx_non_pp] <- paste0(x$Var1[idx_non_pp], "_P")

  # Other variable types - Much more specific patterns

  # Respiration - Fix overly broad match
  idx <- grepl("^(Respiration|Resp)$|^(Respiration|Resp)_|^R$|^R_", x$Var2, ignore.case = TRUE) &
    !grepl("(Production|Rate|Range|Ratio)", x$Var2, ignore.case = TRUE) # Exclude common R-containing words

  # Debug the respiration detection
  if (any(idx)) {
    message("DEBUG: Found respiration columns: ", paste(x$Var2[idx], collapse=", "))
  }

  idx_living <- !(x$Var1_norm %in% nl_norm)
  x$Variable[idx & idx_living] <- paste0(x$Var1[idx & idx_living], "_R")

  # Skip respiration variables for non-living nodes completely
  x$Variable[idx & !idx_living] <- "SKIP_THIS_VARIABLE"

  # Unused energy - More specific pattern
  idx <- grepl("^(Unused)$|^(Unused)_|^U$|^U_", x$Var2, ignore.case = TRUE) &
    !grepl("(Upper|Use|Used)", x$Var2, ignore.case = TRUE) # Exclude common U-containing words
  x$Variable[idx] <- paste0(x$Var1[idx], "_U")

  # Export - More specific pattern
  idx <- grepl("^(Export|EX|Exports)$|^(Export|EX|Exports)_", x$Var2, ignore.case = TRUE)
  x$Variable[idx] <- paste0(x$Var1[idx], "_EX")

  # Import - More specific pattern
  idx <- grepl("^(Import|Imports|Input|IN)$|^(Import|Imports|Input|IN)_", x$Var2, ignore.case = TRUE) &
    !grepl("(min|Include)", x$Var2, ignore.case = TRUE) # Exclude common IN-containing words
  x$Variable[idx] <- paste0(x$Var1[idx], "_IM")

  # Assimilation - More specific pattern
  idx <- grepl("^(Assimilation|Efficiency|AE)$|^(Assimilation|Efficiency|AE)_", x$Var2, ignore.case = TRUE)
  x$Variable[idx] <- paste0(x$Var1[idx], "_AE")

  # Filter out any "SKIP" variables
  x <- x[x$Variable != "SKIP_THIS_VARIABLE", ]

  # Debug variable assignments
  var_types <- gsub(".*_", "", x$Variable)
  var_counts <- table(var_types)
  message("DEBUG: Variable types detected: ",
          paste(names(var_counts), var_counts, sep="=", collapse=", "))

  # Handle unclassified "unknown" variables
  unknown_vars <- x[x$Variable == "unknown", ]
  if (nrow(unknown_vars) > 0) {
    message("Warning: Found ", nrow(unknown_vars), " variables that couldn't be classified")
    message("DEBUG: First few unclassified Var2 values: ", paste(head(unknown_vars$Var2), collapse=", "))

    # Create a clean suffix from Var2 for the variable name
    clean_suffix <- function(s) {
      # Extract first word, remove special chars
      first_word <- strsplit(s, "\\s+|_")[[1]][1]
      clean <- gsub("[^A-Za-z0-9]", "", first_word)
      return(paste0("_", clean))
    }

    # Apply the suffix to each unknown variable
    for (i in which(x$Variable == "unknown")) {
      suffix <- clean_suffix(x$Var2[i])
      x$Variable[i] <- paste0(x$Var1[i], suffix)
    }
  }

  # Process inequalities without additional filtering
  # Use the input values exactly as they are
  x$Freq <- as.character(x$Freq)
  x$Inequality <- rep("none", nrow(x))

  # Format values to avoid scientific notation but preserve expressions
  x$Freq_formatted <- sapply(x$Freq, format_number)

  # Create inequalities based only on column name pattern
  idx_lower <- grepl("lower|low|min|minimum", x$Var2, ignore.case = TRUE)
  x$Inequality[idx_lower] <- paste0(
    x$Variable[idx_lower],
    " > ",
    x$Freq_formatted[idx_lower]
  )

  idx_upper <- grepl("upper|up|max|maximum", x$Var2, ignore.case = TRUE)
  x$Inequality[idx_upper] <- paste0(
    x$Variable[idx_upper],
    " < ",
    x$Freq_formatted[idx_upper]
  )

  # Filter out "none" inequalities
  x <- x[x$Inequality != "none", ]

  # If no inequalities found, return minimal output
  if (nrow(x) == 0) {
    return(c("! Network Data Input Inequalities", "", ""))
  }

  toreturn <- c(
    "! Network Data Input Inequalities",
    "",
    as.vector(x$Inequality),
    ""
  )
  return(toreturn)
}
