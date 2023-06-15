#' @title variable_def()
#' @description Defines whole variables sections from various other functions
#' @param x tidy network input data matrix
#' @inheritParams autoGen
#' @return vector of variables

variable_def <-
  function(x,
           NLNode,
           primary_producer,
           respiration) {
    q_var <- qvar(x)
    p_var <- pvar(x, NLNode = NLNode, respiration = respiration)
    u_var <- uvar(x, respiration = respiration, NLNode = NLNode)
    ae_var <- aevar(x, respiration = respiration)

    combined <- c(
      q_var,
      p_var,
      u_var,
      ae_var
    )

    # Check for primary producers
    # Change Q to GPP, and P to NPP
    if (!is.null(primary_producer)) {
      qtogpp <- paste0(paste0(primary_producer, "_Q"), collapse = "|")
      ptonpp <- paste0(paste0(primary_producer, "_P"), collapse = "|")

      combined2 <- ifelse(grepl(x = combined, pattern = qtogpp) == TRUE,
        gsub(x = combined, pattern = "_Q", replacement = "_GPP"),
        ifelse(grepl(x = combined, pattern = ptonpp) == TRUE,
          gsub(x = combined, pattern = "_P", replacement = "_NPP"),
          combined
        )
      )
      return(combined2)
    } else {
      return(combined)
    }
  }
