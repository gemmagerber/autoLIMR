#' @title net_data_resp_flows()
#' @description Constructs respiration flows for each living compartment, if
#' respiration is included in the model, and the external sink is 'CO2'
#' @param x Tidy network data input matrix.
#' @inheritParams autoGen
#' @return Vector containing the respiration flows and vanity headings
#'
net_data_resp_flows <- function(x,
                                respiration = respiration,
                                respiration_element = respiration_element,
                                primary_producer = primary_producer,
                                NLNode = NLNode) {
  if (respiration == TRUE) {
    LN <- net_data_node(x, node.type = "living", NLNode = NLNode)

    if (!is.null(primary_producer)) {
      gpp <- grep(
        as.vector(rownames(x)),
        pattern = paste0(primary_producer, collapse = "|"),
        value = TRUE,
        invert = FALSE,
        ignore.case = FALSE
      )

      if (is.null(respiration_element)) {
        gppF <- paste(gpp, "_GPP: ", "CO2", " -> ", gpp, sep = "")
        RF <- paste(LN, "_R: ", LN, " -> ", "CO2", sep = "")
      } else {
        gppF <-
          paste(gpp,
            "_GPP: ",
            toupper(respiration_element),
            " -> ",
            gpp,
            sep = ""
          )
        RF <-
          paste(LN,
            "_R: ",
            LN,
            " -> ",
            toupper(respiration_element),
            sep = ""
          )
      }
      toreturn <-
        c(
          "! GPP flows",
          "",
          gppF,
          "",
          "! Respiration flows",
          "",
          RF,
          ""
        )
      return(toreturn)
    } else {
      if (is.null(respiration_element)) {
        RF <- paste(LN, "_R: ", LN, " -> ", "CO2", sep = "")
      } else {
        RF <-
          paste(LN,
            "_R: ",
            LN,
            " -> ",
            toupper(respiration_element),
            sep = ""
          )
      }
      toreturn <- c("! Respiration flows", "", RF, "")
      return(toreturn)
    }
  }
}
