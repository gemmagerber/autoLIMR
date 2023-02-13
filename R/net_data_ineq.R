#' @title Function: net_data_ineq()
#' @description Inequalities definition (with headings)
#' @param x network input data matrix
#' @param respiration  If respiration = TRUE in main autoLIMR argument
#' @param primary_producer Primary producers defined in main autoLIMR function
#'
net_data_ineq <- function(x, respiration,
                          primary_producer) {
  x <- na.omit(as.data.frame(as.table(x)))

  x$Variable <- ifelse(
    grepl("Consumption|GPP|Q", x$Var2) &
      grepl(paste0(primary_producer, collapse = "|"), x$Var1),
    paste0(x$Var1, "_GPP"),
    ifelse(
      grepl("Consumption|GPP|Q", x$Var2) &
        !grepl(paste0(primary_producer, collapse = "|"), x$Var1),
      paste0(x$Var1, "_Q"),
      ifelse(
        grepl("Production|Net_Primary_Production|NPP|P", x$Var2) &
          grepl(paste0(primary_producer, collapse = "|"), x$Var1),
        paste0(x$Var1, "_NPP"),
        ifelse(
          grepl("Production|Net_Primary_Production|NPP|P", x$Var2) &
            !grepl(paste0(primary_producer, collapse = "|"), x$Var1),
          paste0(x$Var1, "_P"),
          ifelse(
            grepl("respiration|Resp|R", x$Var2),
            paste0(x$Var1, "_R"),
            ifelse(
              grepl("Unused energy|Unused|U", x$Var2),
              paste0(x$Var1, "_U"),
              ifelse(
                grepl("Export|EX|Ex|Exports", x$Var2),
                paste0(x$Var1, "_EX"),
                ifelse(
                  grepl("Import|Imports|Input|IN|In", x$Var2),
                  paste0(x$Var1, "_IM"),
                  ifelse(
                    grepl("Assimilation|Efficiency|AE", x$Var2),
                    paste0(x$Var1, "_AE"),
                    "none"
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  x$Freq <- as.character(x$Freq)

  x$Inequality <- ifelse(
    x$Freq %in% c(1, 0, "1", "0", "1.00",
                  "0.00000", "0.00000000", "0.000"),
    "none",
    ifelse(
      grepl("lower|low|min|minimum", x$Var2, ignore.case = TRUE),
      paste0(x$Variable, " > ", x$Freq),
      ifelse(
        grepl("upper|up|max|maximum", x$Var2, ignore.case = TRUE),
        paste0(x$Variable, " < ", x$Freq),
        "none"
      )
    )
  )

  x <- x[!grepl("none", x$Inequality),]

  toreturn <- c("! Network Data Input Inequalities",
                "",
                as.vector(x$Inequality),
                "")
  return(toreturn)
}
