#' @title adj_mat_flows()
#' @description Defines internal network flows from adjacency matrix workbook
#' @param x adjacency matrix
#' @importFrom stats na.omit
#' @return Vector of adjacency matrix flows with vanity headings
#'
adj_mat_flows <- function(x) {
  x <- stats::na.omit(as.data.frame(as.table(x)))
  x$flowtype <-
    ifelse(
      grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
      "_to_",
      ifelse(
        grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
        "_Q_",
        ifelse(
          !grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
          "_U_",
          ifelse(
            !grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
            "_Q_",
            "test"
          )
        )
      )
    )

  x$flownames <- paste0(x$Var1, x$flowtype, x$Var2)
  x$flows <- paste0(x$flownames, ": ", x$Var1, " -> ", x$Var2)
  flows <- as.vector(x$flows)

  # Add headings
  flows2 <- c("! Adjacency Matrix flows", "", sort(flows))
  return(flows2)
}
