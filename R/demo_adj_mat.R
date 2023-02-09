#' @title demo_adj_mat() function
#' @description Create a named list of demo adjacency matrix sheets for 4node
#'
#' @return A list containing 4 seasons worth of network input data
#' @export
demo_adj_mat <- function() {
  adj_summer <- data.frame(
    X = c("Det", "Plant", "Invert", "Vert"),
    Det = c(NA, 1, 1, 1),
    Plant = c(NA, NA, NA, NA),
    Invert = c(1, 1, NA, NA),
    Vert = c(NA, NA, 1, NA)
  )

  adj_autumn <- data.frame(
    X = c("Det", "Plant", "Invert", "Vert"),
    Det = c(NA, 1, 1, 1),
    Plant = c(NA, NA, NA, NA),
    Invert = c(1, 1, NA, NA),
    Vert = c(NA, NA, 1, NA)
  )

  adj_winter <- data.frame(
    X = c("Det", "Plant", "Invert", "Vert"),
    Det = c(NA, 1, 1, 1),
    Plant = c(NA, NA, NA, NA),
    Invert = c("0.01, 0.6", 0.6, NA, NA),
    Vert = c(0.1, NA, "0.6, 1", NA)
  )

  adj_spring <- data.frame(
    X = c("Det", "Plant", "Invert", "Vert"),
    Det = c(NA, 1, 1, 1),
    Plant = c(NA, NA, NA, NA),
    Invert = c(1, 1, NA, NA),
    Vert = c(NA, NA, 1, NA)
  )

  adj_data <- list(
    "Summer" = adj_summer,
    "Autumn" = adj_autumn,
    "Winter" = adj_winter,
    "Spring" = adj_spring
  )

  return(adj_data)
}
