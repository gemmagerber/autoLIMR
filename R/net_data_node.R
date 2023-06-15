#' @title net_data_node()
#' @description Defines vectors of living/non-living/input/export compartments
#' @param x Tidy network data input matrix
#' @param node.type One of living/nonliving/Input/Export
#' @param NLNode the defined non-living (NLNode) from the main autoGen() argument
#' @return A vector containing the node names for the compartments defined as
#' living/non-living/import/export

net_data_node <- function(x, node.type, NLNode) {
  if (node.type == "living") {
    LN <- grep(
      as.vector(rownames(x)),
      pattern = paste0(NLNode, "NLNode", collapse = "|"),
      value = TRUE,
      invert = TRUE,
      ignore.case = FALSE
    )
    return(LN)
  }
  if (node.type == "nonliving") {
    NLN <- grep(
      as.vector(rownames(x)),
      pattern = paste0(NLNode, "NLNode", collapse = "|"),
      value = TRUE,
      invert = FALSE,
      ignore.case = FALSE
    )
    return(NLN)
  }

  if (node.type == "Import") {
    input.matrix <- matrix_def(x, mat.type = "Import")
    in.mat3 <- as.vector(paste0(rownames(input.matrix), "Import"))
    return(in.mat3)
  }

  if (node.type == "Export") {
    export.matrix <- matrix_def(x, mat.type = "Export")
    export.nodes <-
      as.vector(paste0(rownames(export.matrix), "Export"))
    return(export.nodes)
  }
}
