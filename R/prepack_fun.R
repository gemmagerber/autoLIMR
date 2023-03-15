#' @title prepack_fun(): function to pack network values into network objects
#' @description Based on package enaR network packing criteria (Lau et al., 2017)
#'
#' @param x A solved flow matrix, orientated from row i to column j
#' @param full_limfile LIM Declaration file built from check_build()
#' @importFrom network network set.edge.attribute set.vertex.attribute

prepack_fun <- function(x, full_limfile) {
  if (!requireNamespace("network", quietly = TRUE)) {
    stop("Package \"network\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # List objects together
  listed <- list(
    flow = as.matrix(fmat_fun(x)),
    input = input_fun(x),
    export = export_fun(x),
    respiration = resp_fun(x),
    storage = as.vector(full_limfile[["Components"]][["val"]]),
    living = living_fun(x)
  )

  # enaR::pack function bits
  y <- network::network(listed[[1]], directed = TRUE, loops = TRUE)
  network::set.edge.attribute(y, names(listed)[1], as.numeric(listed[[1]]))
  flow <- as.matrix(fmat_fun(x))
  rownames(flow) <- colnames(flow)
  network::set.edge.attribute(y, "flow", flow[flow > 0])
  network::set.vertex.attribute(y, "input", input_fun(x))
  network::set.vertex.attribute(y, "export", export_fun(x))
  network::set.vertex.attribute(y, "respiration", resp_fun(x))
  network::set.vertex.attribute(y, "output", output_fun(x))
  network::set.vertex.attribute(y, "storage", as.vector(full_limfile[["Components"]][["val"]]))
  network::set.vertex.attribute(y, "living", living_fun(x))
  network::set.vertex.attribute(y, "vertex.names", rownames(flow))
}
