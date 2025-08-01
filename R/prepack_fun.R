#' @title prepack_fun()
#' @description Function to pack network values into network objects.
#' Based on package enaR network packing criteria (Lau et al., 2017)
#'
#' @param x A solved flow matrix, orientated from row i to column j.
#' @param full_lim LIM Declaration file.
#' @importFrom network network set.edge.attribute set.vertex.attribute
prepack_fun <- function(x, full_lim) {
  if (!requireNamespace("network", quietly = TRUE)) {
    stop("Package \"network\" must be installed to use this function.",
         call. = FALSE)
  }

  # Create flow matrix
  flow <- as.matrix(fmat_fun(x))

  # Debug info (only for testing)
  # message("Flow matrix dimensions: ", nrow(flow), "x", ncol(flow))
  # positive_count <- sum(flow > 0)
  # message("Flow matrix has ", positive_count, " positive values")

  # List objects together
  listed <- list(
    flow = flow,
    input = input_fun(x),
    export = export_fun(x),
    respiration = resp_fun(x),
    storage = as.vector(full_lim[["Components"]][["val"]]),
    living = living_fun(x)
  )

  # Create network with vertices based on flow matrix
  y <- network::network.initialize(nrow(flow), directed = TRUE, loops = TRUE)

  # Set vertex names
  network::set.vertex.attribute(y, "vertex.names", rownames(flow))

  # Find positive flow values
  edge_ids <- which(flow > 0, arr.ind = TRUE)

  # If we have positive flows, add them as edges
  if (nrow(edge_ids) > 0) {
    message("Setting edge attributes for ", nrow(edge_ids), " positive flows")

    # Add edges for positive flows
    for (i in 1:nrow(edge_ids)) {
      from_id <- edge_ids[i, 1]
      to_id <- edge_ids[i, 2]
      flow_value <- flow[from_id, to_id]

      # Add the edge
      network::add.edge(y, from_id, to_id)

      # Set the flow attribute
      network::set.edge.attribute(y, "flow", flow_value,
                                  e = network::get.edgeIDs(y, from_id, to_id))
    }
  } else {
    message("WARNING: No positive flow values found - adding dummy flow edge")

    # Add a dummy edge with zero flow to ensure 'flow' attribute exists
    network::add.edge(y, 1, 1)  # Self-loop on first vertex
    network::set.edge.attribute(y, "flow", 0,
                                e = network::get.edgeIDs(y, 1, 1))
  }

  # Helper function to adjust attribute length and clean values
  adjust_attr <- function(attr, n) {
    if (is.null(attr) || length(attr) == 0) return(rep(0, n))
    if (length(attr) > n) attr <- attr[1:n]
    if (length(attr) < n) attr <- c(attr, rep(0, n - length(attr)))
    attr[is.na(attr)] <- 0  # Replace NA with 0
    return(as.numeric(attr))  # Ensure numeric
  }

  # Fix attributes
  n_vertices <- network::network.size(y)
  input_attr <- adjust_attr(listed$input, n_vertices)
  export_attr <- adjust_attr(listed$export, n_vertices)
  respiration_attr <- adjust_attr(listed$respiration, n_vertices)
  output_attr <- adjust_attr(output_fun(x), n_vertices)
  storage_attr <- adjust_attr(listed$storage, n_vertices)
  living_attr <- adjust_attr(listed$living, n_vertices)

  # Set vertex attributes
  network::set.vertex.attribute(y, "input", input_attr)
  network::set.vertex.attribute(y, "export", export_attr)
  network::set.vertex.attribute(y, "respiration", respiration_attr)
  network::set.vertex.attribute(y, "output", output_attr)
  network::set.vertex.attribute(y, "storage", storage_attr)
  network::set.vertex.attribute(y, "living", living_attr)

  # Return the network object
  return(y)
}
