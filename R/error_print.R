#' @title Function: error_print()
#' @description Validates input files and provides feedback
#' @keywords internal
#' @inheritParams autoGen
#' @return Logical. TRUE if validation passed, FALSE otherwise.
error_print <- function(net_data_input, adj_mat_input) {
  # Check for NULL inputs first (early returns)
  if (is.null(net_data_input)) {
    stop(
      "Please provide the network input data workbook (.xlsx) or comma
      separated value (.csv) filename and file extension containing
      biomass and inequalities ('net_data_input' argument)."
    )
  }

  if (is.null(adj_mat_input)) {
    stop(
      "Please provide the input data workbook (.xlsx) or comma
      separated value (.csv) filename and file extension containing
      adjacency matrices of internal network flows
      ('adj_mat_input' argument)."
    )
  }

  # Check file types
  valid_pattern <- "\\.csv$|\\.xlsx$|demo"

  if (!grepl(net_data_input, pattern = valid_pattern)) {
    stop(
      "No network input workbook (.xlsx or .csv) provided, nor has the
      'demo' data been stipulated. Within the 'net_data_input' argument,
      please provide the name and file extension of the network input data
      workbook (e.g., 'myfile.xlsx' or 'myfile.csv') containing network
      compartment biomass and inequalities. If you would like to use the
      demo datasets, use 'net_data_input = demo'."
    )
  }

  if (!grepl(adj_mat_input, pattern = valid_pattern)) {
    stop(
      "No adjacency matrix workbook (.xlsx or .csv) provided, nor has the
      'demo' data been stipulated. Within the 'adj_mat_input' argument,
      please provide the name and file extension of the adjacency matrix
      workbook (e.g., 'myadmat.xlsx' or 'myadmat.csv') containing
      adjacency matrices of internal network flows. If you would like to
      use the demo datasets, use 'adj_mat_input = demo'."
    )
  }

  # Check file type match (only if both aren't "demo")
  if (net_data_input != "demo" && adj_mat_input != "demo") {
    net_data_input_filetype <- sub(net_data_input,
                                   pattern = ".*\\.(.*)",
                                   replacement = "\\1")

    adj_mat_input_filetype <- sub(adj_mat_input,
                                  pattern = ".*\\.(.*)",
                                  replacement = "\\1")

    if (net_data_input_filetype != adj_mat_input_filetype) {
      stop(
        "Please ensure the input files have the same filetypes.
        Both should be either '.xlsx' or '.csv'"
      )
    }
  }

  # Simply return TRUE without the message
  return(TRUE)
}
