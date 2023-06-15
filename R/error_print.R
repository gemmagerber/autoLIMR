#' @title Function: error_print()
#' @description Prints errors if input files are undefined
#' @inheritParams autoGen
#'
#'
error_print <-
  function(net_data_input,
           adj_mat_input) {
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
        adjacency matrices of internal network flows ('adj_mat_input' argument)."
      )
    }

    if (!is.null(net_data_input) &
      !is.null(adj_mat_input)) {
      # Test that network input data is an .xlsx workbook, .csv file, or 'demo'
      if (grepl(net_data_input, pattern = "\\.csv$|\\.xlsx$|demo") == FALSE) {
        stop(
          "No network input workbook (.xlsx or .csv) provided, nor has the 'demo'
          data been stipulated. Within the 'net_data_input' argument, please
          provide the name and file extension of the network input data workbook
          (e.g., 'myfile.xlsx' or 'myfile.csv') containing network compartment
          biomass and inequalities. If you would like to use the demo datasets,
          please stipulate 'demo'."
        )
      }

      # Test that adjacency matrix input data is an .xlsx workbook, .csv file,
      # or 'demo'
      if (grepl(adj_mat_input, pattern = "\\.csv$|\\.xlsx$|demo") == FALSE) {
        stop(
          "No adjacency matrix workbook (.xlsx or .csv) provided, nor has the
          'demo' data been stipulated. Within the 'adj_mat_input' argument,
          please provide the name and file extension of the adjacency matrix
          workbook (e.g., 'myadmat.xlsx' or 'myadmat.csv') containing
          adjacency matrices of internal network flows. If you would like to use
          the demo datasets, please stipulate 'demo'."
        )
      }
      # Test that network input data and adjacency matrix input data are the same file type
      net_data_input_filetype <- sub(net_data_input,
        pattern = ".*\\.(.*)",
        replacement = "\\1"
      )

      adj_mat_input_filetype <- sub(adj_mat_input,
        pattern = ".*\\.(.*)",
        replacement = "\\1"
      )

      if (net_data_input_filetype != adj_mat_input_filetype) {
        stop(
          "Please ensure the input files have the same filetypes.
    Both should be either '.xlsx' or '.csv'"
        )
      }

      if (net_data_input_filetype == adj_mat_input_filetype) {
        message(
          strwrap(
            prefix = " \n",
            initial = "",
            "Both workbooks are readable, good job!
            Translating LIM declaration files..."
          )
        )
      }
    }
  }
