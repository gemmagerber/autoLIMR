#' @title autoGen()
#'
#' @description Automatically translate network input data into LIM declaration
#' files describing weighted and unweighted networks. Capable of translating
#' multiple networks at once (via Excel '\code{.xlsx}' inputs of multiple
#' sheets), or a single network at a time (via comma separated value
#' '\code{.csv}'inputs for a single network).
#'
#' When using \code{.xlsx} input files describing multiple networks,
#' \code{autoGen()} simultaneously translates multiple network models into
#' their respective LIM declaration files. This is advantageous when
#' constructing multiple network models, e.g., for a time series.
#' Function \code{autoGen()} requires two Workbooks, namely the
#' ‘Network Data Input’ workbook (Workbook 1) and the ‘Adjacency Matrix’
#' workbook (Workbook 2) as input. Within both workbooks, the user specifies
#' a unique worksheet name for each network model, which should match
#' identically between the two workbooks.
#'
#' As Microsoft Excel is proprietary and may not function on all operating
#' systems on which R is used, \code{autoGen()} also accepts
#' non-proprietary comma separated values (CSV) input files. As CSV files are
#' limited to a single sheet per file, as opposed to Excel workbooks that can
#' contain multiple sheets, the translation of input data from CSV files to
#' LIM declaration files with \code{autoGen()} is limited to a single network
#' at a time. Despite this limitation, the translation functionality from CSV
#' files allows autoLIMR to be used in R on multiple operating systems,
#' aligning with the power of R’s open source nature, availability,
#' and computer operating system interpretability.
#'
#' This function takes some action. It also attempts to create a
#' few files in your working directory called \code{data.R},
#' under folders \code{autoLIMR Weighted Network LIMfiles},
#' and \code{autoLIMR Unweighted Network LIMfiles}.
#' If \code{data.R} files cannot be created a warning is raised.
#'
#' @param net_data_input the user-defined network input data workbook
#' (.xlsx) or comma separated value (.csv) filename and file extension
#' containing biomass and inequalities. If the argument is missing, the
#' function will stop and return an error prompting the user to declare
#' the appropriate input file.
#'
#' @param adj_mat_input the user-defined input data workbook (.xlsx) or comma
#' separated value (.csv) filename and file extension containing
#' adjacency matrices of internal network flows. If the argument is missing, the
#' function will stop and return an error prompting the user to declare
#' the appropriate input file.
#'
#' @param NLNode Character vector defining one or multiple non-living
#' compartments e.g., \code{NLNode = "Detritus"},
#' \code{NLNode = c("Detritus", "Detritus2")}.
#'
#' @param respiration Logical. Is respiration included in the network model?
#' If \code{TRUE}, the default, named respiration
#' flows are defined from all defined living compartments (in the case of
#' ecological networks) to an external carbon dioxide \code{'CO2'} sink (e.g.,
#' \code{Invertebrate_R: Invertebrate -> CO2}). Additionally, if primary
#' producers are included in the network (e.g., ecological networks), Gross
#' Primary Production (GPP) flows are defined for each primary-producing
#' compartment as the flow of energy/material from the external carbon
#' dioxide sink to the compartment (e.g., \code{Plant_GPP: CO2 -> Plant}).
#' If \code{FALSE}, no
#' respiration flows are defined for living compartments.
#'
#' @param respiration_element Character vector. If \code{NULL}, the default,
#' the external respiration element is defined as \code{'CO2'}.
#' This argument is only valid if argument \code{respiration = TRUE}.
#'
#' @param primary_producer Character vector defining one or multiple
#' primary producer compartments (in the case of ecological networks)
#' e.g., \code{primary_producer = "Plant"},
#' \code{primary_producer = c("Plant", "Plant2")}.
#'
#' @param author Character vector. LIM declaration file author name.
#' Defaults to system user.
#'
#' @param date Character vector. Date of file creation. If \code{NULL},
#' the default, the date defaults to the system date.
#'
#' @param weighted Logical. Return weighted LIM declaration files? If
#' \code{NULL}, the default, both weighted and unweighted LIM declaration files
#' are returned. Defaults to \code{TRUE}.
#'
#' @param force If set to \code{TRUE}, files of \code{data.R},
#' under folders \code{weighted_limfiles},
#' and \code{unweighted_limfiles} will be created on the
#' user's working directory. If this function is used in an
#' interactive session the user will be asked whether or not \code{data.R}
#' files should be created. The default value is \code{FALSE}.
#'
#' @return If argument \code{weighted = TRUE}, the default, a list of character
#' vectors describing a 1) weighted and 2) unweighted
#' network LIM declaration files. If argument \code{weighted = FALSE}, a list
#' containing a single LIM declaration file describing an unweighted network.
#' Each LIM declaration file is the standard
#'   input file to solve a single, or multiple plausible network configurations
#'   using linear inverse modelling (LIM) and Markov Chain Monte Carlo (MCMC)
#'   techniques using the autoLIMR function \code{multi_net()}, or with external
#'   R packages \code{LIM} and '\code{limSolve}.
#'
#'
#'
#' @examples
#' \dontrun{
#' autoGen(
#'   net_data_input = "your_network_data_workbook.csv",
#'   adj_mat_input = "your_adjacency_matrix_data_workbook.csv",
#'   NLNode = "Detritus",
#'   primary_producer = "Plant",
#'   respiration = TRUE,
#'   respiration_element = "CO2",
#'   weighted = TRUE,
#'   force = TRUE
#' )
#'
#' autoGen(
#'   net_data_input = "your_network_data_workbook.xlsx",
#'   adj_mat_input = "your_adjacency_matrix_data_workbook.xlsx",
#'   NLNode = c("Detritus", "Detritus2"),
#'   primary_producer = c("Plant", "Plant2"),
#'   respiration = TRUE,
#'   respiration_element = "CO2",
#'   weighted = TRUE,
#'   force = TRUE
#' )
#' }
#'
#' @export

autoGen <- function(net_data_input = "demo",
                    adj_mat_input = "demo",
                    NLNode = NULL,
                    respiration = TRUE,
                    respiration_element = "CO2",
                    primary_producer = NULL,
                    author = NULL,
                    date = NULL,
                    weighted = TRUE,
                    force = TRUE) {
  # Call error_print for validation
  error_print(net_data_input, adj_mat_input)

  # Show success message just once
  message(
    strwrap(
      prefix = " \n",
      initial = "",
      "Both workbooks are readable, good job! Translating LIM declaration files..."
    )
  )

  # Are input files .csv, .xlsx, or 'demo'? # If .csv ...
  # Determine input file types and load appropriate data
  is_csv_net <- grepl("\\.csv$", net_data_input)
  is_csv_adj <- grepl("\\.csv$", adj_mat_input)
  is_demo_net <- identical(net_data_input, "demo")
  is_demo_adj <- identical(adj_mat_input, "demo")

  # Check file existence for non-demo inputs
  if (!is_demo_net &&
      !is_csv_net && !grepl("\\.xlsx$", net_data_input)) {
    stop("Network input must be 'demo', a .csv file, or a .xlsx file")
  }

  if (!is_demo_adj &&
      !is_csv_adj && !grepl("\\.xlsx$", adj_mat_input)) {
    stop("Adjacency matrix input must be 'demo', a .csv file, or a .xlsx file")
  }

  if (!is_demo_net && !file.exists(net_data_input)) {
    stop("Network input file not found: ", net_data_input)
  }

  if (!is_demo_adj && !file.exists(adj_mat_input)) {
    stop("Adjacency matrix file not found: ", adj_mat_input)
  }

  # Process CSV inputs
  if (is_csv_net && is_csv_adj) {
    message("Processing CSV input files...")
    # Option 1: Define the needed variables here
    net_data_sheet_list <- list()
    net_data_sheet_list[[1]] <- read.csv(net_data_input, stringsAsFactors = FALSE)
    adj_matrix_sheet_list <- list()
    adj_matrix_sheet_list[[1]] <- read.csv(adj_mat_input, stringsAsFactors = FALSE)

    # Or Option 2: Return early with processed results
    # limfile_list <- autoGen_csv(...)
    # return(limfile_list)  # Skip the rest of the function for CSV
  } else {
    message("Processing Excel/demo input files...")
    # Load network data
    if (is_demo_net) {
      message("Using demo network data...")
      net_data_sheet_list <- demo_net_input()
    } else {
      message("Reading network data from file...")
      net_data_sheet_list <- read_all_sheets(filename = net_data_input)
    }

    # Load adjacency matrices
    if (is_demo_adj) {
      message("Using demo adjacency matrices...")
      adj_matrix_sheet_list <- demo_adj_mat()
    } else {
      message("Reading adjacency matrices from file...")
      adj_matrix_sheet_list <- read_all_sheets(filename = adj_mat_input)
    }
  }

  # Execution: Tidy up net_data_sheet_list
  net_data_sheets <-
    lapply(X = net_data_sheet_list, net_data_tidy, NLNode = NLNode)

  # Execution: Tidy up adj mat sheets
  adj_matrix_sheets <-
    lapply(X = adj_matrix_sheet_list, adj_mat_tidy, NLNode = NLNode)

  # Execution: define compartment list
  comp.list <- lapply(X = net_data_sheets, FUN = net_data_node_list)
  comp.list <- lapply(X = comp.list, function(x) {
    c("### COMPARTMENTS", "", x, "", "### END COMPARTMENTS", "")
  })

  # Execution: define externals list
  externals.list <-
    lapply(X = net_data_sheets,
           FUN = net_data_external_list,
           respiration,
           respiration_element)
  externals.list <- lapply(X = externals.list, function(x) {
    c("### EXTERNALS", "", x, "", "### END EXTERNALS", "")
  })

  # Execution: define QPU variables, change based on primary producers
  vars.list <- lapply(
    X = net_data_sheets,
    FUN = variable_def,
    NLNode = NLNode,
    primary_producer = primary_producer,
    respiration = respiration
  )
  vars.list <- lapply(X = vars.list, function(x) {
    c("### VARIABLES", "", x, "", "### END VARIABLES", "")
  })

  ## Flow definition
  # Define Respiration flows
  resp.flows <- lapply(
    X = net_data_sheets,
    FUN = net_data_resp_flows,
    respiration = TRUE,
    respiration_element = respiration_element,
    primary_producer = primary_producer,
    NLNode = NLNode
  )

  # Define Import and Export flows
  inex.flows <-
    lapply(X = net_data_sheets, FUN = net_data_inex_flows)

  # Define adjacency matrix flows (internal flows)
  admat.flows <-
    lapply(X = adj_matrix_sheets, adj_mat_flows)

  # Merge flow lists, add section heading and ending
  resp.lim <- lapply(X = resp.flows, function(x) {
    c("### FLOWS", "", x)
  })

  inex.lim <- lapply(X = inex.flows, function(x) {
    c(x)
  })

  admat.lim <- lapply(X = admat.flows, function(x) {
    c(x, "", "### END FLOWS", "")
  })


  # Execution: Get inequalities from matrices
  net_data_ineq_list <-
    lapply(X = net_data_sheets, net_data_ineq,
           primary_producer = primary_producer,
           NLNode = NLNode)

  netineq <- lapply(X = net_data_ineq_list, function(x) {
    c("### INEQUALITIES", "", x)
  }
  )

  adj_mat_ineq_list <-
    lapply(X = adj_matrix_sheets, adj_mat_ineq)

  admatineq <- lapply(X = adj_mat_ineq_list, function(x) {
    c(x, "", "### END INEQUALITIES", "")
  })

  # Execution: Get metadata table1 for weighted file
  meta_w <- lapply(
    X = net_data_sheets,
    FUN = meta1,
    primary_producer = primary_producer,
    respiration = respiration,
    respiration_element = respiration_element,
    NLNode = NLNode,
    weighted = TRUE,
    author = author,
    date = date
  )

  # Execution: Get metadata table1 for unweighted file
  meta_uw <- lapply(
    X = net_data_sheets,
    FUN = meta1,
    primary_producer = primary_producer,
    respiration = respiration,
    respiration_element = respiration_element,
    NLNode = NLNode,
    weighted = FALSE
  )

  # Execution: get metadata table2
  meta_2 <- lapply(X = adj_matrix_sheets, FUN = meta2)

  Weighted <-
    Map(
      c,
      meta_w,
      meta_2,
      comp.list,
      externals.list,
      vars.list,
      resp.lim,
      inex.lim,
      admat.lim,
      netineq,
      admatineq
    )

  Unweighted <-
    Map(
      c,
      meta_uw,
      meta_2,
      comp.list,
      externals.list,
      vars.list,
      resp.lim,
      inex.lim,
      admat.lim
    )
  # Add a helper function to format numbers consistently
  format_number <- function(x, digits = 10) {
    # Safely try to convert to numeric with warning suppression
    num_value <- suppressWarnings(as.numeric(x))

    if (!is.na(num_value)) {
      # It's a valid number, format without scientific notation
      return(format(num_value, scientific = FALSE, digits = digits, trim = TRUE))
    } else {
      # Not a valid number, return unchanged
      return(x)
    }
  }

  # Replace scientific notation in all LIM files
  fix_scientific_notation <- function(lim_file) {
    # Process each line
    sapply(lim_file, function(line) {
      # Skip lines that don't contain numbers with decimal points
      if (!grepl("[0-9]\\.[0-9]", line)) {
        return(line)
      }

      # Try to match values that could be in scientific notation
      # Extract parts that might be numbers using a more careful approach
      parts <- unlist(strsplit(line, "[^0-9.e+-]+"))
      parts <- parts[parts != ""]

      # Process each potential number
      for (part in parts) {
        # Only process parts that match scientific notation pattern (e.g., 1.23e+05)
        if (grepl("^[0-9]+\\.[0-9]+e[+-][0-9]+$", part)) {
          # Safely convert to standard notation
          formatted <- format_number(part)
          # Replace in the line (only exact matches)
          line <- gsub(part, formatted, line, fixed = TRUE)
        }
      }
      return(line)
    }, USE.NAMES = FALSE)
  }

  # Apply to both weighted and unweighted LIM files
  Weighted <- lapply(Weighted, fix_scientific_notation)
  Unweighted <- lapply(Unweighted, fix_scientific_notation)

  returnlist <- list(Weighted, Unweighted)
  names(returnlist) <- c("Weighted", "Unweighted")

  # # Apply the primary producer notation fix
  # returnlist <- lapply(returnlist, function(files) {
  #   fix_primary_producer_notation(files, primary_producer)
  # })

  # Write files to directories
  file_paths <- force_write_test(data = returnlist,
                                 weighted = weighted,
                                 force = force)

  # Instead of returning the full LIM files, return a summary with paths
  if (is.logical(file_paths) && !file_paths) {
    # If force_write_test returned FALSE (files not written)
    message("Files were not written to disk.")

    # Create an invisible return value with the original data
    # This keeps backward compatibility for code that expects the full return value
    invisible(returnlist)
  } else {
    # If files were written successfully
    # Determine which directories were used
    weighted_dir <- file.path(getwd(), "weighted_limfiles")
    unweighted_dir <- file.path(getwd(), "unweighted_limfiles")

    # Create a summary result
    result <- list(
      success = TRUE,
      message = "LIM files successfully created.",
      directories = character(0),
      file_count = 0
    )

    # Add directory information based on files actually written
    if (file.exists(file.path(weighted_dir, "Weighted_network_limfile.R"))) {
      result$directories <- c(result$directories, weighted_dir)
      result$file_count <- result$file_count + 1
    }

    if (file.exists(file.path(unweighted_dir, "Unweighted_network_limfile.R"))) {
      result$directories <- c(result$directories, unweighted_dir)
      result$file_count <- result$file_count + 1
    }

    # Print success message
    message("Successfully created ", result$file_count, " LIM files in:")
    for (dir in result$directories) {
      message("  - ", dir)
    }

    # Return the summary invisibly, but also attach the original data as an attribute
    # for backward compatibility
    attr(result, "limfiles") <- returnlist
    invisible(result)
  }
}
