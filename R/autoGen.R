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
#' @param weighted Logical. Return weighted LIM declaration files? If \code{NULL},
#' the default, both weighted and unweighted LIM declaration files are returned.
#' Defaults to \code{TRUE}.
#'
#' @param force If set to \code{TRUE}, files of \code{data.R},
#' under folders \code{autoLIMR Weighted Network LIMfiles},
#' and \code{autoLIMR Unweighted Network LIMfiles} will be created on the
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
                    force = FALSE) {
  # Function setup
  write_limfiles <- function(x, weighted = NULL, path = NULL) {
    if (is.null(path)) {
      path <- getwd()
    } else if (!is.null(path)) {
      path <- path
    }

    if (weighted == TRUE | is.null(weighted)) {
      # Create folders
      path_w <- file.path(path, "weighted_limfiles")
      if (dir.exists(path_w) == FALSE) {
        dir.create(path_w)
      }
      path_uw <- file.path(path, "unweighted_limfiles")
      if (dir.exists(path_uw) == FALSE) {
        dir.create(path_uw)
      }

      # Check classes
      w <- x[["Weighted"]]
      uw <- x[["Unweighted"]]

      # For .csv original files
      if (inherits(w, "character") == TRUE & inherits(uw, "character") == TRUE) {
        write(
          x[["Weighted"]],
          paste0(path_w, "////", "Weighted_Network_LIMfile.R")
        )
        write(
          x[["Unweighted"]],
          paste0(path_uw, "////", "Unweighted_Network_LIMfile.R")
        )
      } else if (inherits(w, "list") == TRUE & inherits(uw, "list") == TRUE) {
        for (i in 1:length(w)) {
          write(
            w[[i]],
            paste0(path_w, "////", names(w)[i], "_Weighted_Network_LIMfile.R")
          )
        }

        for (i in 1:length(uw)) {
          write(
            uw[[i]],
            paste0(path_uw, "////", names(uw)[i], "_Unweighted_Network_LIMfile.R")
          )
        }
      }

      message("LIM Declaration files successfully written to the working directory.")
    } else if (weighted == FALSE) {
      # Create folders
      path_uw <- file.path(path, "unweighted_limfiles")
      if (dir.exists(path_uw) == FALSE) {
        dir.create(path_uw)
      }

      # For .csv original files
      if (inherits(class(x[["Unweighted"]]), "character") == TRUE) {
        write(
          x[["Unweighted"]],
          paste0(path_uw, "////", "Unweighted_Network_LIMfile.R")
        )
      } else if (inherits(class(x[["Unweighted"]]), "list") == TRUE) {
        uw <- x[["Unweighted"]]

        for (i in 1:length(uw)) {
          write(
            uw[[i]],
            paste0(path_uw, "////", names(uw)[i], "_Unweighted_Network_LIMfile.R")
          )
        }
      }
      message("LIM Declaration files successfully written to the working directory.")
    }
  }

  force_write_test <- function(x, weighted) {
    if (force == FALSE | is.null(force)) {
      return(x)
    } else if (force == TRUE) {
      if (interactive() == TRUE) {
        title <-
          paste0("May autoLIMR write LIM declaration files to the working directory?")
        result <- utils::select.list(c("Yes", "No"), title = title)

        if (result == "Yes") {
          write_limfiles(x, weighted = weighted)
        } else if (result == "No") {
          return(x)
        }
      } else if (interactive() == FALSE) {
        # print('not interactive')
        return(x)
      }
    }
  }

  # Error checks
  error_print(net_data_input, adj_mat_input) # Print errors for undefined inputs

  # Are input files .csv, .xlsx, or 'demo'?
  # If .csv ...
  if (grepl(net_data_input, pattern = "\\.csv$") == TRUE &
    grepl(adj_mat_input, pattern = "\\.csv$") == TRUE) {
    limfile_list <- autoGen_csv(
      net_data_input = net_data_input,
      adj_mat_input = adj_mat_input,
      NLNode = NLNode,
      respiration = respiration,
      respiration_element = respiration_element,
      primary_producer = primary_producer,
      weighted = weighted,
      author = author,
      date = date
    )

    force_write_test(x = limfile_list, weighted = weighted)
  } else {
    # If input files are .xlsx or 'demo':
    # Are we reading in demo data, or actual .xlsx files?
    if (net_data_input == "demo") {
      net_data_sheet_list <- demo_net_input() # Get demo data
    } else {
      net_data_sheet_list <-
        read_all_sheets(filename = net_data_input) # Read in net data sheets
    }

    if (adj_mat_input == "demo") {
      adj_matrix_sheet_list <- demo_adj_mat()
    } else {
      adj_matrix_sheet_list <-
        read_all_sheets(filename = adj_mat_input) # Read in adjacency matrices
    }

    # Execution: Tidy up net_data_sheet_list
    net_data_sheets <-
      lapply(X = net_data_sheet_list, net_data_tidy, NLNode = NLNode)

    # Execution: Tidy up adj mat sheets
    adj_matrix_sheets <-
      lapply(X = adj_matrix_sheet_list, adj_mat_tidy, NLNode = NLNode)

    # Execution: define compartment list
    comp.list <- lapply(X = net_data_sheets, FUN = net_data_node_list)
    comp.list <- lapply(
      X = comp.list,
      function(x) {
        c(
          "### COMPARTMENTS",
          "",
          x,
          "",
          "### END COMPARTMENTS",
          ""
        )
      }
    )

    # Execution: define externals list
    externals.list <-
      lapply(
        X = net_data_sheets,
        FUN = net_data_external_list,
        respiration,
        respiration_element
      )
    externals.list <- lapply(
      X = externals.list,
      function(x) {
        c(
          "### EXTERNALS",
          "",
          x,
          "",
          "### END EXTERNALS",
          ""
        )
      }
    )

    # Execution: define QPU variables, change based on primary producers
    vars.list <- lapply(
      X = net_data_sheets,
      FUN = variable_def,
      NLNode = NLNode,
      primary_producer = primary_producer,
      respiration = respiration
    )
    var.list <- lapply(
      X = vars.list,
      function(x) {
        c(
          "### VARIABLES",
          "",
          x,
          "",
          "### END VARIABLES",
          ""
        )
      }
    )

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
    resp.lim <- lapply(
      X = resp.flows,
      function(x) {
        c("### FLOWS", "", x)
      }
    )

    inex.lim <- lapply(
      X = inex.flows,
      function(x) {
        c(x)
      }
    )

    admat.lim <- lapply(
      X = admat.flows,
      function(x) {
        c(x, "", "### END FLOWS", "")
      }
    )


    # Execution: Get inequalities from matrices
    net_data_ineq_list <-
      lapply(X = net_data_sheets, net_data_ineq, primary_producer = primary_producer)

    netineq <- lapply(
      X = net_data_ineq_list,
      function(x) {
        c("### INEQUALITIES", "", x)
      }
    )

    adj_mat_ineq_list <-
      lapply(X = adj_matrix_sheets, adj_mat_ineq)

    admatineq <- lapply(
      X = adj_mat_ineq_list,
      function(x) {
        c(x, "", "### END INEQUALITIES", "")
      }
    )

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
    meta_2 <- lapply(
      X = adj_matrix_sheets,
      FUN = meta2
    )



    Weighted <-
      Map(
        c,
        meta_w,
        meta_2,
        comp.list,
        externals.list,
        var.list,
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
        var.list,
        resp.lim,
        inex.lim,
        admat.lim
      )

    returnlist <- list(Weighted, Unweighted)
    names(returnlist) <- c("Weighted", "Unweighted")

    ### Function: write LIMfiles to sub folders in working directory
    force_write_test(x = returnlist, weighted = weighted)
  }
}
