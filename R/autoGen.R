#' @title Function autoGen()
#' @description Automatically defines LIM weighted and unweighted declaration files
#' from two input workbooks.
#'
#' This function takes some action. It also attempts to create a
#' few files in your working directory called \code{data.R},
#' under folders \code{autoLIMR Weighted Network LIMfiles},
#' and \code{autoLIMR Unweighted Network LIMfiles}.
#' If \code{data.R} files cannot be created a warning is raised.
#'
#' @inheritParams error_print
#' @inheritParams read_all_sheets
#' @inheritParams net_data_tidy
#' @inheritParams adj_mat_tidy
#' @inheritParams meta1

#' @param NLNode Character vector defining non-living compartments e.g.,
#'  NLNode = "Detritus", NLNode = c("Detritus", "Detritus2")
#' @param respiration Logical. Is respiration included in the network model? Default = TRUE
#' @param respiration_element Character vector. If Respiration = TRUE,
#'   the external respiration sink element to be defined. Defaults to "CO2"
#' @param primary_producer Character vector defining primary producer compartments e.g.,
#'   primary_producer = "Plant", primary_producer = c("Plant", "Plant2")
#' @param author Character vector. LIM declaration file author name.Defaults to system user.
#' @param date Character vector. Date of file creation. Defaults to system date.
#' @param weighted Logical. Return weighted LIM declaration files? Defaults to TRUE
#' @return Two folders containing weighted, and unweighted network LIM
#'   declaration files respectively. For use with R packages 'LIM' and 'limSolve'
#' @param force If set to \code{TRUE}, files of \code{data.R},
#' under folders \code{autoLIMR Weighted Network LIMfiles},
#' and \code{autoLIMR Unweighted Network LIMfiles} will be created on the
#' user's working directory. If this function is used in an
#' interactive session the user will be asked whether or not \code{data.R}
#' files should be created. The default value is \code{FALSE}.
#' @examples \dontrun{
#' autoGen(
#'   net_data_input = "your_network_data_workbook.xlsx",
#'   adj_mat_input = "your_adjacency_matrix_data_workbook.xlsx",
#'   NLNode = "Detritus",
#'   primary_producer = "Plant",
#'   respiration = TRUE,
#'   respiration_element = "CO2",
#'   author = "Gemma Gerber",
#'   weighted = TRUE,
#'   force = TRUE
#' )
#' }
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
  # Execution: Print Errors for undefined sheets
  error_print(net_data_input, adj_mat_input)

  # Are we reading in demo data, or actual .xlsx files?

  if (net_data_input == "demo") {
    # Get demo data
    net_data_sheet_list <- demo_net_input()
    # return(net_data_sheet_list)
  } else {
    # Execution: Read in net data sheets
    net_data_sheet_list <-
      read_all_sheets(filename = net_data_input)
    # return(net_data_sheet_list)
  }

  if (adj_mat_input == "demo") {
    adj_matrix_sheet_list <- demo_adj_mat()
    # return(adj_matrix_sheet_list)
  } else {
    # Execution: Read in Fmats
    adj_matrix_sheet_list <-
      read_all_sheets(filename = adj_mat_input)
    # return(adj_matrix_sheet_list)
  }

  # Execution: Tidy up net_data_sheet_list
  net_data_sheets <-
    lapply(X = net_data_sheet_list, net_data_tidy, NLNode = NLNode)

  # Execution: Tidy up adj mat sheets
  adj_matrix_sheets <-
    lapply(X = adj_matrix_sheet_list, adj_mat_tidy, NLNode = NLNode)

  # Execution: define compartment list
  comp.list <- lapply(X = net_data_sheets, FUN = net_data_node_list)

  # Execution: define externals list
  externals.list <-
    lapply(
      X = net_data_sheets,
      FUN = net_data_external_list,
      respiration,
      respiration_element
    )

  # Execution: define QPU variables, change based on primary producers
  vars <- lapply(
    X = net_data_sheets,
    FUN = variable_def,
    NLNode = NLNode,
    primary_producer = primary_producer,
    respiration = respiration
  )

  # Execution: respiration flow definition
  resp_flows <- lapply(
    X = net_data_sheets,
    FUN = net_data_resp_flows,
    respiration = TRUE,
    respiration_element = respiration_element,
    primary_producer = primary_producer,
    NLNode = NLNode
  )

  # Execution: Input and Export flows
  inex.flow.list <-
    lapply(X = net_data_sheets, FUN = net_data_inex_flows)

  # Execution: Define matrix flows only
  adj.mats.flow.list <-
    lapply(X = adj_matrix_sheets, adj_mat_flows)

  # Execution: Get inequalities from matrices
  net_data_ineq_list <-
    lapply(X = net_data_sheets, net_data_ineq, primary_producer = primary_producer)
  adj_mat_ineq_list <-
    lapply(X = adj_matrix_sheets, adj_mat_ineq)

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

  ## Execution: merge_sections compartments, give name
  comp.lim <- lapply(
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

  # Execution: merge_sections compartments, give name
  externals.lim <- lapply(
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

  # Execution: merge_sections variable lists, name sections
  var.lim <- lapply(
    X = vars,
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

  # Execution: merge_sections flow lists, add section headings
  resp.lim <- lapply(
    X = resp_flows,
    function(x) {
      c("### FLOWS", "", x)
    }
  )

  inex.lim <- lapply(
    X = inex.flow.list,
    function(x) {
      c(x)
    }
  )

  admat.lim <- lapply(
    X = adj.mats.flow.list,
    function(x) {
      c(x, "", "### END FLOWS", "")
    }
  )

  netineq <- lapply(
    X = net_data_ineq_list,
    function(x) {
      c("### INEQUALITIES", "", x)
    }
  )
  admatineq <- lapply(
    X = adj_mat_ineq_list,
    function(x) {
      c(x, "", "### END INEQUALITIES", "")
    }
  )


  Weighted <-
    Map(
      c,
      meta_w,
      meta_2,
      comp.lim,
      externals.lim,
      var.lim,
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
      comp.lim,
      externals.lim,
      var.lim,
      resp.lim,
      inex.lim,
      admat.lim
    )

  ### Function: write LIMfiles to subfolders in working directory
  # Check if working directory exists (it should)
  # Then check if folder names exist
  # If folders do not exist, create them
  # Then write LIMfiles into the folders

  write_limfile <- function(type, object, force) {

    if (force == FALSE | is.null(force)) {
      stop("No permission to write LIMfiles. Please change by setting force = TRUE.")
    }

    if (type == "Weighted") {
      path <- file.path(getwd(), "weighted_limfiles")
    }

    if (type == "Unweighted") {
      path <- file.path(getwd(), "unweighted_limfiles")
    }

    if(force == TRUE | !force && interactive()) {
    # Interactive
      title <-
        paste0(
          "May autoLIMR create a folder of ",
          type,
          " LIMfiles in your working directory?"
        )

      result <- utils::select.list(c("Yes", "No"), title = title)

      # Check if working directory available. It should be.
      if (!dir.exists(file.path(getwd()))) {
        stop("No working directory exists. Please set with setwd().")}

      # Check if dir exists. If not, create them.
      if (dir.exists(path) == FALSE) {
        dir.create(path)}

      if (result == "Yes") {
        # Write files into subfolders in working directory
        for (i in 1:length(object)) {
          write(
            object[[i]],
            paste0(
              path,
              "////",
              names(object)[i],
              "_",
              type,
              "_Network_LIMfile.R"
            )
          )
        }
        message(
          "LIM Declaration files successfully written to folders. Please check working directory."
        )
      }

    }
  }


  write_limfile(type = "Weighted", object = Weighted, force)
  write_limfile(type = "Unweighted", object = Unweighted, force)

}
