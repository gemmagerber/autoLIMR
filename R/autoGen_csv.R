#' @title autoGen_csv()
#' @description Reads comma separated value (CSV) files describing
#' 1) network input data and 2) internal flows as an adjacency matrix, and
#' translates into a weighted and unweighted LIM declaration file compatible
#' with LIM and limSolve.
#'
#' @inheritParams autoGen
#' @importFrom utils read.csv
#'
#' @return If argument \code{weighted = TRUE}, the default, a list of character
#' vectors describing a 1) weighted and 2) unweighted
#' network LIM declaration file. If argument \code{weighted = FALSE}, a list
#' containing a single LIM declaration file describing an unweighted network.

autoGen_csv <- function(net_data_input,
                        adj_mat_input,
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

  # Read in .csv files
  read_all_csv <- function(filename, tibble = FALSE) {
    options("scipen" = 999)
    x <- utils::read.csv(paste0(filename), colClasses = "character", na.strings = "")
    x <- as.data.frame(x)
    return(x)
  }

  net_data <- read_all_csv(net_data_input)
  admat_data <- read_all_csv(adj_mat_input)

  # Tidy read-in files
  tidy_net_data <- net_data_tidy(net_data, NLNode = NLNode)
  tidy_admat_data <- adj_mat_tidy(admat_data, NLNode = NLNode)

  # Define compartment list
  comp.list <- net_data_node_list(x = tidy_net_data)
  comp.list <-
    c(
      "### COMPARTMENTS",
      "",
      comp.list,
      "",
      "### END COMPARTMENTS",
      ""
    )

  # Define externals list
  externals.list <- net_data_external_list(
    x = tidy_net_data,
    respiration = respiration,
    respiration_element = respiration_element
  )
  externals.list <-
    c("### EXTERNALS", "", externals.list, "", "### END EXTERNALS", "")

  # Define variables, change based on primary producers
  vars.list <- variable_def(
    x = tidy_net_data,
    NLNode = NLNode,
    primary_producer = primary_producer,
    respiration = respiration
  )
  vars.list <-
    c("### VARIABLES", "", vars.list, "### END VARIABLES", "")

  ### Define flows
  # Define respiration and GPP flows
  resp.flows <- net_data_resp_flows(
    x = tidy_net_data,
    respiration = respiration,
    respiration_element = respiration_element,
    primary_producer = primary_producer,
    NLNode = NLNode
  )

  # Define Import and Export flows
  inex.flows <- net_data_inex_flows(x = tidy_net_data)

  # Define adjacency matrix flows (internal flows)
  admat.flows <- adj_mat_flows(x = tidy_admat_data)

  # Bind flows, give heading
  flows <- c(
    "### FLOWS",
    "",
    resp.flows,
    inex.flows,
    admat.flows,
    "",
    "### END FLOWS",
    ""
  )


  # Define inequalities
  net_inequalities <- net_data_ineq(
    x = tidy_net_data,
    primary_producer = primary_producer
  )
  admat_inequalities <- adj_mat_ineq(x = tidy_admat_data)

  ineq <- c(
    "### INEQUALITIES",
    "",
    net_inequalities,
    admat_inequalities,
    "",
    "### END INEQUALITIES",
    ""
  )

  # Get metadata table1 for weighted file
  meta_w <- meta1(
    x = tidy_net_data,
    primary_producer = primary_producer,
    respiration = respiration,
    respiration_element = respiration_element,
    NLNode = NLNode,
    weighted = TRUE,
    author = author,
    date = date
  )

  # Get metadata table1 for unweighted file
  meta_uw <- meta1(
    x = tidy_net_data,
    primary_producer = primary_producer,
    respiration = respiration,
    respiration_element = respiration_element,
    NLNode = NLNode,
    weighted = FALSE,
    author = author,
    date = date
  )

  # Get metadata2
  meta_2 <- meta2(x = tidy_admat_data)

  # Merge compartments with headings
  if (weighted == TRUE) {
    # Return weighted and unweighted
    Weighted <- c(
      meta_w,
      meta_2,
      comp.list,
      externals.list,
      vars.list,
      flows,
      ineq
    )
    Unweighted <- c(
      meta_uw,
      meta_2,
      comp.list,
      externals.list,
      vars.list,
      flows
    )
    returnlist <- list(Weighted, Unweighted)
    names(returnlist) <- c("Weighted", "Unweighted")
    return(returnlist)
  } else if (weighted == FALSE) {
    # return only unweighted
    Unweighted <- c(
      meta_uw,
      meta_2,
      comp.list,
      externals.list,
      vars.list,
      flows
    )
    returnlist <- list(Unweighted)
    names(returnlist) <- c("Unweighted")
    return(returnlist)
  }


  ## Do we write the files to hard drive?
  ## If not, return as list of two objects
}
