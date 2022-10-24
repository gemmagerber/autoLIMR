## Function: variable_def()
#' Defines whole variables sections from various other functions
#' @inheritParams qvar
#' @inheritParams pvar
#' @inheritParams uvar
#' @inheritParams aevar
#' @inheritParams pp_true
#' @export
variable_def <-
  function(x,
           NLNode,
           primary_producer,
           respiration) {
    if (length(NLNode) > 0) {
      u_var <- uvar(x, respiration = respiration)
    } else {
      u_var <- c("! No Unused Energy/Material Variables defined", "")
    }

    q_var <- qvar(x)
    p_var <- pvar(x, NLNode = NLNode, respiration = respiration)
    ae_var <- aevar(x, respiration = respiration)

    if (!is.null(primary_producer)) {
      q_var <- pp_true(q_var, primary_producer = primary_producer)
      p_var <- pp_true(p_var, primary_producer = primary_producer)
      u_var <- pp_true(u_var, primary_producer = primary_producer)
      ae_var <- pp_true(ae_var, primary_producer = primary_producer)

    }

    toreturn <- c(q_var,
                  p_var,
                  u_var,
                  ae_var)

    return(toreturn)

  }
