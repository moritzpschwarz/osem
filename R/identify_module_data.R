#' Identifies which variables are required for running the module
#'
#' Identifies the required variables for running the module and extracts that
#' subset.
#'
#' @param module A row of the specification table.
#' @param classification A data.frame with two columns: 'var' stores the
#'   Eurostat variable code and 'class' the classification code (one of
#'   \code{"n"}, \code{"d"}, or \code{"x"}) reflecting whether the variable is
#'   endogenous by modelling, endogenous by identity/definition, or exogenous.
#'   As returned by \link{classify_variables}.
#' @param data A dataset containing at least the required dependent variable and
#'   independent variables for the module.
#'
#' @return Returns a subset of the original \code{data} that only contains the
#'   relevant variables for the module.
#'
#' @export

identify_module_data <- function(module, classification, data) {

  # extract variable names
  dep <- module$dependent_eu
  indep <- module$independent_eu
  indep <- unlist(strsplits(indep, split = c("\\+", "\\-")))
  indep <- gsub(" ", "", indep)

  # if module is an identity/definition module then the dependent variable is not necessary
  if (module$type == "d") {

    # original variables
    vars.need.base <- indep
    # in the model, need the predicted values (hat) of the endogenous variables
    # check status of the variables: if x need original data; if n or d, need hat
    vars.need <- classification[classification$var %in% vars.need.base, ] %>%
      mutate(vartrans = case_when(class == "n" ~ paste0(var, ".hat"),
                                  class == "d" ~ paste0(var, ".hat"),
                                  class == "x" ~ var,
                                  TRUE ~ NA_character_)) %>%
      pull(vartrans)

  } else if (module$type == "n") { # module is endogenous and actually modelled
    # for endogenous modelled variables, always use the observed values
    vars.need <- union(dep, indep)
  }

  # check whether necessary variables are in dataset
  if (!(all(vars.need %in% unique(data$na_item)))) {
    stop(paste0("Cannot find all required variables in the data for module ", module$order, "."))
  }

  # extract the necessary data
  sub <- data %>%
    filter(na_item %in% vars.need)

  return(sub)

}

