#' Performs computations for the identity/definition modules
#'
#' Calculates the fitted values for identity/definition modules using previously
#' fitted values of endogenous variables.
#'
#' @inheritParams identify_module_data
#'
#' @return Returns the original tibble or data.frame \code{data} in wide format
#'   and appended with the fitted values of the module.
#'
#' @export

identity_module <- function(module, data, classification) {

  # original specification (identity/definition)
  rhs <- module$independent_eu
  indep <- strsplits(module$independent_eu, splits = c("\\+", "\\-"))
  indep <- gsub(" ", "", indep)

  # new fitted value name
  dep <- module$dependent_eu
  dep.fitted <- paste0(dep, ".level.hat")

  # check state of all that appear and translate to hat where necessary
  trans <- classification[(classification$var %in% indep), ] %>%
    filter(class %in% c("n", "d")) %>%
    mutate(vartrans = paste0(var, ".hat"))

  # translate
  for (i in 1:NROW(trans)) {
    rhs <- gsub(pattern = trans[i, "var"], replacement = trans[i, "vartrans"], x = rhs)
  }

  # calculate fitted values
  fitted <- data %>%
    pivot_wider(names_from = na_item, values_from = values) %>%
    mutate(!!dep.fitted := eval(parse(text = rhs)))

  return(fitted)

}
