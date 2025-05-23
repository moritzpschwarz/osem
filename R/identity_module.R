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

identity_module <- function(module, data, classification) {

  # original specification (identity/definition)
  rhs <- module$independent
  indep <- strsplits(module$independent, splits = c(" \\+ ", " \\- ", " / ", " \\* "))
  indep <- gsub(" ", "", indep)

  # new fitted value name
  dep <- module$dependent
  dep.fitted <- paste0(dep, ".level.hat")

  # check state of all that appear and translate to hat where necessary
  trans <- classification[(classification$var %in% indep), ] %>%
    dplyr::filter(.data$class %in% c("n", "d")) %>%
    dplyr::mutate(vartrans = paste0(.data$var, ".hat"))

  # translate
  if(nrow(trans) > 0){
    for (i in 1:NROW(trans)) {
      rhs <- gsub(pattern = paste0("\\b", trans[i, "var"], "\\b"), replacement = trans[i, "vartrans"], x = rhs)
    }
  }

  # calculate fitted values
  data %>%
    tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values") -> dat_tmp

  dat_tmp_names <- names(dat_tmp)

  dat_tmp %>%
    dplyr::mutate(!!dep.fitted := eval(parse(text = rhs))) %>%
    setNames(c(dat_tmp_names, dep.fitted)) -> fitted

  return(fitted)

}
