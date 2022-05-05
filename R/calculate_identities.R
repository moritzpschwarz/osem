#' Calculate values for identity/definition variables not available in Eurostat
#'
#' Calculates true values for variables that are given by identity or definition but are not directly available from Eurostat.
#'
#' @param specification A tibble or data.frame as returned by
#'   \code{\link{translate_variables}}.
#' @param data A tibble or data.frame containing the downloaded or locally
#'   loaded data as returned by \code{\link{load_or_download_variables}}.
#' @inheritParams translate_variables
#'
#' @return Returns the original tibble or data.frame \code{data} with added
#'   values for the identity/definition variables that were not available in
#'   Eurostat.
#'
#' @export

calculate_identities <- function(specification, data, dictionary = NULL) {

  # identity must be given as a module (i.e. must be a dependent variable)
  identities <- specification %>%
    filter(type == "d" & !(dependent_eu %in% dictionary$eurostat_code))

  # not sure whether can solve without dropping these vars
  # could add back later but not necessary?
  data <- data %>% select(-unit, -geo, -s_adj)

  for (i in 1:NROW(identities)) {
    identity <- identities[i, ]
    dep <- identity$dependent_eu
    indep <- identity$independent_eu
    data <- data %>%
      pivot_wider(names_from = na_item, values_from = values) %>%
      mutate(!!dep := eval(parse(text = indep))) %>%
      pivot_longer(cols = !time, names_to = "na_item", values_to = "values")
  }

  return(data)

}
